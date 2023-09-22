module Server (
  runMauMauServer,
) where

import MauMau qualified as MM
import MauMau.Constants qualified as Constants
import MauMau.State qualified as MMS
import Server.Message qualified as Msg
import Server.ResponseModels qualified as RM
import Server.State qualified as State

import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Data.Aeson (eitherDecode, encode)
import Data.Map qualified as Map
import Data.Maybe qualified as May
import Data.Text qualified as Text
import Network.WebSockets qualified as WS

runMauMauServer :: String -> Int -> IO ()
runMauMauServer ip port = do
  state <- STM.newTVarIO State.emptyState
  WS.runServer ip port $ withPingingConnection $ app state

withPingingConnection :: (WS.Connection -> IO ()) -> WS.ServerApp
withPingingConnection toRun pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) (toRun conn)

app :: TVar State.State -> WS.Connection -> IO ()
app stateVar conn = do
  userName <- acceptUserName stateVar conn
  let user = State.createUser userName conn
  STM.atomically $ STM.modifyTVar' stateVar (\s -> s{State.users = Map.insert userName user s.users})
  handleMessage userName conn stateVar
  putStrLn ("User with userName '" ++ show (State.userNameToText userName) ++ "' just ended their session.")

handleMessage :: State.UserName -> WS.Connection -> TVar State.State -> IO ()
handleMessage userName conn stateVar = do
  msg <- acceptValidMessage conn
  case msg of
    Msg.CreateGame -> do
      newGame <- State.createNewGame userName -- Will not be added to the state if conditions are not met, but we need to create it outside the STM atomic block
      (newState, audience, msgToSend) <- STM.atomically $ do
        state <- STM.readTVar stateVar
        case State.userGame userName state of
          Just game -> pure (state, SingleUser conn, Msg.CreateGameResponse . Left . Msg.AlreadyInGame . State.gameIdToText . State.getGameId $ game)
          Nothing -> do
            let gameId = State.getGameId newGame
                newState = state{State.games = Map.insert gameId newGame state.games, State.users = Map.adjust (State.addUserToGame gameId) userName state.users}

            STM.writeTVar stateVar newState
            pure (newState, All, Msg.CreateGameResponse . Right $ Msg.MkCreatedGameResponseParams{Msg.gameId = State.gameIdToText gameId, Msg.creatorName = State.userNameToText userName})

      sendMsgToAudience newState audience msgToSend
    Msg.StartGame -> do
      gameOrMsg <- STM.atomically $ do
        state <- STM.readTVar stateVar
        case State.userGame userName state of
          Nothing -> pure . Left . Msg.StartGameResponse . Left $ Msg.NotInAnyGame
          Just game
            | State.gameCreator game /= userName -> pure . Left . Msg.StartGameResponse . Left $ Msg.NotCreator
            | State.InProgress g <- game -> pure . Left . Msg.StartGameResponse . Left $ if g.status == State.Ended then Msg.AlreadyEnded else Msg.AlreadyRunning
            | otherwise -> pure $ Right game

      case gameOrMsg of
        Left msgToSend -> sendMsg conn msgToSend
        Right game -> do
          startedGameOrError <- State.startGame game
          case startedGameOrError of
            Left State.NotEnoughUsers -> sendMsg conn (Msg.StartGameResponse $ Left Msg.NotEnoughUsers)
            Left State.NotInStartingPhase -> sendMsg conn (Msg.StartGameResponse $ Left Msg.AlreadyRunning)
            Right (startedGame, gameState) -> do
              (newState, gameId) <- STM.atomically $ do
                state <- STM.readTVar stateVar
                let gameId = State.getGameId startedGame
                    newState = state{State.games = Map.insert gameId startedGame state.games}
                STM.writeTVar stateVar newState
                pure (newState, gameId)

              sendMsgToGame newState gameId $ Msg.StartGameResponse $ Right $ RM.fromAppModel gameState
    Msg.ConnectToGame rawGameId -> do
      (newState, audience, msgToSend) <- STM.atomically $ do
        state <- STM.readTVar stateVar
        case State.isGameId rawGameId state of
          Nothing -> pure (state, SingleUser conn, Msg.ConnectResponse . Left . Msg.GameIdNotFound $ rawGameId)
          Just (gameId, State.WaitingForPlayers game) ->
            case State.addUser userName game of
              Left _ -> pure (state, SingleUser conn, Msg.ConnectResponse $ Left $ Msg.TooManyPlayers Constants.maxUsersInGame)
              Right newGame -> do
                let newUsers = Map.adjust (State.addUserToGame gameId) userName state.users
                    newState = state{State.games = Map.insert gameId newGame state.games, State.users = newUsers}
                STM.writeTVar stateVar newState

                let response =
                      Msg.MkUserConnectedToGameResponse
                        { Msg.gameId = State.gameIdToText gameId
                        , Msg.userName = State.userNameToText userName
                        , Msg.usersInGame = map State.userNameToText $ State.gameUsers newGame
                        }
                pure (newState, Game gameId, Msg.ConnectResponse $ Right response)
          _ -> pure (state, SingleUser conn, Msg.ConnectResponse $ Left Msg.GameIsRunning)

      sendMsgToAudience newState audience msgToSend
    Msg.DisconnectFromGame -> do
      (newState, audience, msgToSend) <- STM.atomically $ do
        state <- STM.readTVar stateVar
        case State.removeUserFromGame userName state of
          Left State.NotInGame -> pure (state, SingleUser conn, Msg.DisconnectFromGameResponse . Left $ Msg.NoGameToDisconnectFrom)
          Left State.UserDoesNotExist -> error "Connected user is not in the state. This should never happen."
          Left State.UserInNonExistentGame -> error "User is in a game that does not exist. This should never happen."
          Right (newState, wasGameDeleted, gameId) -> do
            STM.writeTVar stateVar newState
            let response =
                  Msg.MkDisconnectFromGameResponse
                    { Msg.userName = State.userNameToText userName
                    , Msg.gameId = State.gameIdToText gameId
                    , Msg.gameStatusAfterDisconnect = if wasGameDeleted then Msg.Removed else Msg.Remains
                    }
            pure (newState, All, Msg.DisconnectFromGameResponse $ Right response)

      sendMsgToAudience newState audience msgToSend
    Msg.InGameMessage turn -> do
      (newState, audience, msgToSend) <- STM.atomically $ do
        state <- STM.readTVar stateVar
        case (State.userGame userName state, Msg.toAppTurn turn) of
          (Nothing, _) -> pure (state, SingleUser conn, Msg.TurnResult $ Left Msg.NotInGame)
          (_, Left err) -> pure (state, SingleUser conn, Msg.TurnResult $ Left err)
          (Just game, Right appTurn) -> do
            let gameStateOrError = case game of
                  State.WaitingForPlayers _ -> Left Msg.GameNotStarted
                  State.InProgress g
                    | g.status == State.Ended -> Left Msg.GameHasEnded
                    | g.gameState.nextPlayer.name /= State.userNameToText userName -> Left $ Msg.NotYourTurn g.gameState.nextPlayer.name
                    | otherwise -> Right g.gameState
            case flip MM.evaluateTurn appTurn <$> gameStateOrError of
              Left err -> pure (state, SingleUser conn, Msg.TurnResult $ Left err)
              Right (Left err) -> pure (state, SingleUser conn, Msg.TurnResult $ Left $ Msg.InvalidTurn $ RM.fromAppInvalidTurnReason err)
              Right (Right newGameState) -> do
                case State.updateGameState newGameState game of
                  Nothing -> error "Evaluated turn on a non-running game. This should not happen."
                  Just newGame -> do
                    let newState = state{State.games = Map.insert (State.getGameId game) newGame state.games}
                    STM.writeTVar stateVar newState
                    let response =
                          if State.hasEnded newGame
                            then Msg.GameEnded $ RM.fromAppModel newGameState
                            else Msg.TurnResult $ Right $ RM.fromAppModel newGameState
                    pure (newState, Game $ State.getGameId newGame, response)

      sendMsgToAudience newState audience msgToSend
    Msg.LogInAs _ -> sendMsg conn (Msg.LogInResponse $ Left Msg.AlreadyLoggedIn)

  handleMessage userName conn stateVar

acceptUserName :: TVar State.State -> WS.Connection -> IO State.UserName
acceptUserName stateVar conn = do
  msg <- acceptValidMessage conn
  case msg of
    Msg.LogInAs usrName ->
      case State.createUserName usrName of
        Left _ -> do
          sendMsg conn (Msg.LogInResponse $ Left Msg.InvalidUserName)
          acceptUserName stateVar conn
        Right x -> do
          state <- STM.readTVarIO stateVar
          if State.isTaken x state
            then sendMsg conn (Msg.LogInResponse $ Left Msg.UserNameTaken) >> acceptUserName stateVar conn
            else pure x
    _ -> do
      sendMsg conn Msg.SendUserNameFirst
      acceptUserName stateVar conn

acceptValidMessage :: WS.Connection -> IO Msg.RequestMessage
acceptValidMessage conn = do
  msg <- WS.receiveData conn
  case eitherDecode msg of
    Left err -> do
      sendMsg conn (Msg.InvalidMessage $ Text.pack err)
      acceptValidMessage conn
    Right x -> pure x

sendMsg :: WS.Connection -> Msg.ResponseMessage -> IO ()
sendMsg conn msg = WS.sendTextData conn (encode msg)

broadcast :: State.State -> Msg.ResponseMessage -> IO ()
broadcast state = sendMsgTo (Map.elems state.users)

sendMsgTo :: [State.User] -> Msg.ResponseMessage -> IO ()
sendMsgTo users msg = mapM_ (flip sendMsg msg . (.connection)) users

sendMsgToGame :: State.State -> State.GameId -> Msg.ResponseMessage -> IO ()
sendMsgToGame st gId = sendMsgTo (getGameUsers st gId)
 where
  getGame :: State.State -> State.GameId -> State.Game
  getGame state gameId = May.fromMaybe (error "Missing game for gameId.") $ Map.lookup gameId state.games

  getGameUsers :: State.State -> State.GameId -> [State.User]
  getGameUsers state gameId = May.mapMaybe (`Map.lookup` state.users) (State.gameUsers $ getGame state gameId)

data Audience
  = SingleUser WS.Connection
  | Game State.GameId
  | All

sendMsgToAudience :: State.State -> Audience -> Msg.ResponseMessage -> IO ()
sendMsgToAudience state audience msg = case audience of
  SingleUser conn -> sendMsg conn msg
  Game gameId -> sendMsgToGame state gameId msg
  All -> broadcast state msg

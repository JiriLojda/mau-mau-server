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
import Control.Monad (when)
import Data.Aeson (eitherDecode, encode)
import Data.Map qualified as Map
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
  handleMessage user stateVar
  putStrLn ("User with userName '" ++ show (State.userNameToText userName) ++ "' just ended their session.")

handleMessage :: State.User -> TVar State.State -> IO ()
handleMessage user stateVar = do
  msg <- acceptValidMessage user.connection
  shouldContinue <- case msg of
    Msg.CreateGame -> do
      newGame <- State.createNewGame user.userName -- Will not be added to the state if conditions are not met, but we need to create it outside the STM atomic block
      msgToSend <- STM.atomically $ do
        state <- STM.readTVar stateVar
        case State.userGame user.userName state of
          Just game -> pure . Msg.CreateGameResponse . Left . Msg.AlreadyInGame . State.gameIdToText . State.getGameId $ game
          Nothing -> do
            let gameId = State.getGameId newGame
                newState = state{State.games = Map.insert gameId newGame state.games, State.users = Map.adjust (State.addUserToGame gameId) user.userName state.users}

            STM.writeTVar stateVar newState
            pure . Msg.CreateGameResponse . Right . State.gameIdToText $ gameId

      sendMsg user.connection msgToSend
      pure True
    Msg.StartGame -> do
      gameOrMsg <- STM.atomically $ do
        state <- STM.readTVar stateVar
        case State.userGame user.userName state of
          Nothing -> pure . Left . Msg.StartGameResponse . Left $ Msg.NotInAnyGame
          Just game
            | State.gameCreator game /= user.userName -> pure . Left . Msg.StartGameResponse . Left $ Msg.NotCreator
            | State.InProgress g <- game -> pure . Left . Msg.StartGameResponse . Left $ if g.status == State.Ended then Msg.AlreadyEnded else Msg.AlreadyRunning
            | otherwise -> pure $ Right game

      case gameOrMsg of
        Left msgToSend -> sendMsg user.connection msgToSend >> pure True
        Right game -> do
          startedGameOrError <- State.startGame game
          case startedGameOrError of
            Left State.NotEnoughUsers -> sendMsg user.connection (Msg.StartGameResponse $ Left Msg.NotEnoughUsers) >> pure True
            Left State.NotInStartingPhase -> sendMsg user.connection (Msg.StartGameResponse $ Left Msg.AlreadyRunning) >> pure True -- This should not happen
            Right (startedGame, gameState) -> do
              STM.atomically $ do
                state <- STM.readTVar stateVar
                let newState = state{State.games = Map.insert (State.getGameId startedGame) startedGame state.games}
                STM.writeTVar stateVar newState
              sendMsg user.connection (Msg.StartGameResponse $ Right $ RM.fromAppModel gameState)
              pure True
    Msg.ConnectToGame rawGameId -> do
      msgToSend <- STM.atomically $ do
        state <- STM.readTVar stateVar
        case State.isGameId rawGameId state of
          Nothing -> pure . Msg.ConnectResponse . Left . Msg.GameIdNotFound $ rawGameId
          Just (gameId, State.WaitingForPlayers game) ->
            case State.addUser user.userName game of
              Left _ -> pure (Msg.ConnectResponse $ Left $ Msg.TooManyPlayers Constants.maxUsersInGame)
              Right newGame -> do
                let newUsers = Map.insert user.userName (State.addUserToGame gameId user) state.users
                    newState = state{State.games = Map.insert gameId newGame state.games, State.users = newUsers}
                STM.writeTVar stateVar newState
                pure (Msg.ConnectResponse $ Right $ map State.userNameToText $ State.gameUsers newGame)
          _ -> pure $ Msg.ConnectResponse $ Left Msg.GameIsRunning

      sendMsg user.connection msgToSend
      pure True
    Msg.DisconnectFromGame -> do
      msgToSend <- STM.atomically $ do
        state <- STM.readTVar stateVar
        case State.userGame user.userName state of
          Nothing -> pure . Msg.DisconnectFromGameResponse . Left $ Msg.NoGameToDisconnectFrom
          Just game ->
            case State.removeFromGame user game of
              Left State.NotInGame -> pure . Msg.DisconnectFromGameResponse . Left $ Msg.NoGameToDisconnectFrom
              Left State.IsLast -> do
                let newUser = user{State.inGame = Nothing}
                    newState = state{State.games = Map.delete (State.getGameId game) state.games, State.users = Map.insert user.userName newUser state.users}
                STM.writeTVar stateVar newState
                pure . Msg.DisconnectFromGameResponse . Right $ Msg.Removed
              Right (newGame, newUser) -> do
                let newState = state{State.games = Map.insert (State.getGameId newGame) newGame state.games, State.users = Map.insert user.userName newUser state.users}
                STM.writeTVar stateVar newState
                pure . Msg.DisconnectFromGameResponse . Right $ Msg.Remains

      sendMsg user.connection msgToSend
      pure True
    Msg.InGameMessage turn -> do
      msgToSend <- STM.atomically $ do
        state <- STM.readTVar stateVar
        case (State.userGame user.userName state, Msg.toAppTurn turn) of
          (Nothing, _) -> pure $ Msg.TurnResult $ Left Msg.NotInGame
          (_, Left err) -> pure $ Msg.TurnResult $ Left err
          (Just game, Right appTurn) -> do
            let gameStateOrError = case game of
                  State.WaitingForPlayers _ -> Left Msg.GameNotStarted
                  State.InProgress g
                    | g.status == State.Ended -> Left Msg.GameHasEnded
                    | g.gameState.nextPlayer.name /= State.userNameToText user.userName -> Left $ Msg.NotYourTurn g.gameState.nextPlayer.name
                    | otherwise -> Right g.gameState
            case flip MM.evaluateTurn appTurn <$> gameStateOrError of
              Left err -> pure $ Msg.TurnResult $ Left err
              Right (Left err) -> pure $ Msg.TurnResult $ Left $ Msg.InvalidTurn $ RM.fromAppInvalidTurnReason err
              Right (Right newGameState) -> do
                case State.updateGameState newGameState game of
                  Nothing -> error "Evaluated turn on a non-running game. This should not happen."
                  Just newGame -> do
                    STM.writeTVar stateVar state{State.games = Map.insert (State.getGameId game) newGame state.games}
                    pure (Msg.TurnResult $ Right $ RM.fromAppModel newGameState)

      sendMsg user.connection msgToSend
      pure True
    Msg.LogInAs _ -> sendMsg user.connection (Msg.LogInResponse $ Left Msg.AlreadyLoggedIn) >> pure True

  when shouldContinue $ handleMessage user stateVar

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

module Server.State (
  Game (..),
  StartingGame (..),
  GameInProgress (..),
  GameStatus (..),
  UserName (..),
  User (userName, connection, inGame),
  State (..),
  GameId,
  userGame,
  addUser,
  gameCreator,
  gameUsers,
  getGameId,
  createNewGame,
  startGame,
  CannotStartGameReason (..),
  updateGameState,
  removeUserFromGame,
  CannotRemoveReason (..),
  userNameToText,
  isGameId,
  gameIdToText,
  createUserName,
  emptyState,
  isInGame,
  isTaken,
  createUser,
  addUserToGame,
) where

import MauMau.State qualified as MMS

import Control.Monad (unless)
import Data.Char (isAlphaNum)
import Data.Either.Extra (maybeToEither)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe qualified as May
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import MauMau.Constants qualified as Constants
import Network.WebSockets qualified as WS

newtype GameId = MkGameId UUID deriving (Show, Eq, Ord)
newtype UserName = MkUserName Text deriving (Show, Eq, Ord)

data State = MkState
  { games :: Map GameId Game
  , users :: Map UserName User
  }
  deriving (Show)

emptyState :: State
emptyState = MkState{games = mempty, users = mempty}

data Game
  = WaitingForPlayers StartingGame
  | InProgress GameInProgress
  deriving (Show)

data StartingGame = MkStartingGame
  { id :: GameId
  , creator :: UserName
  , joinedUsers :: [UserName]
  }
  deriving (Show)

data GameInProgress = MkGameInProgress
  { id :: GameId
  , creator :: UserName
  , gameState :: MMS.GameState
  , disconnectedUsers :: [UserName]
  , status :: GameStatus
  }
  deriving (Show)

data GameStatus
  = Running
  | Ended
  | WaitingForDisconnected
  deriving (Show, Eq)

data User = MkUser
  { userName :: UserName
  , connection :: WS.Connection
  , inGame :: Maybe GameId
  }

instance Show User where
  show u = "MkUser {userName = " ++ show u.userName ++ ", inGame = " ++ show u.inGame ++ "}"

-- State

userGame :: UserName -> State -> Maybe Game
userGame userName state = do
  user <- Map.lookup userName state.users
  gameId <- user.inGame
  Map.lookup gameId state.games

-- Game

data CannotConnectToGameReason
  = TooManyPlayers

addUser :: UserName -> StartingGame -> Either CannotConnectToGameReason Game
addUser userName game
  | 1 + length game.joinedUsers >= Constants.maxUsersInGame = Left TooManyPlayers
  | otherwise = Right $ WaitingForPlayers $ game{joinedUsers = userName : game.joinedUsers}

gameUsers :: Game -> [UserName]
gameUsers (WaitingForPlayers game) = game.creator : game.joinedUsers
gameUsers (InProgress game) = map MkUserName $ MMS.players game.gameState

gameCreator :: Game -> UserName
gameCreator (WaitingForPlayers g) = g.creator
gameCreator (InProgress g) = g.creator

updateGameState :: MMS.GameState -> Game -> Maybe Game
updateGameState _ (WaitingForPlayers _) = Nothing
updateGameState _ (InProgress MkGameInProgress{status = Ended}) = Nothing
updateGameState gameState (InProgress game) = Just $ InProgress game{gameState}

getGameId :: Game -> GameId
getGameId (WaitingForPlayers g) = g.id
getGameId (InProgress g) = g.id

createNewGame :: UserName -> IO Game
createNewGame creator = do
  gameId <- generateGameId
  pure $ WaitingForPlayers $ MkStartingGame{id = gameId, creator, joinedUsers = []}

data CannotRemoveReason
  = NotInGame
  | UserDoesNotExist
  | UserInNonExistentGame

removeUserFromGame :: UserName -> State -> Either CannotRemoveReason (State, Bool)
removeUserFromGame userName state = do
  user <- maybeToEither UserDoesNotExist $ Map.lookup userName state.users
  gameId <- maybeToEither NotInGame user.inGame
  game <- maybeToEither UserInNonExistentGame $ Map.lookup gameId state.games

  newGame <- case game of
    WaitingForPlayers (MkStartingGame{joinedUsers = []}) -> pure Nothing
    WaitingForPlayers g@MkStartingGame{joinedUsers = nextUser : restUsers}
      | g.creator == user.userName -> pure $ Just $ WaitingForPlayers g{creator = nextUser, joinedUsers = restUsers}
      | otherwise -> pure $ Just $ WaitingForPlayers g{joinedUsers = filter (/= user.userName) g.joinedUsers}
    InProgress g
      | MMS.players g.gameState == [userNameToText userName] -> pure Nothing
      | otherwise -> pure $ Just $ InProgress g{disconnectedUsers = user.userName : g.disconnectedUsers, status = if g.status == Ended then Ended else WaitingForDisconnected}

  let newUser = user{inGame = Nothing}
      stateWithNewUser = state{users = Map.insert userName newUser state.users}
      newState = case newGame of
        Nothing -> stateWithNewUser{games = Map.delete gameId state.games}
        Just g -> stateWithNewUser{games = Map.insert gameId g state.games}

  pure (newState, May.isNothing newGame)

data CannotStartGameReason
  = NotEnoughUsers
  | NotInStartingPhase

startGame :: Game -> IO (Either CannotStartGameReason (Game, MMS.GameState))
startGame (InProgress _) = pure $ Left NotInStartingPhase
startGame (WaitingForPlayers (MkStartingGame{joinedUsers = []})) = pure $ Left NotEnoughUsers
startGame (WaitingForPlayers game@MkStartingGame{joinedUsers = nextUser : restUsers}) = do
  gameState <- MMS.generateNew (userNameToText game.creator, userNameToText nextUser) $ map userNameToText restUsers

  pure $ Right (InProgress MkGameInProgress{id = game.id, creator = game.creator, gameState, disconnectedUsers = [], status = Running}, gameState)

-- GameId

generateGameId :: IO GameId
generateGameId = MkGameId <$> UUID.nextRandom

isGameId :: Text -> State -> Maybe (GameId, Game)
isGameId rawGameId state = do
  gameId <- UUID.fromText rawGameId
  game <- Map.lookup (MkGameId gameId) state.games
  pure (MkGameId gameId, game)

gameIdToText :: GameId -> Text
gameIdToText (MkGameId uuid) = UUID.toText uuid

-- UserName

data InvalidUserNameReason
  = InvalidCharacter Char
  | TooShort Int
  | TooLong Int

createUserName :: Text -> Either InvalidUserNameReason UserName
createUserName = fmap MkUserName . validateUserName

validateUserName :: Text -> Either InvalidUserNameReason Text
validateUserName userName = do
  unless (Text.length userName >= Constants.minUserNameLength) (Left $ TooShort Constants.minUserNameLength)
  unless (Text.length userName <= Constants.maxUserNameLength) (Left $ TooLong Constants.maxUserNameLength)
  case Text.find (not . isAlphaNum) userName of
    Nothing -> pure ()
    Just x -> Left (InvalidCharacter x)

  pure userName

isTaken :: UserName -> State -> Bool
isTaken userName state = userName `elem` Map.keys state.users

isInGame :: UserName -> Game -> Bool
isInGame userName@(MkUserName rawUserName) g = case g of
  WaitingForPlayers game -> game.creator == userName || userName `elem` game.joinedUsers
  InProgress game -> rawUserName `isInGameState` game.gameState

isInGameState :: Text -> MMS.GameState -> Bool
isInGameState userName = elem userName . MMS.players

userNameToText :: UserName -> Text
userNameToText (MkUserName x) = x

-- User

createUser :: UserName -> WS.Connection -> User
createUser userName connection = MkUser{userName, connection, inGame = Nothing}

addUserToGame :: GameId -> User -> User
addUserToGame gameId user = user{inGame = Just gameId}

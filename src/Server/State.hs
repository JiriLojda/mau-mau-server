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
  removeFromGame,
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
import Data.Map qualified as Map
import Data.Map.Strict (Map)
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
gameUsers (InProgress game) = map MkUserName $ rawGameUsers game.gameState

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
  | IsLast

removeFromGame :: User -> Game -> Either CannotRemoveReason (Game, User)
removeFromGame user game = do
  unless (user.inGame == Just (getGameId game)) (Left NotInGame)
  let newUser = user{inGame = Nothing}

  newGame <- case game of
    WaitingForPlayers (MkStartingGame{joinedUsers = []}) -> Left IsLast
    WaitingForPlayers g@MkStartingGame{joinedUsers = nextUser : restUsers}
      | g.creator == user.userName -> Right $ WaitingForPlayers g{creator = nextUser, joinedUsers = restUsers}
      | otherwise -> Right $ WaitingForPlayers g{joinedUsers = filter (/= user.userName) g.joinedUsers}
    InProgress g -> Right $ InProgress g{disconnectedUsers = user.userName : g.disconnectedUsers, status = if g.status == Ended then Ended else WaitingForDisconnected}

  pure (newGame, newUser)

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

rawGameUsers :: MMS.GameState -> [Text]
rawGameUsers gameState = gameState.nextPlayer.name : map (.name) gameState.restPlayers

isInGameState :: Text -> MMS.GameState -> Bool
isInGameState userName = elem userName . rawGameUsers

userNameToText :: UserName -> Text
userNameToText (MkUserName x) = x

-- User

createUser :: UserName -> WS.Connection -> User
createUser userName connection = MkUser{userName, connection, inGame = Nothing}

addUserToGame :: GameId -> User -> User
addUserToGame gameId user = user{inGame = Just gameId}

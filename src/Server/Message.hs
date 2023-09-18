{-# LANGUAGE OverloadedStrings #-}

module Server.Message (
  GameRequestMessage (..),
  RequestMessage (..),
  ResponseMessage (..),
  LogInFailReason (..),
  ConnectFailReason (..),
  CreateGameError (..),
  TurnFailReason (..),
  DisconnectFromGameFailReason (..),
  GameStatusAfterDisconnect (..),
  StartGameErrorReason (..),
  toAppTurn,
) where

import Server.ResponseModels qualified as RM

import Data.Aeson (FromJSON (..), Key, Object, ToJSON (toJSON), Value, object, withObject, withText, (.:), (.=))
import Data.Aeson.KeyMap qualified as AesonMap
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import MauMau qualified as MM

-- Request models

data RequestMessage
  = InGameMessage GameRequestMessage
  | ConnectToGame Text
  | DisconnectFromGame
  | CreateGame
  | StartGame
  | LogInAs Text
  deriving (Show)

instance FromJSON RequestMessage where
  parseJSON = withObject "Message" $ \obj -> do
    messageTypeValue <- requireField "type" obj
    messageType <- parseJSON messageTypeValue
    case messageType of
      CreateGameType -> pure CreateGame
      StartGameType -> pure StartGame
      LogInType -> LogInAs <$> obj .: "userName"
      ConnectType -> ConnectToGame <$> obj .: "gameId"
      DisconnectType -> pure DisconnectFromGame
      SkipTurnType -> pure (InGameMessage SkipTurn)
      DrawCardsType -> pure (InGameMessage DrawCards)
      PlayCardType -> do
        rawCard <- requireField "card" obj
        card <- parseJSON rawCard
        case card of
          RM.MkCard{RM.cardType = RM.Queen} -> InGameMessage . PlayCard card <$> (obj .: "chosenSuite")
          _ -> pure $ InGameMessage (PlayCard card Nothing)

data RequestMessageType
  = LogInType
  | CreateGameType
  | StartGameType
  | ConnectType
  | DisconnectType
  | PlayCardType
  | SkipTurnType
  | DrawCardsType

instance FromJSON RequestMessageType where
  parseJSON = withText "Message type" $ \m ->
    case m of
      "logIn" -> pure LogInType
      "createGame" -> pure CreateGameType
      "startGame" -> pure StartGameType
      "connectToGame" -> pure ConnectType
      "disconnectFromGame" -> pure DisconnectType
      "playCard" -> pure PlayCardType
      "skipTurn" -> pure SkipTurnType
      "drawCards" -> pure DrawCardsType
      _ -> fail ("Message type '" ++ show m ++ "' is not a valid message type.")

-- GameRequestMessage (Turn action)

data GameRequestMessage
  = PlayCard RM.Card (Maybe RM.Suite)
  | SkipTurn
  | DrawCards
  deriving (Show)

toAppTurn :: GameRequestMessage -> Either TurnFailReason MM.Turn
toAppTurn SkipTurn = Right MM.Skip
toAppTurn DrawCards = Right MM.DrawCards
toAppTurn (PlayCard card@RM.MkCard{RM.cardType = RM.Queen} (Just suite)) = Right $ MM.PlayQueen (RM.cardToAppModel card) (RM.suiteToAppModel suite)
toAppTurn (PlayCard RM.MkCard{RM.cardType = RM.Queen} Nothing) = Left QueenWithoutSuite
toAppTurn (PlayCard _ (Just _)) = Left SuiteWithoutQueen
toAppTurn (PlayCard card Nothing) = Right $ MM.PlayCard (RM.cardToAppModel card)

-- Response models

data ResponseMessage
  = GameEnded RM.GameState
  | TurnResult (Either TurnFailReason RM.GameState)
  | ConnectResponse (Either ConnectFailReason [Text])
  | DisconnectFromGameResponse (Either DisconnectFromGameFailReason GameStatusAfterDisconnect)
  | CreateGameResponse (Either CreateGameError Text)
  | StartGameResponse (Either StartGameErrorReason RM.GameState)
  | LogInResponse (Either LogInFailReason [Text])
  | SendUserNameFirst
  | InvalidMessage Text

instance ToJSON ResponseMessage where
  toJSON (GameEnded s) = object ["type" .= ("gameEnded" :: Text), "state" .= toJSON s]
  toJSON (TurnResult (Left err)) = object ["type" .= ("turn" :: Text), "error" .= toJSON err]
  toJSON (TurnResult (Right state)) = object ["type" .= ("turn" :: Text), "success" .= toJSON True, "state" .= toJSON state]
  toJSON (ConnectResponse (Left err)) = object ["type" .= ("connectToGame" :: Text), "error" .= toJSON err]
  toJSON (ConnectResponse (Right users)) = object ["type" .= ("connectToGame" :: Text), "success" .= toJSON True, "users" .= toJSON users]
  toJSON (DisconnectFromGameResponse (Left err)) = object ["type" .= ("disconnectFromGame" :: Text), "error" .= toJSON err]
  toJSON (DisconnectFromGameResponse (Right gameStatus)) = object ["type" .= ("disconnectFromGame" :: Text), "success" .= toJSON True, "gameStatusAfterDisconnect" .= toJSON gameStatus]
  toJSON (CreateGameResponse (Left err)) = object ["type" .= ("gameCreation" :: Text), "error" .= toJSON err]
  toJSON (CreateGameResponse (Right gameId)) = object ["type" .= ("gameCreation" :: Text), "success" .= toJSON True, "gameId" .= toJSON gameId]
  toJSON (StartGameResponse (Left err)) = object ["type" .= ("startGame" :: Text), "error" .= toJSON err]
  toJSON (StartGameResponse (Right state)) = object ["type" .= ("startGame" :: Text), "success" .= True, "state" .= toJSON state]
  toJSON (LogInResponse (Left err)) = object ["type" .= ("logIn" :: Text), "error" .= toJSON err]
  toJSON (LogInResponse (Right gameIds)) = object ["type" .= ("logIn" :: Text), "success" .= toJSON True, "availableGameIds" .= toJSON gameIds]
  toJSON SendUserNameFirst = object ["type" .= ("handshake" :: Text), "error" .= ("Provide your userName before other interaction." :: Text)]
  toJSON (InvalidMessage err) = object ["type" .= ("error" :: Text), "error" .= ("You sent and invalid message. Error: " ++ show err)]

data StartGameErrorReason
  = NotCreator
  | AlreadyRunning
  | AlreadyEnded
  | NotInAnyGame
  | NotEnoughUsers

instance ToJSON StartGameErrorReason where
  toJSON NotCreator = "You are not the creator of this game."
  toJSON AlreadyRunning = "The game is already running."
  toJSON AlreadyEnded = "The game has alredy ended."
  toJSON NotInAnyGame = "You are not in any game."
  toJSON NotEnoughUsers = "There is not enough users to start the game."

newtype CreateGameError
  = AlreadyInGame Text

instance ToJSON CreateGameError where
  toJSON (AlreadyInGame gameId) = toJSON ("Cannot create a game when you already are in a game with gameId: '" ++ show gameId ++ "'.")

data TurnFailReason
  = InvalidTurn RM.InvalidTurnReason
  | NotYourTurn Text
  | NotInGame
  | GameNotStarted
  | GameHasEnded
  | QueenWithoutSuite
  | SuiteWithoutQueen

instance ToJSON TurnFailReason where
  toJSON (InvalidTurn reason) = toJSON reason
  toJSON (NotYourTurn currentUserName) = toJSON ("It is not your turn now. Currently waiting for turn of '" ++ show currentUserName ++ "'.")
  toJSON NotInGame = "You are not in any game. Connect to a game or create one first."
  toJSON GameNotStarted = "The game has not started yet. Wait for the game creator to start the game."
  toJSON GameHasEnded = "The game has already ended. Join another game first."
  toJSON QueenWithoutSuite = "You must choose a suite when you play the Queen card."
  toJSON SuiteWithoutQueen = "You can only choose a suite when playing the Queen card."

data DisconnectFromGameFailReason
  = NoGameToDisconnectFrom

instance ToJSON DisconnectFromGameFailReason where
  toJSON NoGameToDisconnectFrom = "You are not in any game."

data GameStatusAfterDisconnect
  = Remains
  | Removed

instance ToJSON GameStatusAfterDisconnect where
  toJSON Remains = "remains"
  toJSON Removed = "removed"

data ConnectFailReason
  = GameIdNotFound Text
  | TooManyPlayers Int
  | GameIsRunning

instance ToJSON ConnectFailReason where
  toJSON (GameIdNotFound gameId) = toJSON ("The game id '" ++ show gameId ++ "' doesn't exist.")
  toJSON (TooManyPlayers amount) = toJSON ("The game already has the maximum of " ++ show amount ++ " players.")
  toJSON GameIsRunning = "The game is already running and doesn't accept new players"

data LogInFailReason
  = UserNameTaken
  | InvalidUserName
  | AlreadyLoggedIn

instance ToJSON LogInFailReason where
  toJSON UserNameTaken = "This user name is already taken."
  toJSON InvalidUserName = "This user name is not valid."
  toJSON AlreadyLoggedIn = "You already are logged in."

-- Utils

requireField :: Key -> Object -> Parser Value
requireField key obj = case AesonMap.lookup key obj of
  Nothing -> fail ("Missing a required field '" ++ show key ++ "'.")
  Just v -> pure v

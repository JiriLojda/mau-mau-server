{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  CreatedGameResponseParams (..),
  UserConnectedToGameResponse (..),
  DisconnectFromGameResponse (..),
  toAppTurn,
) where

import Server.ResponseModels qualified as RM

import Data.Aeson (FromJSON (..), Key, Object, ToJSON (toJSON), Value, object, withObject, withText, (.:), (.=))
import Data.Aeson.KeyMap qualified as AesonMap
import Data.Aeson.Types (Parser)
import Data.Aeson.Types qualified as Aeson
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
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
  | ConnectResponse (Either ConnectFailReason UserConnectedToGameResponse)
  | DisconnectFromGameResponse (Either DisconnectFromGameFailReason DisconnectFromGameResponse)
  | CreateGameResponse (Either CreateGameError CreatedGameResponseParams)
  | StartGameResponse (Either StartGameErrorReason RM.GameState)
  | LogInResponse (Either LogInFailReason [Text])
  | SendUserNameFirst
  | InvalidMessage Text

instance ToJSON ResponseMessage where
  toJSON (GameEnded s) = successMsg "gameEnded" ["state" .= s]
  toJSON (TurnResult (Left err)) = errorMsg @"GTE" "turn" err
  toJSON (TurnResult (Right state)) = successMsg "turn" ["state" .= state]
  toJSON (ConnectResponse (Left err)) = errorMsg @"CTG" "connectToGame" err
  toJSON (ConnectResponse (Right params)) = successMsg "connectToGame" ["gameId" .= params.gameId, "userName" .= params.userName, "usersInGame" .= params.usersInGame]
  toJSON (DisconnectFromGameResponse (Left err)) = errorMsg @"DFG" "disconnectFromGame" err
  toJSON (DisconnectFromGameResponse (Right params)) = successMsg "disconnectFromGame" ["gameStatusAfterDisconnect" .= toJSON params.gameStatusAfterDisconnect, "userName" .= params.userName, "gameId" .= params.gameId]
  toJSON (CreateGameResponse (Left err)) = errorMsg @"CNG" "gameCreation" err
  toJSON (CreateGameResponse (Right params)) = successMsg "gameCreation" ["gameId" .= toJSON params.gameId, "creator" .= params.creatorName]
  toJSON (StartGameResponse (Left err)) = errorMsg @"SCG" "startGame" err
  toJSON (StartGameResponse (Right state)) = successMsg "startGame" ["state" .= toJSON state]
  toJSON (LogInResponse (Left err)) = errorMsg @"LIE" "logIn" err
  toJSON (LogInResponse (Right gameIds)) = successMsg "logIn" ["availableGameIds" .= toJSON gameIds]
  toJSON SendUserNameFirst = errorMsg @"MUN" "handshake" NoUserNameError
  toJSON (InvalidMessage err) = errorMsg @"GPE" "error" (ParseError err)

data CreatedGameResponseParams = MkCreatedGameResponseParams
  { gameId :: Text
  , creatorName :: Text
  }

data StartGameErrorReason
  = NotCreator
  | AlreadyRunning
  | AlreadyEnded
  | NotInAnyGame
  | NotEnoughUsers

data UserConnectedToGameResponse = MkUserConnectedToGameResponse
  { userName :: Text
  , gameId :: Text
  , usersInGame :: [Text]
  }

instance MauMauServerError "SCG" StartGameErrorReason where
  toErrorWithCode NotCreator = const (100, "You are not the creator of this game.")
  toErrorWithCode AlreadyRunning = const (200, "The game is already running.")
  toErrorWithCode AlreadyEnded = const (210, "The game has alredy ended.")
  toErrorWithCode NotInAnyGame = const (300, "You are not in any game.")
  toErrorWithCode NotEnoughUsers = const (400, "There is not enough users to start the game.")

newtype CreateGameError
  = AlreadyInGame Text

instance MauMauServerError "CNG" CreateGameError where
  toErrorWithCode (AlreadyInGame gameId) = const (100, Text.pack $ "Cannot create a game when you already are in a game with gameId: '" ++ show gameId ++ "'.")

data TurnFailReason
  = InvalidTurn RM.InvalidTurnReason
  | NotYourTurn Text
  | NotInGame
  | GameNotStarted
  | GameHasEnded
  | QueenWithoutSuite
  | SuiteWithoutQueen

instance MauMauServerError "GTE" TurnFailReason where
  toErrorWithCode (InvalidTurn reason) = const $ toErrorWithCode reason (Proxy @"CIT")
  toErrorWithCode (NotYourTurn currentUserName) = const (110, mconcat ["It is not your turn now. Currently waiting for turn of '", currentUserName, "'."])
  toErrorWithCode NotInGame = const (200, "You are not in any game. Connect to a game or create one first.")
  toErrorWithCode GameNotStarted = const (210, "The game has not started yet. Wait for the game creator to start the game.")
  toErrorWithCode GameHasEnded = const (220, "The game has already ended. Join another game first.")
  toErrorWithCode QueenWithoutSuite = const (300, "You must choose a suite when you play the Queen card.")
  toErrorWithCode SuiteWithoutQueen = const (310, "You can only choose a suite when playing the Queen card.")

instance MauMauServerError "CIT" RM.InvalidTurnReason where
  toErrorWithCode err@RM.OnlyAceOrSkip = const (100, Text.pack $ show err)
  toErrorWithCode err@RM.OnlySevenOrDraw = const (110, Text.pack $ show err)
  toErrorWithCode err@(RM.OnlyQueenOr _ _) = const (120, Text.pack $ show err)
  toErrorWithCode err@RM.SkipIsNotPossible = const (130, Text.pack $ show err)
  toErrorWithCode err@RM.GameHadEnded = const (200, Text.pack $ show err)
  toErrorWithCode err@(RM.InvalidInputState _) = const (300, Text.pack $ show err)
  toErrorWithCode err@RM.InvalidInput = const (310, Text.pack $ show err)
  toErrorWithCode err@RM.CanOnlyPlayCardsFromHand = const (400, Text.pack $ show err)

data DisconnectFromGameFailReason
  = NoGameToDisconnectFrom

instance MauMauServerError "DFG" DisconnectFromGameFailReason where
  toErrorWithCode NoGameToDisconnectFrom = const (100, "You are not in any game.")

data DisconnectFromGameResponse = MkDisconnectFromGameResponse
  { userName :: Text
  , gameId :: Text
  , gameStatusAfterDisconnect :: GameStatusAfterDisconnect
  }

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
  | AlreadyConnectedToAnotherGame Text
  | AlreadyConnected

instance MauMauServerError "CTG" ConnectFailReason where
  toErrorWithCode (GameIdNotFound gameId) = const (100, mconcat ["The game id '", gameId, "' doesn't exist."])
  toErrorWithCode (TooManyPlayers amount) = const (200, mconcat ["The game already has the maximum of ", Text.pack $ show amount, " players."])
  toErrorWithCode (AlreadyConnectedToAnotherGame gameId) = const (210, mconcat ["You are already connected to a game with id '", gameId, "'."])
  toErrorWithCode AlreadyConnected = const (220, "You are already connected to this game.")
  toErrorWithCode GameIsRunning = const (300, "The game is already running and doesn't accept new players")

data LogInFailReason
  = UserNameTaken
  | InvalidUserName
  | AlreadyLoggedIn

instance MauMauServerError "LIE" LogInFailReason where
  toErrorWithCode UserNameTaken = const (100, "This user name is already taken.")
  toErrorWithCode InvalidUserName = const (110, "This user name is not valid.")
  toErrorWithCode AlreadyLoggedIn = const (200, "You already are logged in.")

data NoUserNameError = NoUserNameError

instance MauMauServerError "MUN" NoUserNameError where
  toErrorWithCode _ = const (100, "Provide your userName before other interaction.")

newtype ParseError = ParseError Text

instance MauMauServerError "GPE" ParseError where
  toErrorWithCode (ParseError err) = const (100, Text.pack $ "You sent and invalid message. Error: " ++ show err)

-- Error codes

class (KnownSymbol prefix) => MauMauServerError (prefix :: Symbol) reason | prefix -> reason where
  toErrorWithCode :: reason -> Proxy prefix -> (Int, Text)
  toJSONError :: reason -> Proxy prefix -> Value
  toJSONError reason proxy = object ["code" .= mconcat [Text.pack $ symbolVal proxy, "-", Text.pack $ show code], "message" .= message]
   where
    (code, message) = toErrorWithCode reason proxy

-- Utils

requireField :: Key -> Object -> Parser Value
requireField key obj = case AesonMap.lookup key obj of
  Nothing -> fail ("Missing a required field '" ++ show key ++ "'.")
  Just v -> pure v

errorMsg :: forall errPrefix errReason. (MauMauServerError errPrefix errReason) => Text -> errReason -> Value
errorMsg context reason = object ["type" .= context, "error" .= toJSONError reason (Proxy @errPrefix)]

successMsg :: Text -> [Aeson.Pair] -> Value
successMsg context fields = object (["type" .= context, "success" .= True] ++ fields)

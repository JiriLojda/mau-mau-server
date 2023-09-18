{-# LANGUAGE DeriveAnyClass #-}

module Server.ResponseModels (
  -- GameState
  GameState,
  Card (..),
  Suite (..),
  CardType (..),
  fromAppModel,
  cardToAppModel,
  suiteToAppModel,
  -- Error reasons
  InvalidStateReason,
  InvalidTurnReason,
  fromAppInvalidStateReason,
  fromAppInvalidTurnReason,
) where

import MauMau qualified as MM
import MauMau.State qualified as MMS
import MauMau.StateValidation qualified as MMV

import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Text (Text)
import GHC.Generics (Generic)

-- GameState

data GameState = MkGameState
  { nextPlayer :: Player
  , restPlayers :: [Player]
  , topCard :: Card
  , discardPile :: [Card]
  , drawPile :: [Card]
  , topCardState :: TopCardState
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TopCardState
  = NoEffect
  | AceActive
  | SevenActive Int
  | QueenActive Suite
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Suite
  = Hearts
  | Diamonds
  | Spades
  | Clubs
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, FromJSON, ToJSON)

data CardType
  = Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, FromJSON, ToJSON)

data Card = MkCard
  { cardType :: CardType
  , suite :: Suite
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Player = MkPlayer
  { hand :: [Card]
  , name :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- GameState conversion from the App model

fromAppModel :: MMS.GameState -> GameState
fromAppModel appM =
  MkGameState
    { nextPlayer = mapPlayer appM.nextPlayer
    , restPlayers = map mapPlayer appM.restPlayers
    , topCard = mapCard appM.topCard
    , discardPile = map mapCard appM.discardPile
    , drawPile = map mapCard appM.drawPile
    , topCardState = mapTopCardState appM.topCardState
    }

mapPlayer :: MMS.Player -> Player
mapPlayer appP = MkPlayer{hand = map mapCard appP.hand, name = appP.name}

mapCard :: MMS.Card -> Card
mapCard appC = MkCard{cardType = mapCardType appC.cardType, suite = mapSuite appC.suite}

mapCardType :: MMS.CardType -> CardType
mapCardType MMS.Ace = Ace
mapCardType MMS.King = King
mapCardType MMS.Queen = Queen
mapCardType MMS.Jack = Jack
mapCardType MMS.Ten = Ten
mapCardType MMS.Nine = Nine
mapCardType MMS.Eight = Eight
mapCardType MMS.Seven = Seven

mapSuite :: MMS.Suite -> Suite
mapSuite MMS.Hearts = Hearts
mapSuite MMS.Diamonds = Diamonds
mapSuite MMS.Spades = Spades
mapSuite MMS.Clubs = Clubs

mapTopCardState :: MMS.TopCardState -> TopCardState
mapTopCardState MMS.NoEffect = NoEffect
mapTopCardState MMS.AceActive = AceActive
mapTopCardState (MMS.SevenActive num) = SevenActive num
mapTopCardState (MMS.QueenActive suite) = QueenActive (mapSuite suite)

-- GameState conversion to the App model

cardToAppModel :: Card -> MMS.Card
cardToAppModel card = MMS.MkCard{MMS.cardType = cardTypeToAppModel card.cardType, MMS.suite = suiteToAppModel card.suite}

suiteToAppModel :: Suite -> MMS.Suite
suiteToAppModel Hearts = MMS.Hearts
suiteToAppModel Diamonds = MMS.Diamonds
suiteToAppModel Spades = MMS.Spades
suiteToAppModel Clubs = MMS.Clubs

cardTypeToAppModel :: CardType -> MMS.CardType
cardTypeToAppModel Ace = MMS.Ace
cardTypeToAppModel King = MMS.King
cardTypeToAppModel Queen = MMS.Queen
cardTypeToAppModel Jack = MMS.Jack
cardTypeToAppModel Ten = MMS.Ten
cardTypeToAppModel Nine = MMS.Nine
cardTypeToAppModel Eight = MMS.Eight
cardTypeToAppModel Seven = MMS.Seven

-- Error reason

data InvalidTurnReason
  = OnlyAceOrSkip
  | OnlySevenOrDraw
  | OnlyQueenOr CardType Suite
  | GameHadEnded
  | SkipIsNotPossible
  | InvalidInputState InvalidStateReason
  | InvalidInput
  | CanOnlyPlayCardsFromHand

instance ToJSON InvalidTurnReason where
  toJSON OnlyAceOrSkip = toJSON "You can only play an ace or skip the turn."
  toJSON OnlySevenOrDraw = toJSON "You can only play a seven or draw cards."
  toJSON (OnlyQueenOr t s) = toJSON ("You can only play a Queen or a card of type " ++ show (toJSON t) ++ " or a card with suite " ++ show (toJSON s) ++ ".")
  toJSON GameHadEnded = toJSON "The game has already ended."
  toJSON SkipIsNotPossible = toJSON "It is currently not possible to skip."
  toJSON (InvalidInputState reason) = toJSON ("The game state is invalid, because: " ++ show (toJSON reason))
  toJSON InvalidInput = toJSON "The provided input is not valid."
  toJSON CanOnlyPlayCardsFromHand = toJSON "You can only play cards from your hand."

data InvalidStateReason
  = NonMatchingTopCardState
  | IncorrectAmountOfCardsInGame
  | IncorrectNumRequiredBySeven

instance ToJSON InvalidStateReason where
  toJSON NonMatchingTopCardState = toJSON "The top card state does not match the top card."
  toJSON IncorrectAmountOfCardsInGame = toJSON "The amount of cards in the game is incorrect."
  toJSON IncorrectNumRequiredBySeven = toJSON "The amount of cards required by the top card state for a card seven on the top is not correct."

-- Error reason conversion from App model

fromAppInvalidStateReason :: MMV.InvalidStateReason -> InvalidStateReason
fromAppInvalidStateReason MMV.NonMatchingTopCardState = NonMatchingTopCardState
fromAppInvalidStateReason MMV.IncorrectAmountOfCardsInGame = IncorrectAmountOfCardsInGame
fromAppInvalidStateReason MMV.IncorrectNumRequiredBySeven = IncorrectNumRequiredBySeven

fromAppInvalidTurnReason :: MM.InvalidTurnReason -> InvalidTurnReason
fromAppInvalidTurnReason MM.OnlyAceOrSkip = OnlyAceOrSkip
fromAppInvalidTurnReason MM.OnlySevenOrDraw = OnlySevenOrDraw
fromAppInvalidTurnReason (MM.OnlyQueenOr t s) = OnlyQueenOr (mapCardType t) (mapSuite s)
fromAppInvalidTurnReason MM.GameHadEnded = GameHadEnded
fromAppInvalidTurnReason MM.SkipIsNotPossible = SkipIsNotPossible
fromAppInvalidTurnReason (MM.InvalidInputState reason) = InvalidInputState (fromAppInvalidStateReason reason)
fromAppInvalidTurnReason MM.InvalidInput = InvalidInput
fromAppInvalidTurnReason MM.CanOnlyPlayCardsFromHand = CanOnlyPlayCardsFromHand
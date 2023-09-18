module MauMau.Cards (
  Suite (..),
  CardType (..),
  Card (..),
  all,
  allSuites,
  ace,
  king,
  queen,
  jack,
  ten,
  nine,
  eight,
  seven,
) where

import Prelude hiding (all)

data Suite
  = Hearts
  | Diamonds
  | Spades
  | Clubs
  deriving (Show, Eq, Ord, Enum, Bounded)

data CardType
  = Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  deriving (Show, Eq, Ord, Enum, Bounded)

data Card = MkCard
  { cardType :: CardType
  , suite :: Suite
  }
  deriving (Show, Eq, Ord)

all :: [Card]
all = [MkCard cType cSuite | cType <- enumFrom minBound, cSuite <- enumFrom minBound]

allSuites :: [Suite]
allSuites = enumFrom minBound

ace :: Suite -> Card
ace suite = MkCard{suite = suite, cardType = Ace}

king :: Suite -> Card
king suite = MkCard{suite = suite, cardType = King}

queen :: Suite -> Card
queen suite = MkCard{suite = suite, cardType = Queen}

jack :: Suite -> Card
jack suite = MkCard{suite = suite, cardType = Jack}

ten :: Suite -> Card
ten suite = MkCard{suite = suite, cardType = Ten}

nine :: Suite -> Card
nine suite = MkCard{suite = suite, cardType = Nine}

eight :: Suite -> Card
eight suite = MkCard{suite = suite, cardType = Eight}

seven :: Suite -> Card
seven suite = MkCard{suite = suite, cardType = Seven}

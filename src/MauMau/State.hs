module MauMau.State (
  Suite (..),
  CardType (..),
  Card (..),
  TopCardState (..),
  Player (..),
  GameState (..),
  cardsInGame,
  generateNew,
) where

import MauMau.Cards (Card, CardType, Suite)
import MauMau.Cards qualified as Cards
import MauMau.Constants qualified as Constants

import Data.Function ((&))
import Data.List qualified as List
import Data.Text (Text)
import System.Random qualified as Random

data GameState = MkGameState
  { nextPlayer :: Player
  , restPlayers :: [Player]
  , topCard :: Card
  , discardPile :: [Card]
  , drawPile :: [Card]
  , topCardState :: TopCardState
  }
  deriving (Show, Eq)

data TopCardState
  = NoEffect
  | AceActive
  | SevenActive Int
  | QueenActive Suite
  deriving (Show, Eq)

data Player = MkPlayer
  { hand :: [Card]
  , name :: Text
  }
  deriving (Show, Eq)

cardsInGame :: GameState -> [Card]
cardsInGame state = state.topCard : state.nextPlayer.hand ++ concatMap (.hand) state.restPlayers ++ state.discardPile ++ state.drawPile

generateNew :: (Text, Text) -> [Text] -> IO GameState
generateNew (currentPlayer, secondPlayer) otherPlayers = do
  randomGen <- Random.newStdGen
  let shuffledCards =
        zip (Random.randoms randomGen :: [Int]) Cards.all
          & List.sortOn fst
          & map snd

      (nextPlayer, restAfterFirstCards) = makePlayer currentPlayer shuffledCards
      (restPlayers, restCards) = List.foldl' (\(ps, cs) u -> mapFirst (: ps) $ makePlayer u cs) ([], restAfterFirstCards) (secondPlayer : otherPlayers)
      (topCard, drawPile) = case List.uncons restCards of
        Nothing -> error "There is not enough cards for all the players"
        Just res -> res

  pure $ MkGameState{nextPlayer, restPlayers, topCard, discardPile = [], drawPile, topCardState = NoEffect}

makePlayer :: Text -> [Card] -> (Player, [Card])
makePlayer name cards = (MkPlayer{name, hand}, rest)
 where
  hand = take Constants.startingCardsNumber cards
  rest = drop Constants.startingCardsNumber cards

mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst mapper (x, y) = (mapper x, y)

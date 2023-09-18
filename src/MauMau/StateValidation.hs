module MauMau.StateValidation (
  InvalidStateReason (..),
  validateState,
) where

import Control.Monad (unless)
import Data.List qualified as List
import MauMau.Cards qualified as Cards
import MauMau.State

data InvalidStateReason
  = NonMatchingTopCardState
  | IncorrectAmountOfCardsInGame
  | IncorrectNumRequiredBySeven
  deriving (Show, Eq)

validateState :: GameState -> Either InvalidStateReason GameState
validateState state = do
  unless (doesTopCardStateMatchTopCard state.topCard state.topCardState) (Left NonMatchingTopCardState)
  unless (areAllCardsInTheGame state) (Left IncorrectAmountOfCardsInGame)
  unless (doesSevenRequireCorrectNumber state) (Left IncorrectNumRequiredBySeven)

  pure state

doesTopCardStateMatchTopCard :: Card -> TopCardState -> Bool
doesTopCardStateMatchTopCard card AceActive = card.cardType == Ace
doesTopCardStateMatchTopCard card (SevenActive _) = card.cardType == Seven
doesTopCardStateMatchTopCard card (QueenActive _) = card.cardType == Queen
doesTopCardStateMatchTopCard card NoEffect = card.cardType /= Queen

areAllCardsInTheGame :: GameState -> Bool
areAllCardsInTheGame state = List.sort (cardsInGame state) == List.sort Cards.all

doesSevenRequireCorrectNumber :: GameState -> Bool
doesSevenRequireCorrectNumber state@MkGameState{topCardState = SevenActive num} = (1 + length (List.takeWhile ((== Seven) . (.cardType)) state.discardPile)) * 2 >= num
doesSevenRequireCorrectNumber _ = True

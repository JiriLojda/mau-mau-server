module MauMau (
  evaluateTurn,
  Turn (..),
  InvalidTurnReason (..),
) where

import MauMau.Constants as Const
import MauMau.State
import MauMau.StateValidation qualified as StateValidation
import Utils.List qualified as Utils

import Control.Monad (unless, when)
import Data.Function ((&))

data Turn
  = PlayCard Card
  | PlayQueen Card Suite
  | DrawCards
  | Skip
  deriving (Show, Eq)

data InvalidTurnReason
  = OnlyAceOrSkip
  | OnlySevenOrDraw
  | OnlyQueenOr CardType Suite
  | GameHadEnded
  | SkipIsNotPossible
  | InvalidInputState StateValidation.InvalidStateReason
  | InvalidInput
  | CanOnlyPlayCardsFromHand
  deriving (Show, Eq)

evaluateTurn :: GameState -> Turn -> Either InvalidTurnReason GameState
evaluateTurn state turn = do
  case StateValidation.validateState state of
    Left r -> Left $ InvalidInputState r
    Right _ -> pure ()

  when (playsQueenWithPlayCard turn || playsOtherCardWithPlayQueen turn) (Left InvalidInput)
  unless (hasCardInHand state turn) (Left CanOnlyPlayCardsFromHand)
  when (null state.restPlayers) (Left GameHadEnded)

  case state.topCard.cardType of
    Ace -> playOnAce state turn
    Seven -> playOnSeven state turn
    _ -> case turn of
      PlayCard card -> playStandardMove card Nothing state
      PlayQueen card suite -> playStandardMove card (Just suite) state
      DrawCards -> pure $ playDrawMove Const.numToDrawOnWait state
      Skip -> Left SkipIsNotPossible

playsQueenWithPlayCard :: Turn -> Bool
playsQueenWithPlayCard (PlayCard (MkCard{cardType = Queen})) = True
playsQueenWithPlayCard _ = False

playsOtherCardWithPlayQueen :: Turn -> Bool
playsOtherCardWithPlayQueen (PlayQueen card _) = card.cardType /= Queen
playsOtherCardWithPlayQueen _ = False

hasCardInHand :: GameState -> Turn -> Bool
hasCardInHand state (PlayCard c) = c `elem` state.nextPlayer.hand
hasCardInHand state (PlayQueen c _) = c `elem` state.nextPlayer.hand
hasCardInHand _ _ = True

playOnSeven :: GameState -> Turn -> Either InvalidTurnReason GameState
playOnSeven state turn = do
  when (state.topCard.cardType /= Seven) (error "Server error, called seven handler on non-seven card")

  case (turn, state.topCardState) of
    (DrawCards, SevenActive num) -> pure $ playDrawMove num $ state{topCardState = NoEffect}
    (DrawCards, NoEffect) -> pure $ playDrawMove Const.numToDrawOnWait state
    (PlayCard card, SevenActive _)
      | card.cardType /= Seven -> Left OnlySevenOrDraw
      | otherwise -> playStandardMove card Nothing state
    (PlayCard card, NoEffect) -> playStandardMove card Nothing state
    (PlayQueen _ _, SevenActive _) -> Left OnlySevenOrDraw
    (PlayQueen card suite, NoEffect) -> playStandardMove card (Just suite) state
    (Skip, SevenActive _) -> Left OnlySevenOrDraw
    (Skip, NoEffect) -> Left SkipIsNotPossible
    _ -> error "Top card state and top card are not matching. This should never happen."

playOnAce :: GameState -> Turn -> Either InvalidTurnReason GameState
playOnAce state turn = do
  when (state.topCard.cardType /= Ace) (error "Server error, called ace handler on non-ace card")

  case (turn, state.topCardState) of
    (Skip, AceActive) ->
      state{topCardState = NoEffect}
        & moveToNextPlayer
        & pure
    (Skip, NoEffect) -> Left SkipIsNotPossible
    (PlayCard card, AceActive)
      | card.cardType == Ace -> playStandardMove card Nothing state
      | otherwise -> Left OnlyAceOrSkip
    (PlayCard card, NoEffect) -> playStandardMove card Nothing state
    (PlayQueen _ _, AceActive) -> Left OnlyAceOrSkip
    (PlayQueen card suite, NoEffect) -> playStandardMove card (Just suite) state
    (DrawCards, AceActive) -> Left OnlyAceOrSkip
    (DrawCards, NoEffect) -> pure $ playDrawMove Const.numToDrawOnWait state
    _ -> error "Top card state and top card are not matching. This should never happen."

playStandardMove :: Card -> Maybe Suite -> GameState -> Either InvalidTurnReason GameState
playStandardMove card chosenSuite state = do
  case state.topCardState of
    QueenActive suite -> unless (card.suite == suite || card.cardType == Queen) (Left $ OnlyQueenOr Queen suite)
    NoEffect -> unless (card.suite == state.topCard.suite || card.cardType == state.topCard.cardType || card.cardType == Queen) (Left $ OnlyQueenOr state.topCard.cardType state.topCard.suite)
    _ -> pure ()

  state
    & playCard card chosenSuite
    & moveToNextPlayer
    & pure

playDrawMove :: Int -> GameState -> GameState
playDrawMove num state =
  state
    & drawCards num
    & moveToNextPlayer

moveToNextPlayer :: GameState -> GameState
moveToNextPlayer state = case Utils.head state.restPlayers of
  Nothing -> error "No rest players when evaluating turn, this should not happen."
  Just newNextPlayer -> state{nextPlayer = newNextPlayer, restPlayers = tail state.restPlayers ++ ([state.nextPlayer | not $ null state.nextPlayer.hand])}

playCard :: Card -> Maybe Suite -> GameState -> GameState
playCard card suiteChoice state =
  state
    { nextPlayer = state.nextPlayer{hand = filter (/= card) state.nextPlayer.hand}
    , topCard = card
    , discardPile = state.topCard : state.discardPile
    , topCardState = case (card.cardType, state.topCardState) of
        (Ace, _) -> AceActive
        (Seven, SevenActive num) -> SevenActive (num + Const.numToDrawOnSeven)
        (Seven, _) -> SevenActive Const.numToDrawOnSeven
        (Queen, _) -> case suiteChoice of
          Nothing -> error "When playing a queen a suite needs to be chosen, this should never happen."
          Just suite -> QueenActive suite
        (_, _) -> NoEffect
    }

drawCards :: Int -> GameState -> GameState
drawCards numCards state =
  preparedState
    { nextPlayer = preparedState.nextPlayer{hand = drawnCards ++ preparedState.nextPlayer.hand}
    , drawPile = drop numCards preparedState.drawPile
    }
 where
  preparedState = if length state.drawPile < numCards then turnPiles state else state
  drawnCards = take numCards preparedState.drawPile

turnPiles :: GameState -> GameState
turnPiles state = state{discardPile = [], drawPile = state.drawPile ++ reverse state.discardPile}

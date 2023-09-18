{-# LANGUAGE OverloadedStrings #-}

module MauMauSpec (spec) where

import MauMau qualified as MM
import MauMau.Cards qualified as Cards
import MauMau.State qualified as MMS
import MauMau.StateValidation qualified as SV
import Utils.List qualified as Utils

import Data.List qualified as List
import Data.Text qualified as Text
import Hedgehog (MonadGen, annotate, assert, cover, failure, forAll, success, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (HasCallStack)
import Test.Hspec qualified as HS
import Test.Hspec.Hedgehog (MonadTest, hedgehog, modifyMaxSuccess)

spec :: HS.Spec
spec = do
  HS.describe "evaluateTurn" $ do
    allCardsSpec
    aceSpec
    sevenSpec
    queenSpec
    validationSpec

allCardsSpec :: HS.Spec
allCardsSpec = HS.describe "all cards specs" $ do
  HS.it "Can always play card of the same type or suite on any inactive card apart from Queen" $ hedgehog $ do
    topCard <- forAll $ Gen.element $ filter (not . isType MMS.Queen) Cards.all
    hand <- forAll $ Gen.element $ filter (isType topCard.cardType `alternatively` isSuite topCard.suite `asWellAs` (/= topCard) `asWellAs` (not . isType MMS.Queen)) Cards.all
    initialState <- forAll $ gameStateGen $ MkGenParams{topCard = topCard, topCardState = MMS.NoEffect, currentPlayerHandCards = [hand]}

    cover 10 "Same suite" $ hand.suite == topCard.suite
    cover 10 "Same type" $ hand.cardType == topCard.cardType

    assertSuccesfulTurn $ MkAssertSuccesfulTurnParams{result = MM.evaluateTurn initialState (MM.PlayCard hand), initialState, playedCard = hand}

  HS.it "Reverts discard and turn it into draw when draw is empty" $ do
    let initialDiscardPile = filter (`notElem` [Cards.nine MMS.Clubs, Cards.eight MMS.Hearts, Cards.ten MMS.Hearts, Cards.ten MMS.Clubs]) Cards.all
    let initialState =
          MMS.MkGameState
            { MMS.nextPlayer = MMS.MkPlayer{MMS.name = "Current Player", MMS.hand = [Cards.nine MMS.Clubs]}
            , MMS.restPlayers = [MMS.MkPlayer{MMS.name = "Player 2", MMS.hand = [Cards.eight MMS.Hearts]}]
            , MMS.topCard = Cards.ten MMS.Hearts
            , MMS.discardPile = initialDiscardPile ++ [Cards.ten MMS.Clubs]
            , MMS.drawPile = []
            , MMS.topCardState = MMS.NoEffect
            }

    let result = MM.evaluateTurn initialState MM.DrawCards

    fmap (.nextPlayer.name) result `HS.shouldBe` Right "Player 2"
    fmap (Utils.head . (.restPlayers)) result `HS.shouldBe` Right (Just (MMS.MkPlayer{MMS.name = "Current Player", MMS.hand = [Cards.ten MMS.Clubs, Cards.nine MMS.Clubs]}))
    fmap (.topCard) result `HS.shouldBe` Right (Cards.ten MMS.Hearts)
    fmap (.discardPile) result `HS.shouldBe` Right []
    fmap (.drawPile) result `HS.shouldBe` Right (reverse initialDiscardPile)

  modifyMaxSuccess (const 1000) $ HS.it "Can never skip when top card is not an active ace" $ hedgehog $ do
    topCard <- forAll $ Gen.element Cards.all
    hand <- forAll $ Gen.element $ filter (/= topCard) Cards.all
    queenSuite <- forAll Gen.enumBounded
    topCardState <- forAll $ Gen.element $ case topCard.cardType of
      MMS.Ace -> [MMS.NoEffect]
      MMS.Seven -> [MMS.NoEffect, MMS.SevenActive 2]
      MMS.Queen -> [MMS.QueenActive queenSuite]
      _ -> [MMS.NoEffect]
    initialState <- forAll $ gameStateGen $ MkGenParams{topCard = topCard, topCardState, currentPlayerHandCards = [hand]}

    cover 1 "Inactive ace" (topCard.cardType == MMS.Ace)
    cover 1 "Active seven" (topCard.cardType == MMS.Seven && isActiveSeven topCardState)
    cover 1 "Inactive seven" (topCard.cardType == MMS.Seven && topCardState == MMS.NoEffect)
    cover 1 "Queen" (topCard.cardType == MMS.Queen)

    let result = MM.evaluateTurn initialState MM.Skip

    case result of
      Right _ -> annotate "Skipping when there is no active ace should fail." >> failure
      Left MM.SkipIsNotPossible -> success
      Left reason
        | topCard.cardType == MMS.Seven && isActiveSeven topCardState -> reason === MM.OnlySevenOrDraw
        | otherwise -> annotate ("Trying to skip when it is not possible gave an unexpected error reason: " ++ show reason) >> failure

  HS.it "Does not evaluate when the game has already ended" $ hedgehog $ do
    topCard <- forAll $ Gen.element Cards.all
    let topCardState = case topCard.cardType of
          MMS.Queen -> MMS.QueenActive MMS.Clubs
          _ -> MMS.NoEffect
    hand <- forAll $ Gen.element $ filter (/= topCard) Cards.all
    someSuite <- forAll $ Gen.element Cards.allSuites
    rawInitialState <- forAll $ gameStateGen $ MkGenParams{topCard, topCardState, currentPlayerHandCards = [hand]}

    let initialStateWithNoRestPlayers = rawInitialState{MMS.restPlayers = [], MMS.discardPile = concatMap (.hand) rawInitialState.restPlayers ++ rawInitialState.discardPile}
        turn = case hand.cardType of
          MMS.Queen -> (`MM.PlayQueen` someSuite)
          _ -> MM.PlayCard

        result = MM.evaluateTurn initialStateWithNoRestPlayers (turn hand)

    result === Left MM.GameHadEnded

aceSpec :: HS.Spec
aceSpec = HS.describe "ace specs" $ do
  HS.it "Cannot play anything else but ace when the top card is an active ace, the ace remains active" $ hedgehog $ do
    topCard <- forAll $ anySuiteGen Cards.ace
    nonAceHand <- forAll $ Gen.element (filter ((not . isType MMS.Ace) `asWellAs` (not . isType MMS.Queen)) Cards.all)
    aceHand <- forAll $ Gen.element $ filter (/= topCard) [Cards.ace s | s <- Cards.allSuites]
    hand <- forAll $ Gen.element (aceHand : [nonAceHand])
    initialState <- forAll $ gameStateGen $ MkGenParams{topCard = topCard, topCardState = MMS.AceActive, currentPlayerHandCards = [hand]}

    cover 30 "Can play with an ace" $ hand == aceHand
    cover 30 "Cannot play with no ace" $ hand == nonAceHand

    let result = MM.evaluateTurn initialState (MM.PlayCard hand)

    if hand == aceHand
      then do
        assertSuccesfulTurn $ MkAssertSuccesfulTurnParams{result, initialState, playedCard = hand}
        fmap (.topCardState) result === Right MMS.AceActive
      else result === Left MM.OnlyAceOrSkip

  HS.it "Can skip when top card is an active ace" $ hedgehog $ do
    topCard <- forAll $ anySuiteGen Cards.ace
    initialState <- forAll $ gameStateGen $ MkGenParams{topCard = topCard, topCardState = MMS.AceActive, currentPlayerHandCards = []}

    let result = MM.evaluateTurn initialState MM.Skip
    case result of
      Left reason -> annotate ("Failed to skip a turn for active ace, reason: " ++ show reason) >> failure
      Right newState -> do
        Just newState.nextPlayer === Utils.head initialState.restPlayers
        Just initialState.nextPlayer === Utils.last newState.restPlayers
        newState.topCardState === MMS.NoEffect

  HS.it "Can play any same-suite card on a non-active ace" $ hedgehog $ do
    topCard <- forAll $ anySuiteGen Cards.ace
    nonAceHand <- forAll $ Gen.element $ filter ((not . isType MMS.Ace) `asWellAs` isSuite topCard.suite `asWellAs` (not . isType MMS.Queen)) Cards.all
    initialState <- forAll $ gameStateGen $ MkGenParams{topCard = topCard, topCardState = MMS.NoEffect, currentPlayerHandCards = [nonAceHand]}

    assertSuccesfulTurn $ MkAssertSuccesfulTurnParams{result = MM.evaluateTurn initialState (MM.PlayCard nonAceHand), initialState, playedCard = nonAceHand}

sevenSpec :: HS.Spec
sevenSpec = HS.describe "seven card spec" $ do
  HS.it "Cannot play anything else but seven when the top card is an active seven, the seven remains active and requires 2 more cards" $ hedgehog $ do
    topCard <- forAll $ anySuiteGen Cards.seven
    nonSevenHand <- forAll $ Gen.element (filter ((not . isType MMS.Seven) `asWellAs` (not . isType MMS.Queen)) Cards.all)
    sevenHand <- forAll $ Gen.element $ filter (/= topCard) [Cards.seven s | s <- Cards.allSuites]
    hand <- forAll $ Gen.element (sevenHand : [nonSevenHand])
    prevRequiredCards <- forAll $ Gen.element [2, 4, 6]
    let neededSevens = take ((prevRequiredCards `div` 2) - 1) [seven | s <- Cards.allSuites, seven <- [Cards.seven s], seven `notElem` [topCard, hand]]
    generatedState <- forAll $ gameStateGen $ MkGenParams{topCard = topCard, topCardState = MMS.SevenActive prevRequiredCards, currentPlayerHandCards = hand : neededSevens}
    let initialState = addFakeSevens neededSevens generatedState

    cover 30 "Can play with a seven" $ hand == sevenHand
    cover 30 "Cannot play with no seven" $ hand == nonSevenHand

    let result = MM.evaluateTurn initialState (MM.PlayCard hand)

    if hand == sevenHand
      then do
        assertSuccesfulTurn $ MkAssertSuccesfulTurnParams{result, initialState, playedCard = hand}
        fmap (.topCardState) result === Right (MMS.SevenActive $ prevRequiredCards + 2)
      else result === Left MM.OnlySevenOrDraw

  HS.it "Can draw when top card is an active seven and draws appropriate amount of cards" $ hedgehog $ do
    topCard <- forAll $ anySuiteGen Cards.seven
    numToDraw <- forAll $ Gen.element [2, 4, 6, 8]
    let neededSevens = take ((numToDraw `div` 2) - 1) [seven | s <- Cards.allSuites, seven <- [Cards.seven s], seven /= topCard]
    let hasEnoughToDraw :: MMS.GameState -> Bool
        hasEnoughToDraw s@MMS.MkGameState{MMS.topCardState = MMS.SevenActive x} = length (cardsToDraw s) >= x
        hasEnoughToDraw _ = False

        topCardState = MMS.SevenActive numToDraw
    generatedState <- forAll $ Gen.filter hasEnoughToDraw $ gameStateGen $ MkGenParams{topCard, topCardState, currentPlayerHandCards = neededSevens}
    let initialState = addFakeSevens neededSevens generatedState

    let result = MM.evaluateTurn initialState MM.DrawCards
    case result of
      Left reason -> annotate ("Failed to draw cards for an active seven, reason: " ++ show reason) >> failure
      Right newState -> do
        Just newState.nextPlayer === Utils.head initialState.restPlayers
        Just initialState.nextPlayer.name === fmap (.name) (Utils.last newState.restPlayers)
        Just (numToDraw + length initialState.nextPlayer.hand) === fmap (length . (.hand)) (Utils.last newState.restPlayers)
        newState.topCardState === MMS.NoEffect

  HS.it "Can play any same-suite card on a non-active seven" $ hedgehog $ do
    topCard <- forAll $ anySuiteGen Cards.seven
    nonSevenHand <- forAll $ Gen.element $ filter ((not . isType MMS.Seven) `asWellAs` isSuite topCard.suite `asWellAs` (not . isType MMS.Queen)) Cards.all
    initialState <- forAll $ gameStateGen $ MkGenParams{topCard, topCardState = MMS.NoEffect, currentPlayerHandCards = [nonSevenHand]}

    let result = MM.evaluateTurn initialState (MM.PlayCard nonSevenHand)

    assertSuccesfulTurn $ MkAssertSuccesfulTurnParams{result, initialState, playedCard = nonSevenHand}

queenSpec :: HS.Spec
queenSpec = HS.describe "queen card spec" $ do
  HS.it "Can be played on any non-active card" $ hedgehog $ do
    topCard <- forAll $ Gen.element Cards.all
    hand <- forAll $ Gen.element $ filter (isType MMS.Queen `asWellAs` (/= topCard)) Cards.all
    someSuite <- forAll $ Gen.element Cards.allSuites
    suiteToChoose <- forAll $ Gen.element Cards.allSuites
    let topCardState = case topCard.cardType of
          MMS.Queen -> MMS.QueenActive someSuite
          _ -> MMS.NoEffect
    initialState <- forAll $ gameStateGen $ MkGenParams{topCard, topCardState, currentPlayerHandCards = [hand]}

    cover 10 "Plays on a card with a different suite" (topCard.suite /= hand.suite)
    cover 10 "Plays on a card with the same suite" (topCard.suite == hand.suite)

    let result = MM.evaluateTurn initialState (MM.PlayQueen hand suiteToChoose)

    assertSuccesfulTurn $ MkAssertSuccesfulTurnParams{result, initialState, playedCard = hand}

  HS.it "Only the chosen suite (excluding other queens) can be played on a queen" $ hedgehog $ do
    topCard <- forAll $ anySuiteGen Cards.queen
    suite <- forAll $ Gen.element Cards.allSuites
    hand <- forAll $ Gen.element $ filter (not . isType MMS.Queen) Cards.all
    initialState <- forAll $ gameStateGen $ MkGenParams{topCard, topCardState = MMS.QueenActive suite, currentPlayerHandCards = [hand]}

    cover 10 "Plays matching suite" (hand.suite == suite)
    cover 10 "Plays non-matching suite" (hand.suite /= suite)

    let result = MM.evaluateTurn initialState (MM.PlayCard hand)

    if suite == hand.suite
      then assertSuccesfulTurn $ MkAssertSuccesfulTurnParams{result, initialState, playedCard = hand}
      else result === Left (MM.OnlyQueenOr topCard.cardType suite)

validationSpec :: HS.Spec
validationSpec = HS.describe "validation spec" $ do
  HS.it "Fails when not all the cards are in the game" $ do
    let initialState = MMS.MkGameState
          { MMS.nextPlayer = MMS.MkPlayer{MMS.name = "CP", MMS.hand = [Cards.ace MMS.Clubs]}
          , MMS.restPlayers = [MMS.MkPlayer{MMS.name = "OP", MMS.hand = [Cards.nine MMS.Clubs]}]
          , MMS.topCard = Cards.nine MMS.Diamonds
          , MMS.discardPile = [Cards.ten MMS.Hearts]
          , MMS.drawPile = drop 1 $ filter (`notElem` [Cards.ace MMS.Clubs, Cards.nine MMS.Clubs, Cards.nine MMS.Diamonds, Cards.ten MMS.Hearts]) Cards.all
          , MMS.topCardState = MMS.NoEffect
          }

    let result = MM.evaluateTurn initialState MM.DrawCards

    result `HS.shouldBe` Left (MM.InvalidInputState SV.IncorrectAmountOfCardsInGame)

data GenParams = MkGenParams
  { topCard :: MMS.Card
  , topCardState :: MMS.TopCardState
  , currentPlayerHandCards :: [MMS.Card]
  }

gameStateGen :: (MonadGen m) => GenParams -> m MMS.GameState
gameStateGen params = do
  allCards <- filter (not . (`elem` params.topCard : params.currentPlayerHandCards)) <$> Gen.shuffle Cards.all
  numPlayers <- Gen.int (Range.linear 1 4)
  playerCardNums <- mapM (const $ Gen.int (Range.linear 1 5)) [1 .. numPlayers]
  currentPlayerNumCards <- Gen.int (Range.linear 1 $ max 1 (5 - length params.currentPlayerHandCards))
  let currentPlayerCards = params.currentPlayerHandCards ++ take currentPlayerNumCards allCards
      (playerCards, restCards) = List.foldl' (\(res, c) num -> (take num c : res, drop num c)) ([], drop currentPlayerNumCards allCards) playerCardNums
      players = zipWith (\cards i -> MMS.MkPlayer{MMS.hand = cards, MMS.name = Text.pack ("Player " ++ show i)}) playerCards [1 .. numPlayers]

  discardPileSize <- Gen.int $ Range.linear 0 $ length restCards
  let discardPile = take discardPileSize restCards
      drawPile = drop discardPileSize restCards

  pure
    $ MMS.MkGameState
      { MMS.nextPlayer = MMS.MkPlayer{MMS.hand = currentPlayerCards, MMS.name = Text.pack "Current Player"}
      , MMS.restPlayers = players
      , MMS.topCard = params.topCard
      , MMS.topCardState = params.topCardState
      , MMS.discardPile = discardPile
      , MMS.drawPile = drawPile
      }

anySuiteGen :: (MonadGen m) => (MMS.Suite -> MMS.Card) -> m MMS.Card
anySuiteGen mkCard = mkCard <$> Gen.enumBounded

isType :: MMS.CardType -> MMS.Card -> Bool
isType t card = t == card.cardType

isSuite :: MMS.Suite -> MMS.Card -> Bool
isSuite suite card = suite == card.suite

asWellAs :: (a -> Bool) -> (a -> Bool) -> a -> Bool
asWellAs pred1 pred2 input = pred1 input && pred2 input

alternatively :: (a -> Bool) -> (a -> Bool) -> a -> Bool
alternatively pred1 pred2 input = pred1 input || pred2 input

data AssertSuccesfulTurnParams = MkAssertSuccesfulTurnParams
  { initialState :: MMS.GameState
  , result :: Either MM.InvalidTurnReason MMS.GameState
  , playedCard :: MMS.Card
  }

assertSuccesfulTurn :: (MonadTest m, HasCallStack) => AssertSuccesfulTurnParams -> m ()
assertSuccesfulTurn MkAssertSuccesfulTurnParams{result = Left reason} = annotate ("The turn was not a success, reason: " ++ show reason) >> failure
assertSuccesfulTurn params@MkAssertSuccesfulTurnParams{result = Right newState} = do
  case params.playedCard.cardType of
    MMS.Queen -> pure ()
    _ -> newState.topCard === params.playedCard

  Just newState.nextPlayer === Utils.head params.initialState.restPlayers
  List.sort (MMS.cardsInGame newState) === List.sort Cards.all

  let playerThatJustPlayed = List.find ((== params.initialState.nextPlayer.name) . (.name)) newState.restPlayers
  if length params.initialState.nextPlayer.hand == 1
    then playerThatJustPlayed === Nothing
    else (List.find (== params.playedCard) . (.hand) <$> playerThatJustPlayed) === Just Nothing

  case params.playedCard.cardType of
    MMS.Ace -> newState.topCardState === MMS.AceActive
    MMS.Seven -> assert $ isActiveSeven newState.topCardState
    MMS.Queen -> assert $ isActiveQueen newState.topCardState
    _ -> newState.topCardState === MMS.NoEffect

isActiveSeven :: MMS.TopCardState -> Bool
isActiveSeven (MMS.SevenActive _) = True
isActiveSeven _ = False

isActiveQueen :: MMS.TopCardState -> Bool
isActiveQueen (MMS.QueenActive _) = True
isActiveQueen _ = False

cardsToDraw :: MMS.GameState -> [MMS.Card]
cardsToDraw s = s.drawPile ++ s.discardPile

addFakeSevens :: [MMS.Card] -> MMS.GameState -> MMS.GameState
addFakeSevens sevens state =
  state
    { MMS.nextPlayer = state.nextPlayer{MMS.hand = filter (`notElem` sevens) state.nextPlayer.hand}
    , MMS.discardPile = sevens ++ state.discardPile
    }

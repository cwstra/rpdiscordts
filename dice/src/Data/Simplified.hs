{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Simplified
  ( Dice (..),
    rollWrappedDice,
    getOrdered,
    addKeepDrop,
    Simplified (..),
    orderedDie,
    generalDie,
    getDice,
    getOrderedDice,
    addEqTest,
    addOrdTest,
    addIntervalTest,
    addNaturalTest,
    display,
  )
where

import Data.Dice (DicePool)
import qualified Data.Dice as D
import Data.KeepDrop
import Data.NumTest
import qualified Data.Text as T
import Data.UserNumber
import System.Random
import qualified Text.Show

data Dice = Ordered (DicePool GeneralRealNumber) | General (DicePool GeneralNumber)
  deriving (Eq)

instance Show Dice where
  show (Ordered d) = show d
  show (General d) = show d

rollWrappedDice :: RandomGen g => Dice -> g -> ([KeptOrDropped GeneralNumber], GeneralNumber, g)
rollWrappedDice (Ordered p) = D.rollDicePool p
rollWrappedDice (General p) = D.rollDicePool p

getOrdered :: Dice -> Either Text (DicePool GeneralRealNumber)
getOrdered (Ordered p) = Right p
getOrdered (General p) = D.limitGeneralPool p

data Simplified
  = Number GeneralNumber
  | Boolean Bool
  | Dice Dice
  | Vector [Simplified]
  deriving (Show, Eq)

orderedDie :: DicePool GeneralRealNumber -> Simplified
orderedDie = Dice . Ordered

generalDie :: DicePool GeneralNumber -> Simplified
generalDie = Dice . General

getDice :: Simplified -> Maybe Dice
getDice (Dice d) = Just d
getDice _ = Nothing

getOrderedDice :: Simplified -> Maybe (DicePool GeneralRealNumber)
getOrderedDice (Dice (Ordered d)) = Just d
getOrderedDice _ = Nothing

addKeepDrop :: DicePool GeneralRealNumber -> (forall a. Ord a => Int -> KeepDrop a) -> Int -> Simplified
addKeepDrop d kd n = Dice $ Ordered $ D.addKeepDrop d kd n

type AddDiceFn = (forall a. DicePool a -> NumTest a -> Either Text (DicePool a))

type PartialEqTest = (forall a. Eq a => NonEmpty a -> NumTest a)

type PartialOrdTest = (forall a. Ord a => a -> NumTest a)

type PartialIntervalTest = (forall a. Ord a => a -> a -> NumTest a)

addEqTest :: AddDiceFn -> Dice -> PartialEqTest -> NonEmpty GeneralNumber -> Either Text Simplified
addEqTest applyFn s@(Ordered d) tFn (n :| ns)
  | (w : ws) <- mapMaybe fromGeneralNumber (n : ns) = Dice . Ordered <$> applyFn d (tFn $ w :| ws)
  | otherwise = Right $ Dice s
addEqTest applyFn s@(General d) tFn ns = Dice . General <$> applyFn d (tFn ns)

addOrdTest :: AddDiceFn -> DicePool GeneralRealNumber -> PartialOrdTest -> GeneralRealNumber -> Either Text Simplified
addOrdTest applyFn d tFn n = Dice . Ordered <$> applyFn d (tFn n)

addIntervalTest :: AddDiceFn -> DicePool GeneralRealNumber -> PartialIntervalTest -> GeneralRealNumber -> GeneralRealNumber -> Either Text Simplified
addIntervalTest applyFn d tFn l g = Dice . Ordered <$> applyFn d (tFn l g)

addNaturalTest :: AddDiceFn -> DicePool GeneralRealNumber -> Either Text Simplified
addNaturalTest applyFn dice
  | Just face <- D.getSimpleFace dice = Dice . Ordered <$> applyFn dice (TestEq $ fromIntegral face :| [])
  | otherwise = Left "Rerolls and explosions must be specified for dice with listed faces"

display :: Simplified -> Text
display (Number n) = show n
display (Boolean b) = show b
display (Dice d) = show d
display (Vector rs) = "(" <> T.intercalate ", " (map display rs) <> ")"

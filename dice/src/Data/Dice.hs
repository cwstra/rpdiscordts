{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Dice
  ( Die,
    createSimpleDie,
    createGeneralDie,
    DicePool,
    createDicePool,
    getSimpleFace,
    addExplode,
    addReroll,
    addKeepDrop,
    addSuccess,
    rollDicePool,
    limitGeneralPool,
  )
where

import Data.KeepDrop
import Data.List (partition)
import qualified Data.List.NonEmpty as NE
import Data.NumTest
import Data.UserNumber
import System.Random.Stateful
import qualified Text.Show as Show

-- Base types / Creation

-- | One of three types of dice:
--   SimpleDie: A standard, n-sided die, where n is a Word64
--   OrderedDie: A die with a list of sides, where each side is a GeneralRealNumber
--   GeneralDie: A die with a list of sides, where each side is a GeneralNumber
data Die a where
  SimpleDie :: Word64 -> Die GeneralRealNumber
  OrderedDie :: NonEmpty GeneralRealNumber -> Die GeneralRealNumber
  GeneralDie :: NonEmpty GeneralNumber -> Die GeneralNumber

instance Eq (Die a) where
  (SimpleDie i1) == (SimpleDie i2) = i1 == i2
  (OrderedDie l1) == (OrderedDie l2) = l1 == l2
  (SimpleDie w) == (OrderedDie l) = 1 :| map fromIntegral [2 .. w] == NE.sort l
  o@(OrderedDie _) == w@(SimpleDie _) = w == o
  (GeneralDie l1) == (GeneralDie l2) = l1 == l2

instance Show (Die a) where
  show (SimpleDie i) = show i
  show (OrderedDie l) = show l
  show (GeneralDie l) = show l

-- | Ordered dice are internal, only created when we need ordering from a modifier,
--   so these helpers restrict the outside creation of dice to the Simple and General types.
createSimpleDie :: Word64 -> Die GeneralRealNumber
createSimpleDie = SimpleDie

createGeneralDie :: NonEmpty GeneralNumber -> Die GeneralNumber
createGeneralDie = GeneralDie

-- | A pool of Dice, along with any roll modifiers
data DicePool a = DicePool
  { poolSize :: Int,
    singleDie :: Die a,
    exploding :: NumTest a,
    reroll :: NumTest a,
    keep_drop :: KeepDrop a,
    success :: NumTest a
  }
  deriving (Eq)

createDicePool :: Int -> Die a -> DicePool a
createDicePool poolnum die =
  DicePool
    { poolSize = poolnum,
      singleDie = die,
      exploding = TestNone,
      reroll = TestNone,
      keep_drop = KeepAll,
      success = TestNone
    }

getSimpleFace :: DicePool GeneralRealNumber -> Maybe Word64
getSimpleFace DicePool {singleDie = (SimpleDie d)} = Just d
getSimpleFace _ = Nothing

-- Modifiers

getOverlap :: Die a -> NumTest a -> Maybe (NumTest a)
getOverlap _ TestNone = Just TestNone
getOverlap (SimpleDie w) (TestEq ts)
  | [] <- overlap = Just TestNone
  | fromIntegral (length overlap) == w = Nothing
  | (o : os) <- overlap = Just $ TestEq $ o :| os
  where
    overlap = NE.filter (\e -> 1 <= e && e <= fromIntegral w) ts
getOverlap (SimpleDie w) (TestNeq ts)
  | [] <- overlap = Nothing
  | fromIntegral (length overlap) == w = Just TestNone
  | (o : os) <- overlap = Just $ TestNeq $ o :| os
  where
    overlap = NE.filter (\e -> 1 <= e && e <= fromIntegral w) ts
getOverlap (SimpleDie w) test@(TestLeq t)
  | t >= fromIntegral w = Nothing
  | t < 1 = Just TestNone
  | otherwise = Just test
getOverlap (SimpleDie w) test@(TestLes t)
  | t > fromIntegral w = Nothing
  | t <= 1 = Just TestNone
  | otherwise = Just test
getOverlap (SimpleDie w) test@(TestGeq t)
  | t <= 1 = Nothing
  | t > fromIntegral w = Just TestNone
  | otherwise = Just test
getOverlap (SimpleDie w) test@(TestGre t)
  | t < 1 = Nothing
  | t >= fromIntegral w = Just TestNone
  | otherwise = Just test
getOverlap (SimpleDie w) (TestIn l g)
  | l <= 1 && fromW <= g = Nothing
  | l > fromW || 1 > g = Just TestNone
  | otherwise = Just $ TestIn l g
  where
    fromW = fromIntegral w
getOverlap (SimpleDie w) (TestOut l g)
  | l > fromW || 1 > g = Nothing
  | l <= 1 && fromW <= g = Just TestNone
  | otherwise = Just $ TestOut l g
  where
    fromW = fromIntegral w
getOverlap (OrderedDie os) (TestEq ts)
  | [] <- overlap = Just TestNone
  | length overlap == length os = Nothing
  | (o : os) <- overlap = Just $ TestEq $ o :| os
  where
    overlap = NE.filter (`elem` os) ts
getOverlap (OrderedDie os) (TestNeq ts)
  | [] <- overlap = Nothing
  | length overlap == length os = Just TestNone
  | (o : os) <- overlap = Just $ TestNeq $ o :| os
  where
    overlap = NE.filter (`elem` os) ts
getOverlap (OrderedDie os) test
  | [] <- overlap = Just TestNone
  | length overlap == length os = Nothing
  | otherwise = Just test
  where
    overlap = NE.filter (numTest test) os
getOverlap (GeneralDie gs) (TestEq ts)
  | [] <- overlap = Just TestNone
  | length overlap == length gs = Nothing
  | (o : os) <- overlap = Just $ TestEq $ o :| os
  where
    overlap = NE.filter (`elem` gs) ts
getOverlap (GeneralDie gs) (TestNeq ts)
  | [] <- overlap = Just TestNone
  | length overlap == length gs = Nothing
  | (o : os) <- overlap = Just $ TestNeq $ o :| os
  where
    overlap = NE.filter (`elem` gs) ts
getOverlap (GeneralDie gs) _ = Nothing

addExplode :: DicePool a -> NumTest a -> Either Text (DicePool a)
addExplode dicePool@DicePool {singleDie} numTest
  | Nothing <- overlap = Left "Infinitely exploding dice pool detected"
  | Just o <- overlap = Right $ dicePool {exploding = o}
  where
    overlap = getOverlap singleDie numTest

addReroll :: DicePool a -> NumTest a -> Either Text (DicePool a)
addReroll dicePool@DicePool {singleDie} numTest
  | Nothing <- overlap = Left "Infinitely rerolling dice pool detected"
  | Just o <- overlap = Right $ dicePool {reroll = o}
  where
    overlap = getOverlap singleDie numTest

addKeepDrop :: DicePool a -> (Int -> KeepDrop a) -> Int -> DicePool a
addKeepDrop dicePool kdt num = dicePool {keep_drop = kdt num}

addSuccess :: DicePool a -> NumTest a -> Either Text (DicePool a)
addSuccess dicePool numTest = Right $ dicePool {success = numTest}

limitGeneralPool :: DicePool GeneralNumber -> Either Text (DicePool GeneralRealNumber)
limitGeneralPool
  DicePool {poolSize, singleDie = (GeneralDie ns), exploding, reroll, success} = do
    newVals <-
      maybeToRight "Tried to coerce die with complex side to real die" $
        traverse fromGeneralNumber ns
    newExpl <- getExpl newVals
    newReroll <- getReroll newVals
    newSuccess <- getSuccess newVals
    return $ DicePool {poolSize, singleDie = OrderedDie newVals, exploding = newExpl, reroll = newReroll, keep_drop = KeepAll, success = newSuccess}
    where
      getReals :: NonEmpty GeneralNumber -> [GeneralRealNumber]
      getReals = mapMaybe fromGeneralNumber . NE.toList
      getExpl realValues
        | TestNone <- exploding = Right TestNone
        | TestEq gs <- exploding,
          [] <- getReals gs =
          Right TestNone
        | TestEq gs <- exploding,
          (r : rs) <- getReals gs,
          (r :| rs) /= realValues =
          Right $ TestEq (r :| rs)
        | TestNeq (g :| gs) <- exploding,
          (r : rs) <- mapMaybe fromGeneralNumber (g : gs),
          (r :| rs) == realValues =
          Right TestNone
        | TestNeq (g :| gs) <- exploding,
          (r : rs) <- mapMaybe fromGeneralNumber (g : gs) =
          Right $ TestNeq (r :| rs)
        | otherwise = Left "Infinitely exploding die detected"
      getReroll realValues
        | TestNone <- reroll = Right TestNone
        | TestNeq (g :| gs) <- reroll,
          (r : rs) <- mapMaybe fromGeneralNumber (g : gs) =
          Right $ TestNeq (r :| rs)
        | TestNeq _ <- reroll = Right TestNone
        | TestEq (g :| gs) <- reroll,
          (r : rs) <- mapMaybe fromGeneralNumber (g : gs),
          (r :| rs) /= realValues =
          Right $ TestEq (r :| rs)
        | otherwise = Left "Infinitely rerolling die detected"
      getSuccess realValues
        | TestNone <- reroll = Right TestNone
        | TestNeq (g :| gs) <- reroll,
          (r : rs) <- mapMaybe fromGeneralNumber (g : gs) =
          Right $ TestNeq (r :| rs)
        | TestNeq _ <- reroll = Right TestNone
        | TestEq (g :| gs) <- reroll,
          (r : rs) <- mapMaybe fromGeneralNumber (g : gs),
          (r :| rs) /= realValues =
          Right $ TestEq (r :| rs)
        | otherwise = Left "Infinitely rerolling die detected"

-- Rolling

pickFrom :: RandomGen g => NonEmpty a -> g -> (a, g)
pickFrom as = first (as NE.!!) . uniformR (0, l - 1)
  where
    l = length as

rollDie :: RandomGen g => Die a -> g -> (a, g)
rollDie (SimpleDie n) = first fromIntegral . uniformR (1, n)
rollDie (OrderedDie ls) = pickFrom ls
rollDie (GeneralDie ls) = pickFrom ls

rollManyDice :: RandomGen g => Int -> Die a -> g -> ([a], g)
rollManyDice n die g
  | n < 0 = ([], g)
  | otherwise = foldl' go ([], g) [1 .. n]
  where
    go (acc, g) 0 = (acc, g)
    go (acc, g) n = first (: acc) $ rollDie die g

--rollManyDice 0 die = ([],)

count :: (a -> Bool) -> [a] -> Int
count fn = foldl' (\s e -> s + if fn e then 1 else 0) 0

explodeResult :: RandomGen g => NumTest a -> Die a -> [a] -> g -> ([a], g)
explodeResult TestNone _ ls g = (ls, g)
explodeResult test die initial g = first reverse $ go (initial, g) (countMatches initial)
  where
    countMatches = count $ numTest test
    go (acc, g1) nextCount
      | [] <- nextSet = (acc, g1)
      | otherwise = go (nextSet ++ acc, g2) (countMatches nextSet)
      where
        (nextSet, g2) = rollManyDice nextCount die g1

rerollResult :: RandomGen g => NumTest a -> Die a -> [a] -> g -> ([KeptOrDropped a], g)
rerollResult TestNone _ ls g = (map Kept ls, g)
rerollResult test die initial g = first reverse $ go [] (splitter initial) g
  where
    splitter = partition (numTest test . snd) . zip [0 ..]
    merger k d = map snd $ sortBy (on compare fst) $ map (second Kept) k ++ map (second Dropped) d
    go acc ([], kept) g = (map (Kept . snd) kept ++ acc, g)
    go acc (dropped, kept) g1 = go (merger kept dropped ++ acc) (splitter nextSet) g2
      where
        (nextSet, g2) = rollManyDice (length dropped) die g1

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe mFn = foldr (\x -> maybe (second (x :)) (\y -> first (y :)) (mFn x)) ([], [])

keepDropResult :: KeepDrop a -> [KeptOrDropped a] -> [KeptOrDropped a]
keepDropResult KeepAll kds = kds
keepDropResult kd kds = newKeptDropped
  where
    (kept, dropped) = partitionMaybe (\(i, x) -> (i,) <$> keptToMaybe x) $ zip [(0 :: Int) ..] kds
    nextStep = uncurry zip $ second (keepDrop kd) $ unzip kept
    newKeptDropped = map snd $ sortBy (\(i1, _) (i2, _) -> compare i1 i2) $ nextStep ++ dropped

successResult :: ToGeneralNumber a => NumTest a -> [KeptOrDropped a] -> ([KeptOrDropped GeneralNumber], GeneralNumber)
successResult TestNone kds = (map (fmap toGeneralNumber) kds, foldl' sumH 0 kds)
  where
    sumH acc (Kept a) = acc + toGeneralNumber a
    sumH acc _ = acc
successResult test kds = (finalKds, foldl' countH 0 finalKds)
  where
    mapH (Kept a) = if numTest test a then Kept (toGeneralNumber a) else Dropped (toGeneralNumber a)
    mapH (Dropped a) = Dropped (toGeneralNumber a)
    finalKds = map mapH kds
    countH acc (Kept _) = acc + 1
    countH acc _ = acc

rollDicePool :: (ToGeneralNumber a, RandomGen g) => DicePool a -> g -> ([KeptOrDropped GeneralNumber], GeneralNumber, g)
rollDicePool pool g1 = (display, value, g4)
  where
    die = singleDie pool
    (initialResult, g2) = rollManyDice (poolSize pool) die g1
    (afterExplode, g3) = explodeResult (exploding pool) die initialResult g2
    (afterReload, g4) = rerollResult (reroll pool) die afterExplode g3
    afterKD = keepDropResult (keep_drop pool) afterReload
    (display, value) = successResult (success pool) afterKD

-- Display

showDicePoolSuffix :: ToGeneralNumber a => DicePool a -> String
showDicePoolSuffix
  DicePool
    { singleDie = die,
      exploding = explodingTest,
      reroll = rerollTest,
      keep_drop = kd,
      success = successTest
    } = suffix
    where
      nshow :: ToGeneralNumber a => a -> String
      nshow = show . toGeneralNumber
      showGenericTest :: ToGeneralNumber a => String -> NumTest a -> String
      showGenericTest pre TestNone = ""
      showGenericTest pre test@(TestEq _) = (if pre == "" then id else dropWhile (== '=')) $ showTest nshow test
      showGenericTest pre test = pre ++ showTest nshow test
      showExplodingTest :: ToGeneralNumber a => Die a -> NumTest a -> String
      showExplodingTest (SimpleDie w) t@(TestEq (a :| [])) = if fromIntegral w == a then "!" else showGenericTest "!" t
      showExplodingTest _ test = showGenericTest "!" test
      suffix =
        concat
          [ showExplodingTest die explodingTest,
            showGenericTest "r" rerollTest,
            showKeepDrop kd,
            showGenericTest "" successTest
          ]

instance ToGeneralNumber a => Show (DicePool a) where
  show d@DicePool {singleDie = die, poolSize = pool} = show pool ++ "d" ++ show die ++ showDicePoolSuffix d

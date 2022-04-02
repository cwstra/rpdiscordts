{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.NumTest
  ( NumTest (..),
    numTest,
    showTest,
    PartialNumTest (..),
    showPartialNumTest,
    applyPartial,
  )
where

import Text.Show

data NumTest a where
  TestNone :: NumTest a
  TestEq :: Eq a => NonEmpty a -> NumTest a
  TestNeq :: Eq a => NonEmpty a -> NumTest a
  TestLeq :: Ord a => a -> NumTest a
  TestLes :: Ord a => a -> NumTest a
  TestGeq :: Ord a => a -> NumTest a
  TestGre :: Ord a => a -> NumTest a
  TestIn :: Ord a => a -> a -> NumTest a
  TestOut :: Ord a => a -> a -> NumTest a

instance Eq (NumTest a) where
  TestNone == TestNone = True
  TestEq s1 == TestEq s2 = s1 == s2
  TestNeq s1 == TestNeq s2 = s1 == s2
  TestLeq n1 == TestLeq n2 = n1 == n2
  TestLes n1 == TestLes n2 = n1 == n2
  TestGeq n1 == TestGeq n2 = n1 == n2
  TestGre n1 == TestGre n2 = n1 == n2
  TestIn s1 e1 == TestIn s2 e2 = s1 == s2 && e1 == e2
  TestOut s1 e1 == TestOut s2 e2 = s1 == s2 && e1 == e2
  _ == _ = False

instance Show (NumTest a) where
  show TestNone = "TestNone"
  show (TestEq _) = "TestEq"
  show (TestNeq _) = "TestNeq"
  show (TestLeq _) = "TestLeq"
  show (TestLes _) = "TestLes"
  show (TestGeq _) = "TestGeq"
  show (TestGre _) = "TestGre"
  show (TestIn _ _) = "TestIn"
  show (TestOut _ _) = "TestOut"

numTest :: NumTest a -> a -> Bool
numTest TestNone _ = True
numTest (TestEq possibilities) a = a `elem` possibilities
numTest (TestNeq possibilities) a = a `notElem` possibilities
numTest (TestLeq fixed) a = fixed >= a
numTest (TestLes fixed) a = fixed > a
numTest (TestGeq fixed) a = fixed <= a
numTest (TestGre fixed) a = fixed < a
numTest (TestIn min max) a = min <= a && a <= max
numTest (TestOut min max) a = min > a || a > max

showListH :: (a -> String) -> NonEmpty a -> String
showListH fn (a :| []) = fn a
showListH fn (a :| rest) = "(" ++ intercalate ", " (map fn $ a : rest) ++ ")"

showTest :: (a -> String) -> NumTest a -> String
showTest _ TestNone = ""
showTest fn (TestEq ls) = "==" ++ showListH fn ls
showTest fn (TestNeq ls) = "/=" ++ showListH fn ls
showTest fn (TestLeq n) = "<=" ++ fn n
showTest fn (TestLes n) = "<" ++ fn n
showTest fn (TestGeq n) = ">=" ++ fn n
showTest fn (TestGre n) = ">" ++ fn n
showTest fn (TestIn m n) = "In(" ++ fn m ++ ", " ++ fn n ++ ")"
showTest fn (TestOut m n) = "Out(" ++ fn m ++ ", " ++ fn n ++ ")"

data PartialNumTest
  = EqTest (forall a. Eq a => NonEmpty a -> NumTest a)
  | OrdTest (forall a. Ord a => a -> NumTest a)
  | IntervalTest (forall a. Ord a => a -> a -> NumTest a)

applyPartial :: PartialNumTest -> NumTest Int
applyPartial (EqTest fn) = fn $ 0 :| []
applyPartial (OrdTest fn) = fn 0
applyPartial (IntervalTest fn) = fn 0 1

showPartialNumTest :: IsString s => Bool -> PartialNumTest -> s
showPartialNumTest showEq = go . applyPartial
  where
    go TestNone = ""
    go (TestEq ls) = if showEq then "==" else ""
    go (TestNeq ls) = "/="
    go (TestLeq n) = "<="
    go (TestLes n) = "<"
    go (TestGeq n) = ">="
    go (TestGre n) = ">"
    go (TestIn m n) = "In"
    go (TestOut m n) = "Out"

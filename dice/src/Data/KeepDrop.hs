{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.KeepDrop
  ( KeepDrop (..),
    keepDrop,
    showKeepDrop,
    showPartialKeepDrop,
    KeptOrDropped (..),
    keptToMaybe,
    showKeptOrDropped,
  )
where

import qualified Data.Text as T

data KeepDrop a where
  KeepAll :: KeepDrop a
  KeepHigh :: Ord a => Int -> KeepDrop a
  DropHigh :: Ord a => Int -> KeepDrop a
  KeepLow :: Ord a => Int -> KeepDrop a
  DropLow :: Ord a => Int -> KeepDrop a

instance Eq (KeepDrop a) where
  KeepAll == KeepAll = True
  (KeepHigh n) == (KeepHigh m) = n == m
  (DropHigh n) == (DropHigh m) = n == m
  (KeepLow n) == (KeepLow m) = n == m
  (DropLow n) == (DropLow m) = n == m
  _ == _ = False

showKeepDrop :: KeepDrop a -> String
showKeepDrop KeepAll = ""
showKeepDrop (KeepHigh n) = "kh" ++ show n
showKeepDrop (DropHigh n) = "dh" ++ show n
showKeepDrop (KeepLow n) = "kl" ++ show n
showKeepDrop (DropLow n) = "dl" ++ show n

showPartialKeepDrop :: IsString s => (forall a. Ord a => Int -> KeepDrop a) -> s
showPartialKeepDrop kdFn
  | KeepAll <- kd = ""
  | KeepHigh _ <- kd = "kh"
  | DropHigh _ <- kd = "dh"
  | KeepLow _ <- kd = "kl"
  | DropLow _ <- kd = "dl"
  where
    kd = kdFn 0 :: KeepDrop Int

data KeptOrDropped a = Kept a | Dropped a
  deriving (Eq, Show)

instance Functor KeptOrDropped where
  fmap fn (Kept a) = Kept (fn a)
  fmap fn (Dropped a) = Dropped (fn a)

instance Applicative KeptOrDropped where
  pure a = Kept a
  (Kept fn) <*> kd = fmap fn kd
  (Dropped fn) <*> (Kept a) = Dropped $ fn a
  (Dropped fn) <*> (Dropped a) = Dropped $ fn a

instance Monad KeptOrDropped where
  (Kept a) >>= fn = fn a
  (Dropped a) >>= fn = Dropped id <*> fn a

keptToMaybe :: KeptOrDropped a -> Maybe a
keptToMaybe (Kept a) = Just a
keptToMaybe _ = Nothing

showKeptOrDropped :: (a -> Text) -> KeptOrDropped a -> Text
showKeptOrDropped fn (Kept a) = fn a
showKeptOrDropped fn (Dropped a) = T.concat ["//", fn a, "//"]

keepDropH :: ([(Int, a)] -> ([(Int, a)], [(Int, a)])) -> (a -> a -> Ordering) -> [a] -> [KeptOrDropped a]
keepDropH splitFn sortFn = unsort . toKeptDropped . liftedSort
  where
    liftedSort = sortBy (on sortFn snd) . zip [0 ..]
    toKeptDropped = uncurry (++) . bimap (map $ second Kept) (map $ second Dropped) . splitFn
    unsort = map snd . sortBy (on compare fst)

keepDrop :: KeepDrop a -> [a] -> [KeptOrDropped a]
keepDrop KeepAll = map Kept
keepDrop (KeepHigh i) = keepDropH (splitAt i) (flip compare)
keepDrop (DropHigh i) = keepDropH (swap . splitAt i) (flip compare)
keepDrop (KeepLow i) = keepDropH (splitAt i) compare
keepDrop (DropLow i) = keepDropH (swap . splitAt i) compare

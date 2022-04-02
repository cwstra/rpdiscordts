{-# LANGUAGE OverloadedStrings #-}

module Data.Resolved
  ( Resolved (..),
    getNumber,
    getRealNumber,
    getInt,
    getPositiveInt,
    getNonEmptyVectorOf,
    getPairOf,
    display,
  )
where

import qualified Data.Text.Lazy as T
import Data.UserNumber

data Resolved = Number GeneralNumber | Boolean Bool | Vector [Resolved]
  deriving (Show, Eq)

getNumber :: Resolved -> Maybe GeneralNumber
getNumber (Number n) = Just n
getNumber _ = Nothing

getRealNumber :: Resolved -> Maybe GeneralRealNumber
getRealNumber (Number (GReal n)) = Just n
getRealNumber _ = Nothing

getInt :: Resolved -> Maybe Integer
getInt (Number (GReal (GSimp (GInt n)))) = Just n
getInt _ = Nothing

getPositiveInt :: Resolved -> Maybe Int
getPositiveInt s
  | (Number (GReal (GSimp (GInt n)))) <- s,
    0 < n,
    n < fromIntegral (maxBound :: Int) =
    Just $ fromInteger n
  | otherwise = Nothing

getNonEmptyVectorOf :: (Resolved -> Maybe a) -> Resolved -> Maybe (NonEmpty a)
getNonEmptyVectorOf getFn (Vector []) = Nothing
getNonEmptyVectorOf getFn (Vector (r : rs)) = traverse getFn $ r :| rs
getNonEmptyVectorOf getFn r = (:| []) <$> getFn r

getPairOf :: (Resolved -> Maybe a) -> Resolved -> Maybe (a, a)
getPairOf getFn (Vector [r1, r2]) = liftA2 (,) (getFn r1) (getFn r2)
getPairOf getFn _ = Nothing

mIntercalate :: Monoid m => m -> [m] -> m
mIntercalate _ [] = mempty
mIntercalate middle (l : ls) = mconcat $ l : concatMap (\e -> [middle, e]) ls

display :: (Monoid s, IsString s) => Resolved -> s
display (Number n) = show n
display (Boolean b) = show b
display (Vector rs) = "(" <> mIntercalate ", " (map display rs) <> ")"

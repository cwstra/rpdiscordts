module Data.UserNumberArb where

import Data.Ratio
import Data.UserNumber
import Test.QuickCheck

instance Arbitrary GeneralSimpleNumber where
  arbitrary = oneof [fmap GInt arbitrary, fmap GFlo arbitrary]
  shrink (GInt n) = map GInt $ shrink n
  shrink (GFlo n) = map GFlo $ shrink n

instance Arbitrary GRatio where
  arbitrary = fmap (\r -> GR (numerator r) (denominator r)) arbitrary
  shrink (GR n d) = fmap (\r -> GR (numerator r) (denominator r)) $ shrink $ n % d

instance Arbitrary GeneralRealNumber where
  arbitrary = oneof [fmap GSimp arbitrary, fmap GRat arbitrary]
  shrink (GSimp n) = map GSimp $ shrink n
  shrink (GRat n) = map GRat $ shrink n

instance Arbitrary GeneralComplex where
  arbitrary = liftA2 GC (oneof [return 0, arbitrary]) arbitrary
  shrink (GC 0 i) = GC 0 <$> shrink i
  shrink (GC r i) = zipWith GC (shrink r) (shrink i)

instance Arbitrary GeneralNumber where
  arbitrary = oneof [fmap GReal arbitrary, fmap GComp arbitrary]
  shrink (GReal n) = map GReal $ shrink n
  shrink (GComp n) = map GComp $ shrink n

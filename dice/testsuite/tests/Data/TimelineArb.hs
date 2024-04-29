module Data.TimelineArb where

import Data.Timeline (Timeline)
import qualified Data.Timeline as TL
import Data.Ratio
import Test.QuickCheck
import Arb

instance Arbitrary Timeline where
  arbitrary = do
    fst :| rst <- arbitrary
    return $ foldl' TL.add (TL.new fst) rst

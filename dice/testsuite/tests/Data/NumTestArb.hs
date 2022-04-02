module Data.NumTestArb where

import Data.NumTest
import Data.Ratio
import Test.QuickCheck

instance Arbitrary PartialNumTest where
  arbitrary =
    elements
      [ EqTest TestEq,
        EqTest TestNeq,
        OrdTest TestLeq,
        OrdTest TestLes,
        OrdTest TestGeq,
        OrdTest TestGre,
        IntervalTest TestIn,
        IntervalTest TestOut
      ]

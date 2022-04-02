module Arb (nonEmptyOf) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

nonEmptyOf :: Gen a -> Gen (NonEmpty a)
nonEmptyOf g = (\(a : as) -> a :| as) <$> listOf1 g

instance (Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary = nonEmptyOf arbitrary
  shrink (e :| es) = mapMaybe nonEmpty $ shrinkList shrink (e : es)

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  shrink e = map T.pack $ shrink $ T.unpack e

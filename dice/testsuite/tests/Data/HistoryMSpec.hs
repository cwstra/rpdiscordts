module Data.HistoryMSpec where

import Arb
import Data.HistoryM
import Data.Text (Text)
import qualified Data.Text as T
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- Not testing randomness at present, so this
-- helper can just use a static gen
getLines :: HistoryM StdGen a -> [(Text, Text)]
getLines = fst . snd . runHistory (mkStdGen 1)

spec :: Spec
spec = do
  describe "showReturn" $ do
    prop "generates a single entry" $ do
      \n -> length (getLines $ showReturn (n :: String)) `shouldBe` 1
    prop "duplicates text" $ do
      \n -> getLines (showReturn (n :: Text)) `shouldBe` [(T.pack $ show n, T.pack $ show n)]
  describe "withMap" $ do
    prop "properly handles prefixes" $ do
      \base prefix ->
        getLines (withMap (prefix <>) $ showReturn (base :: Text)) `shouldBe` [(prefix <> T.pack (show base), prefix <> T.pack (show base))]
    prop "properly handles postfixes" $ do
      \base postfix ->
        getLines (withMap (<> postfix) $ showReturn (base :: Text)) `shouldBe` [(T.pack (show base) <> postfix, T.pack (show base) <> postfix)]
  describe "withZip" $ do
    prop "properly handles infixes" $ do
      \baseL baseR mid ->
        let joiner = \a b -> a <> mid <> b
            leftSide =
              getLines
                ( withZip joiner $ do
                    t1 <- showReturn (baseL :: Text)
                    t2 <- showReturn (baseR :: Text)
                    return (t1, t2)
                )
            endText = T.pack (show baseL) `joiner` T.pack (show baseR)
            rightSide = [(endText, endText)]
         in leftSide `shouldBe` rightSide

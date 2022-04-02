{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MainSpec where

import Data.HistoryM
import Data.Resolved (Resolved)
import qualified Data.Resolved as Resolved
import System.Random
import Test.Hspec
import Text.DiceParser.AST

-- For non-random test cases
staticGen :: StdGen
staticGen = mkStdGen 1

staticRun :: ASTNode -> (Either Text Resolved, ([(Text, Text)], StdGen))
staticRun = runHistory staticGen . resolve

n = NumberNode

infixl 7 -*-

(-*-) = MultNode

infixl 6 -+-

(-+-) = AddNode

neg = NegativeNode . NumberNode

spec :: Spec
spec = do
  describe "static sanity tests" $ do
    it "pass" $ do
      staticRun (n 1) `shouldBe` (Right $ Resolved.Number 1, ([("1", "1")], staticGen))
      staticRun (n 1 -+- fmap n [2, 3]) `shouldBe` (Right $ Resolved.Number 6, ([("1+2+3", "6")], staticGen))
      staticRun (n 1 -+- [n 2]) `shouldBe` (Right $ Resolved.Number 3, ([("1+2", "3")], staticGen))
      staticRun (n 1 -+- [n 2, n 3]) `shouldBe` (Right $ Resolved.Number 6, ([("1+2+3", "6")], staticGen))
      staticRun (n 2 -*- [n 3]) `shouldBe` (Right $ Resolved.Number 6, ([("2*3", "6")], staticGen))
      staticRun (n 1 -*- fmap n [2, 3]) `shouldBe` (Right $ Resolved.Number 6, ([("1*2*3", "6")], staticGen))
      staticRun (n 1 -*- [n 2] -+- [n 3]) `shouldBe` (Right $ Resolved.Number 5, ([("1*2+3", "5")], staticGen))
      staticRun ((n 1 -+- [n 2]) -*- [n 3]) `shouldBe` (Right $ Resolved.Number 9, ([("(1+2)*3", "9")], staticGen))
      staticRun (n 1 -+- [neg 2, n 3]) `shouldBe` (Right $ Resolved.Number 2, ([("1-2+3", "2")], staticGen))
      staticRun (neg 2 -+- [neg 3]) `shouldBe` (Right $ Resolved.Number $ - 5, ([("-2-3", "-5")], staticGen))

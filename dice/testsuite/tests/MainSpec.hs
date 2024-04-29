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
staticRun = runHistory Nothing staticGen . resolve

n = NumberNode

infixl 7 -*-

(-*-) = MultNode

infixl 6 -+-

(-+-) = AddNode

neg = NegativeNode . NumberNode

d l r = DiceNode l $ NumberNode r

infixl 5 `d`

shouldHaveHistory result compare
  | (_, (h, _)) <- result = h `shouldBe` compare

spec :: Spec
spec = do
  describe "static sanity tests" $ do
    it "passes" $ do
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
  describe "dice sanity tests" $ do
    it "passes" $ do
      staticRun (n 1 `d` 6 `d` 6 `d` 6 `d` 6) `shouldHaveHistory`
        [("1d6d6d6d6", "1d6d6d6d6"),
         ("(6)d6d6d6", "6d6d6d6"),
         ("(5 + 4 + 4 + 5 + 3 + 4)d6d6", "25d6d6"),
         ("(5 + 2 + 2 + 4 + 1 + 6 + 2 + 1 + 4 + 1 + 4 + 3 + 6 + 5 + 4 + 6 + 5 + 6 + 1 + 3 + 3 + 2 + 4 + 3 + 3)d6", "86d6"),
         ("(2 + 1 + 5 + 4 + 1 + 3 + 5 + 3 + 5 + 2 + 5 + 6 + 2 + 5 + 3 + 3 + 2 + 6 + 4 + 3 + 2 + 2 + 3 + 4 + 4 + 2 + 3 + 3 + 1 + 1 + 4 + 3 + 1 + 6 + 5 + 6 + 4 + 3 + 3 + 6 + 3 + 2 + 3 + 2 + 4 + 4 + 5 + 3 + 4 + 2 + 2 + 3 + 1 + 4 + 5 + 3 + 2 + 4 + 3 + 3 + 6 + 2 + 5 + 1 + 2 + 1 + 6 + 5 + 6 + 2 + 4 + 6 + 6 + 4 + 2 + 1 + 3 + 1 + 3 + 1 + 6 + 5 + 3 + 6 + 2 + 2)", "289")
        ]

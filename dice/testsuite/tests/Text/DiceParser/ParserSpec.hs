{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.DiceParser.ParserSpec where

import Data.Attoparsec.Text
import Data.NumTest
import qualified Data.Text as T
import Data.UserNumber
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.DiceParser.AST
import Text.DiceParser.ASTArb
import Text.DiceParser.Parser

newtype ParsableNumber = ParsableNumber GeneralNumber
  deriving (Show)

instance Arbitrary ParsableNumber where
  arbitrary = do
    simp <- oneof [fmap GInt arbitrarySizedNatural, fmap (GFlo . abs) $ suchThat arbitrary $ T.all (/= 'e') . show]
    wrapper <- oneof [return GReal, return $ GComp . GC 0]
    return $ ParsableNumber $ wrapper $ GSimp simp
  shrink (ParsableNumber n)
    | GReal (GSimp (GInt i)) <- n = map (ParsableNumber . GReal . GSimp . GInt) $ filter (> 0) $ shrink i
    | GReal (GSimp (GFlo f)) <- n = map (ParsableNumber . GReal . GSimp . GFlo) $ filter (> 0) $ shrink f
    | GComp (GC _ (GSimp (GInt i))) <- n = map (ParsableNumber . GComp . GC 0 . GSimp . GInt) $ filter (> 0) $ shrink i
    | GComp (GC _ (GSimp (GFlo f))) <- n = map (ParsableNumber . GComp . GC 0 . GSimp . GFlo) $ filter (> 0) $ shrink f
    | otherwise = []

toTup (Done i n) = Right (i, n)
toTup (Fail i _ e) = Left (Just e)
toTup (Partial i) = toTup (i "")

tripleTest str val = do
  second applyPartial <$> toTup (parse (partialNumTest Nothing) str) `shouldBe` Right ("", val)
  second applyPartial <$> toTup (parse (partialNumTest $ Just 'r') $ T.cons 'r' str) `shouldBe` Right ("", val)
  second applyPartial <$> toTup (parse (partialNumTest $ Just '!') $ T.cons '!' str) `shouldBe` Right ("", val)

spec :: Spec
spec = do
  describe "numberLiteral" $ do
    prop "parses numbers properly" $
      \(ParsableNumber n) ->
        toTup (parse numberLiteral $ show n) `shouldBe` Right ("", n)
  describe "booleanLiteral" $ do
    it "parses bools properly" $ do
      toTup (parse booleanLiteral "True") `shouldBe` Right ("", True)
      toTup (parse booleanLiteral "true") `shouldBe` Right ("", True)
      toTup (parse booleanLiteral "False") `shouldBe` Right ("", False)
      toTup (parse booleanLiteral "false") `shouldBe` Right ("", False)
  describe "partialNumTest" $ do
    it "parses numTests properly" $ do
      second applyPartial <$> toTup (parse (partialNumTest Nothing) "==") `shouldBe` Right ("", TestEq [0])
      second applyPartial <$> toTup (parse (partialNumTest $ Just 'r') "r") `shouldBe` Right ("", TestEq [0])
      second applyPartial <$> toTup (parse (partialNumTest $ Just '!') "!") `shouldBe` Right ("", TestEq [0])
      tripleTest "/=" $ TestNeq [0]
      tripleTest "<=" $ TestLeq 0
      tripleTest "<" $ TestLes 0
      tripleTest ">=" $ TestGeq 0
      tripleTest ">" $ TestGre 0
      tripleTest "In" $ TestIn 0 1
      tripleTest "Out" $ TestOut 0 1

{-
-- Need to get a better test. Too many edge cases, like complex numbers
describe "expr" $ do
  prop "is the inverse of printStage" $ do
    \(RandomNode n) ->
      let initialStr = printStage n
       in printStage <$> runExprParser initialStr `shouldBe` Right initialStr
-}

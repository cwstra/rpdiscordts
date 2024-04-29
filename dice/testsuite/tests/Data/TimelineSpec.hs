{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TimelineSpec where

import qualified Data.Text as T
import Test.Hspec.QuickCheck
import Data.TimelineArb
import System.Random
import Test.Hspec
import qualified Data.Timeline as TL
import qualified Data.List.NonEmpty as NE

tlLength = length . TL.getForwardHistory

(-+-) = TL.add
infixl 4 -+-
(-=-) tup nxt = TL.new tup -+- nxt
infixl 4 -=-

spec :: Spec
spec = do
  describe "resolveJoin" $ do
    prop "maintains length - 1" $ do
      \tl1 tl2 ->
         tlLength (TL.resolveJoin $ tl2 :| [tl1])
         `shouldBe`
         tlLength tl1 + tlLength tl2 - 1
  describe "zip" $ do
    prop "maintains the larger of the two lengths" $ do
      \tl1 tl2 ->
         tlLength (TL.zip T.append tl1 tl2)
         `shouldBe`
         max (tlLength tl1) (tlLength tl2)

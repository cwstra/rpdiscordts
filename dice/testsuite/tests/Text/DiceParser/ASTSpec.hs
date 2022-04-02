{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.DiceParser.ASTSpec where

import Arb
import Data.Dice
import Data.HistoryM
import Data.KeepDrop
import Data.NumTest
import Data.Resolved (Resolved)
import qualified Data.Resolved as Resolved
import Data.Simplified (Simplified)
import qualified Data.Simplified as Simplified
import Data.UserNumber
import Data.UserNumberArb
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.DiceParser.AST
import Text.DiceParser.ASTArb

-- print stage helpers

d x n = DiceNode (NumberNode x) (NumberNode n)

makePrintWrapper :: Precedence -> Maybe PrecDirection -> ASTNode -> String
makePrintWrapper parentPrec side child
  | precGreater side parentPrec (precedence child) = enclose "(" ")" $ printStage child
  | otherwise = printStage child

neIntercalate :: String -> NonEmpty String -> String
neIntercalate sep (s :| ss) = s ++ mconcat (map (sep <>) ss)

-- resolve helpers

-- For non-random test cases
staticGen :: StdGen
staticGen = mkStdGen 1

staticResolve :: ASTNode -> (Either Text Resolved, ([(Text, Text)], StdGen))
staticResolve = runHistory staticGen . resolve

staticSimplify :: ASTNode -> (Either Text Simplified, ([(Text, Text)], StdGen))
staticSimplify = runHistory staticGen . simplify

n = NumberNode

infixl 7 -*-

(-*-) = MultNode

infixl 6 -+-

(-+-) = AddNode

neg = NegativeNode . NumberNode

spec :: Spec
spec = do
  describe "printStage" $ do
    -- TODO Merge tests w/ similar generators to speed up tests
    prop "displays numbers properly" $
      \n -> printStage (NumberNode n) `shouldBe` (numShow n :: String)
    it "displays bools properly" $ do
      printStage (BooleanNode True) `shouldBe` "True"
      printStage (BooleanNode False) `shouldBe` "False"
    context "when dealing with dice" $ do
      prop "displays simple dice properly" $
        \(n1, n2) -> printStage (n1 `d` n2) `shouldBe` numShow n1 ++ "d" ++ numShow n2
      prop "displays vector dice properly" $
        \(n, e1, v) ->
          printStage (DiceNode (NumberNode n) (VectorNode (NumberNode e1) $ fmap NumberNode v))
            `shouldBe` numShow n ++ "d(" ++ foldl' (\a e -> a <> ", " <> numShow e) (numShow e1) v ++ ")"
      prop "displays arbitrary dice nodes properly" $
        \(RandomNode n1, RandomNode n2) ->
          let dice = DiceNode n1 n2
              mid = makePrintWrapper $ precedence dice
              s1 = mid (Just PrecLeft) n1
              s2 = mid (Just PrecRight) n2
           in printStage dice `shouldBe` s1 ++ "d" ++ s2
    context "when dealing with fudge dice" $ do
      prop "displays plain fudge dice properly" $ do
        \n -> printStage (FudgeNode $ NumberNode n) `shouldBe` numShow n ++ "dF"
      prop "displays arbitrary fudge dice properly" $ do
        \(RandomNode n) ->
          let fudge = FudgeNode n
              s = makePrintWrapper (precedence fudge) Nothing n
           in printStage fudge `shouldBe` s ++ "dF"
    context "when dealing with keepDrops" $ do
      prop "displays normal keepdrops properly" $
        \(n1, n2, n3) -> do
          printStage (KeepDropNode (n1 `d` n2) KeepHigh (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "kh" ++ numShow n3
          printStage (KeepDropNode (n1 `d` n2) KeepLow (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "kl" ++ numShow n3
          printStage (KeepDropNode (n1 `d` n2) DropHigh (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "dh" ++ numShow n3
          printStage (KeepDropNode (n1 `d` n2) DropLow (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "dl" ++ numShow n3
      prop "displays arbitrary keepdrops properly" $
        \(RandomNode n1) (RandomNode n2) ->
          let mid = makePrintWrapper $ precedence $ KeepDropNode n1 KeepHigh n2
              s1 = mid (Just PrecLeft) n1
              s2 = mid (Just PrecRight) n2
           in do
                printStage (KeepDropNode n1 KeepHigh n2) `shouldBe` s1 ++ "kh" ++ s2
                printStage (KeepDropNode n1 KeepLow n2) `shouldBe` s1 ++ "kl" ++ s2
                printStage (KeepDropNode n1 DropHigh n2) `shouldBe` s1 ++ "dh" ++ s2
                printStage (KeepDropNode n1 DropLow n2) `shouldBe` s1 ++ "dl" ++ s2
    context "when dealing with rerolls" $ do
      prop "displays normal equality rerolls properly" $
        \(n1, n2, n3, ns) -> do
          printStage (RerollNode (n1 `d` n2) (EqTest TestEq) (VectorNode (NumberNode n3) $ fmap NumberNode ns))
            `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "r(" ++ neIntercalate ", " (fmap numShow $ [n3] <> ns) ++ ")"
          printStage (RerollNode (n1 `d` n2) (EqTest TestNeq) (VectorNode (NumberNode n3) $ fmap NumberNode ns))
            `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "r/=(" ++ neIntercalate ", " (fmap numShow $ [n3] <> ns) ++ ")"
      prop "displays normal ord rerolls properly" $
        \(n1, n2, n3) -> do
          printStage (RerollNode (n1 `d` n2) (OrdTest TestGeq) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "r>=" ++ numShow n3
          printStage (RerollNode (n1 `d` n2) (OrdTest TestGre) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "r>" ++ numShow n3
          printStage (RerollNode (n1 `d` n2) (OrdTest TestLeq) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "r<=" ++ numShow n3
          printStage (RerollNode (n1 `d` n2) (OrdTest TestLes) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "r<" ++ numShow n3
      prop "displays normal interval rerolls properly" $
        \(n1, n2, n3, n4) -> do
          printStage (RerollNode (n1 `d` n2) (IntervalTest TestIn) (VectorNode (NumberNode n3) [NumberNode n4]))
            `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "rIn(" ++ numShow n3 ++ ", " ++ numShow n4 ++ ")"
          printStage (RerollNode (n1 `d` n2) (IntervalTest TestOut) (VectorNode (NumberNode n3) [NumberNode n4]))
            `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "rOut(" ++ numShow n3 ++ ", " ++ numShow n4 ++ ")"
    context "when dealing with explodes" $ do
      prop "displays normal equality explodes properly" $
        \(n1, n2, n3, ns) -> do
          printStage (ExplodeNode (n1 `d` n2) (EqTest TestEq) (VectorNode (NumberNode n3) $ fmap NumberNode ns))
            `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "!(" ++ neIntercalate ", " (fmap numShow $ [n3] <> ns) ++ ")"
          printStage (ExplodeNode (n1 `d` n2) (EqTest TestNeq) (VectorNode (NumberNode n3) $ fmap NumberNode ns))
            `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "!/=(" ++ neIntercalate ", " (fmap numShow $ [n3] <> ns) ++ ")"
      prop "displays normal ord explodes properly" $
        \(n1, n2, n3) -> do
          printStage (ExplodeNode (n1 `d` n2) (OrdTest TestGeq) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "!>=" ++ numShow n3
          printStage (ExplodeNode (n1 `d` n2) (OrdTest TestGre) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "!>" ++ numShow n3
          printStage (ExplodeNode (n1 `d` n2) (OrdTest TestLeq) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "!<=" ++ numShow n3
          printStage (ExplodeNode (n1 `d` n2) (OrdTest TestLes) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "!<" ++ numShow n3
      prop "displays normal interval explodes properly" $
        \(n1, n2, n3, n4) -> do
          printStage (ExplodeNode (n1 `d` n2) (IntervalTest TestIn) (VectorNode (NumberNode n3) [NumberNode n4]))
            `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "!In(" ++ numShow n3 ++ ", " ++ numShow n4 ++ ")"
          printStage (ExplodeNode (n1 `d` n2) (IntervalTest TestOut) (VectorNode (NumberNode n3) [NumberNode n4]))
            `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "!Out(" ++ numShow n3 ++ ", " ++ numShow n4 ++ ")"
    context "when dealing with success" $ do
      context "as a dice pool modifier" $ do
        prop "displays normal equality success properly" $
          \(n1, n2, n3, ns) -> do
            printStage (SuccessNode (n1 `d` n2) (EqTest TestEq) (VectorNode (NumberNode n3) $ fmap NumberNode ns))
              `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "==(" ++ neIntercalate ", " (fmap numShow $ [n3] <> ns) ++ ")"
            printStage (SuccessNode (n1 `d` n2) (EqTest TestNeq) (VectorNode (NumberNode n3) $ fmap NumberNode ns))
              `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "/=(" ++ neIntercalate ", " (fmap numShow $ [n3] <> ns) ++ ")"
        prop "displays normal ord success properly" $
          \(n1, n2, n3) -> do
            printStage (SuccessNode (n1 `d` n2) (OrdTest TestGeq) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ ">=" ++ numShow n3
            printStage (SuccessNode (n1 `d` n2) (OrdTest TestGre) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ ">" ++ numShow n3
            printStage (SuccessNode (n1 `d` n2) (OrdTest TestLeq) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "<=" ++ numShow n3
            printStage (SuccessNode (n1 `d` n2) (OrdTest TestLes) (NumberNode n3)) `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "<" ++ numShow n3
        prop "displays normal interval success properly" $
          \(n1, n2, n3, n4) -> do
            printStage (SuccessNode (n1 `d` n2) (IntervalTest TestIn) (VectorNode (NumberNode n3) [NumberNode n4]))
              `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "In(" ++ numShow n3 ++ ", " ++ numShow n4 ++ ")"
            printStage (SuccessNode (n1 `d` n2) (IntervalTest TestOut) (VectorNode (NumberNode n3) [NumberNode n4]))
              `shouldBe` numShow n1 ++ "d" ++ numShow n2 ++ "Out(" ++ numShow n3 ++ ", " ++ numShow n4 ++ ")"
      context "as a test" $ do
        prop "displays normal equality tests properly" $
          \(n1, n2, ns) -> do
            printStage (SuccessNode (NumberNode n1) (EqTest TestEq) (VectorNode (NumberNode n2) $ fmap NumberNode ns))
              `shouldBe` numShow n1 ++ "==(" ++ neIntercalate ", " (fmap numShow $ [n2] <> ns) ++ ")"
            printStage (SuccessNode (NumberNode n1) (EqTest TestNeq) (VectorNode (NumberNode n2) $ fmap NumberNode ns))
              `shouldBe` numShow n1 ++ "/=(" ++ neIntercalate ", " (fmap numShow $ [n2] <> ns) ++ ")"
        prop "displays normal ord tests properly" $
          \(n1, n2) -> do
            printStage (SuccessNode (NumberNode n1) (OrdTest TestGeq) (NumberNode n2)) `shouldBe` numShow n1 ++ ">=" ++ numShow n2
            printStage (SuccessNode (NumberNode n1) (OrdTest TestGre) (NumberNode n2)) `shouldBe` numShow n1 ++ ">" ++ numShow n2
            printStage (SuccessNode (NumberNode n1) (OrdTest TestLeq) (NumberNode n2)) `shouldBe` numShow n1 ++ "<=" ++ numShow n2
            printStage (SuccessNode (NumberNode n1) (OrdTest TestLes) (NumberNode n2)) `shouldBe` numShow n1 ++ "<" ++ numShow n2
        prop "displays normal interval tests properly" $
          \(n1, n2, n3) -> do
            printStage (SuccessNode (NumberNode n1) (IntervalTest TestIn) (VectorNode (NumberNode n2) [NumberNode n3])) `shouldBe` numShow n1 ++ "In(" ++ numShow n2 ++ ", " ++ numShow n3 ++ ")"
            printStage (SuccessNode (NumberNode n1) (IntervalTest TestOut) (VectorNode (NumberNode n2) [NumberNode n3])) `shouldBe` numShow n1 ++ "Out(" ++ numShow n2 ++ ", " ++ numShow n3 ++ ")"
    context "when dealing with not nodes" $ do
      prop "displays arbitrary contained nodes properly" $
        \(RandomNode n) -> do
          let s = makePrintWrapper (precedence $ NotNode n) Nothing n
           in printStage (NotNode n) `shouldBe` "~" ++ s
    context "when dealing with and nodes" $ do
      prop "displays arbitrary contained nodes properly" $
        \(RandomNode n, RandomNodes ns) ->
          let andNode = AndNode n ns
              mid = makePrintWrapper $ precedence andNode
              s1 = mid (Just PrecLeft) n
              sFn = mid $ Just PrecRight
           in printStage andNode `shouldBe` s1 ++ mconcat (map (("&&" <>) . sFn) $ toList ns)
    context "when dealing with or nodes" $ do
      prop "displays arbitrary contained nodes properly" $
        \(RandomNode n, RandomNodes ns) ->
          let andNode = OrNode n ns
              mid = makePrintWrapper $ precedence andNode
              s1 = mid (Just PrecLeft) n
              sFn = mid $ Just PrecRight
           in printStage andNode `shouldBe` s1 ++ mconcat (map (("||" <>) . sFn) $ toList ns)
    context "when dealing with exponents" $ do
      prop "displays simple computations properly" $ do
        \n1 n2 -> printStage (ExponentNode (NumberNode n1) (NumberNode n2)) `shouldBe` numShow n1 ++ "^" ++ numShow n2
      prop "displays arbitrary children properly" $ do
        \(RandomNode n1) (RandomNode n2) ->
          let ex = ExponentNode n1 n2
              mid = makePrintWrapper $ precedence ex
              s1 = mid (Just PrecLeft) n1
              s2 = mid (Just PrecRight) n2
           in printStage ex `shouldBe` s1 ++ "^" ++ s2
    context "when dealing with multiplication" $ do
      prop "displays simple computations properly" $ do
        \(num, nums) ->
          let n = NumberNode num
              ns = fmap NumberNode nums
              multNode = MultNode n ns
              mid = makePrintWrapper $ precedence multNode
              s1 = mid (Just PrecLeft) n
              sFn = mid $ Just PrecRight
           in printStage multNode `shouldBe` s1 ++ mconcat (map (("*" <>) . sFn) $ toList ns)
      prop "displays arbitrary children properly" $ do
        \(RandomNode n, RandomNodes ns) ->
          let multNode = MultNode n ns
              mid = makePrintWrapper $ precedence multNode
              s1 = mid (Just PrecLeft) n
              sFn = mid $ Just PrecRight
              beforeConcat (RecipNode r) = "/" <> sFn r
              beforeConcat r = "*" <> sFn r
           in printStage multNode `shouldBe` s1 ++ mconcat (map beforeConcat $ toList ns)
    context "when dealing with reciprocals" $ do
      prop "displays simple computations properly" $ do
        \num ->
          let n = NumberNode num
              recipNode = RecipNode $ NumberNode num
           in printStage recipNode `shouldBe` "1/" ++ numShow num
      prop "displays arbitrary children properly" $ do
        \(RandomNode n) ->
          let recipNode = RecipNode n
              s = makePrintWrapper (precedence recipNode) Nothing n
           in printStage recipNode `shouldBe` "1/" <> s
    context "when dealing with modulus" $ do
      prop "displays simple computations properly" $ do
        \n1 n2 -> printStage (ModNode (NumberNode n1) (NumberNode n2)) `shouldBe` numShow n1 ++ "%" ++ numShow n2
      prop "displays arbitrary children properly" $ do
        \(RandomNode n1) (RandomNode n2) ->
          let modn = ModNode n1 n2
              mid = makePrintWrapper $ precedence modn
              s1 = mid (Just PrecLeft) n1
              s2 = mid (Just PrecRight) n2
           in printStage modn `shouldBe` s1 ++ "%" ++ s2
    context "when dealing with addition" $ do
      prop "displays simple computations properly" $ do
        \(num, nums) ->
          let n = NumberNode num
              ns = fmap NumberNode nums
              addNode = AddNode n ns
              mid = makePrintWrapper $ precedence addNode
              s1 = mid (Just PrecLeft) n
              sFn = mid $ Just PrecRight
              beforeConcat n
                | NumberNode num <- n, getReal num < 0 = "-" <> sFn (NumberNode (- num))
                | otherwise = "+" <> sFn n
           in printStage addNode `shouldBe` s1 ++ mconcat (map (("+" <>) . sFn) $ toList ns)
      prop "displays arbitrary children properly" $ do
        \(RandomNode n, RandomNodes ns) ->
          let addNode = AddNode n ns
              mid = makePrintWrapper $ precedence addNode
              s1 = mid (Just PrecLeft) n
              sFn = mid $ Just PrecRight
              beforeConcat (NegativeNode r) = "-" <> sFn r
              beforeConcat r = "+" <> sFn r
           in printStage addNode `shouldBe` s1 ++ mconcat (map beforeConcat $ toList ns)
    context "when dealing with negation" $ do
      prop "displays simple computations properly" $ do
        \num ->
          let n = NumberNode num
              negNode = NegativeNode $ NumberNode num
           in printStage negNode `shouldBe` "-" ++ numShow num
      prop "displays arbitrary children properly" $ do
        \(RandomNode n) ->
          let negNode = NegativeNode n
              s = makePrintWrapper (precedence negNode) Nothing n
           in printStage negNode `shouldBe` "-" <> s
    context "when dealing with single function notation nodes" $ do
      prop "displays arbitrary contained nodes properly" $
        \(RandomNode n) -> do
          printStage (BoolNode n) `shouldBe` "bool(" ++ printStage n ++ ")"
          printStage (CeilingNode n) `shouldBe` "ceil(" ++ printStage n ++ ")"
          printStage (FloorNode n) `shouldBe` "floor(" ++ printStage n ++ ")"
          printStage (RoundNode n) `shouldBe` "round(" ++ printStage n ++ ")"
    context "when dealing with if" $ do
      prop "displays arbitrary contained nodes properly" $
        \(RandomNode n1, RandomNode n2, RandomNode n3) -> do
          printStage (IfNode n1 n2 n3) `shouldBe` "if(" ++ intercalate ", " (map printStage [n1, n2, n3]) ++ ")"
    context "when dealing with indefinite function notation nodes/vectors" $ do
      prop "displays arbitrary contained nodes properly" $
        \(RandomNodes nes@(n :| ns)) -> do
          printStage (MinNode nes) `shouldBe` "min(" ++ printStage n ++ mconcat (map ((", " <>) . printStage) ns) ++ ")"
          printStage (MaxNode nes) `shouldBe` "max(" ++ printStage n ++ mconcat (map ((", " <>) . printStage) ns) ++ ")"
          printStage (VectorNode n nes) `shouldBe` "(" ++ printStage n ++ mconcat (map ((", " <>) . printStage) $ [n] <> ns) ++ ")"
    context "when dealing with resolves" $ do
      prop "displays arbitrary children properly" $ do
        let tester = 1
            -- Dumb, but should be fine for now
            dropEnd s = take (length s - 1) s
            matcher n
              | VectorNode _ _ <- n = printStage (ResolveNode n) `shouldBe` "[" <> drop 1 (dropEnd $ printStage n) <> "]"
              | otherwise = printStage (ResolveNode n) `shouldBe` "[" <> printStage n <> "]"
         in \(RandomNode n) -> matcher n
  describe "resolve" $ do
    it "passes static sanity checks" $ do
      staticResolve (n 1) `shouldBe` (Right $ Resolved.Number 1, ([("1", "1")], staticGen))
      staticResolve (n 1 -+- fmap n [2, 3]) `shouldBe` (Right $ Resolved.Number 6, ([("1+2+3", "6")], staticGen))
      staticResolve (n 1 -+- [n 2]) `shouldBe` (Right $ Resolved.Number 3, ([("1+2", "3")], staticGen))
      staticResolve (n 1 -+- [n 2, n 3]) `shouldBe` (Right $ Resolved.Number 6, ([("1+2+3", "6")], staticGen))
      staticResolve (n 2 -*- [n 3]) `shouldBe` (Right $ Resolved.Number 6, ([("2*3", "6")], staticGen))
      staticResolve (n 1 -*- fmap n [2, 3]) `shouldBe` (Right $ Resolved.Number 6, ([("1*2*3", "6")], staticGen))
      staticResolve (n 1 -*- [n 2] -+- [n 3]) `shouldBe` (Right $ Resolved.Number 5, ([("1*2+3", "5")], staticGen))
      staticResolve ((n 1 -+- [n 2]) -*- [n 3]) `shouldBe` (Right $ Resolved.Number 9, ([("(1+2)*3", "9")], staticGen))
      staticResolve (n 1 -+- [neg 2, n 3]) `shouldBe` (Right $ Resolved.Number 2, ([("1-2+3", "2")], staticGen))
      staticResolve (neg 2 -+- [neg 3]) `shouldBe` (Right $ Resolved.Number $ - 5, ([("-2-3", "-5")], staticGen))
      staticSimplify (1 `d` 6) `shouldBe` (Right $ Simplified.Dice $ Simplified.Ordered $ createDicePool 1 $ createSimpleDie 6, ([("1d6", "1d6")], staticGen))

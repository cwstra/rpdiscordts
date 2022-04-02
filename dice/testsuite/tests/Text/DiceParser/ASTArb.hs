module Text.DiceParser.ASTArb (ValidNode (..), RandomNode (..), RandomNodes (..)) where

import Arb
import Data.KeepDrop
import Data.NumTestArb
import Data.UserNumberArb
import Test.QuickCheck
import Text.DiceParser.AST
import qualified Text.Show

newtype ValidNode = ValidNode ASTNode

newtype RandomNode = RandomNode ASTNode

instance Show RandomNode where
  show (RandomNode n) = show n

instance Arbitrary RandomNode where
  arbitrary = RandomNode <$> sized arbSized
    where
      arbNum = NumberNode <$> arbitrary
      arbBool = BooleanNode <$> arbitrary
      arbKd g = do
        n1 <- g
        n2 <- g
        elements [kh n1 n2, kl n1 n2, dh n1 n2, dl n1 n2]
        where
          kh n1 n2 = KeepDropNode n1 KeepHigh n2
          kl n1 n2 = KeepDropNode n1 KeepLow n2
          dh n1 n2 = KeepDropNode n1 DropHigh n2
          dl n1 n2 = KeepDropNode n1 DropLow n2
      arbComp n =
        oneof
          [ liftA2 DiceNode smaller smaller,
            FudgeNode <$> smaller,
            arbKd smaller,
            liftA3 RerollNode smaller arbitrary smaller,
            liftA3 ExplodeNode smaller arbitrary smaller,
            liftA3 SuccessNode smaller arbitrary smaller,
            BoolNode <$> smaller,
            NotNode <$> smaller,
            AndNode <$> smaller <*> nonEmptyOf smaller,
            OrNode <$> smaller <*> nonEmptyOf smaller,
            liftA3 IfNode smaller smaller smaller,
            liftA2 ExponentNode smaller smaller,
            MultNode <$> smaller <*> nonEmptyOf smaller,
            RecipNode <$> smaller,
            liftA2 ModNode smaller smaller,
            AddNode <$> smaller <*> nonEmptyOf smaller,
            NegativeNode <$> smaller,
            CeilingNode <$> smaller,
            FloorNode <$> smaller,
            RoundNode <$> smaller,
            MaxNode <$> nonEmptyOf smaller,
            MinNode <$> nonEmptyOf smaller,
            VectorNode <$> smaller <*> nonEmptyOf smaller,
            ResolveNode <$> smaller
          ]
        where
          smaller = arbSized $ n `div` 8
      arbSized n
        | n <= 0 = oneof [arbNum, arbBool]
        | otherwise = oneof [arbNum, arbBool, arbComp n]
  shrink (RandomNode node)
    | NumberNode n <- node = map (RandomNode . NumberNode) $ shrink n
    | BooleanNode b <- node = map (RandomNode . BooleanNode) $ shrink b
    | DiceNode n1 n2 <- node = binode DiceNode n1 n2
    | FudgeNode n <- node = uninode FudgeNode n
    | KeepDropNode n1 kd n2 <- node = binode (`KeepDropNode` kd) n1 n2
    | RerollNode n1 t n2 <- node = binode (`RerollNode` t) n1 n2
    | ExplodeNode n1 t n2 <- node = binode (`ExplodeNode` t) n1 n2
    | SuccessNode n1 t n2 <- node = binode (`SuccessNode` t) n1 n2
    | BoolNode n <- node = uninode BoolNode n
    | NotNode n <- node = uninode NotNode n
    | AndNode n ns <- node = atLeastTwoNode MultNode n ns
    | OrNode n ns <- node = atLeastTwoNode MultNode n ns
    | IfNode n1 n2 n3 <- node = map RandomNode $ do
      (RandomNode r1) <- shrink $ RandomNode n1
      (RandomNode r2) <- shrink $ RandomNode n2
      (RandomNode r3) <- shrink $ RandomNode n3
      [r1, r2, r3, IfNode r1 r2 r3]
    | ExponentNode n1 n2 <- node = binode ExponentNode n1 n2
    | MultNode n ns <- node = atLeastTwoNode MultNode n ns
    | RecipNode n <- node = uninode RecipNode n
    | ModNode n1 n2 <- node = binode ModNode n1 n2
    | AddNode n ns <- node = atLeastTwoNode MultNode n ns
    | NegativeNode n <- node = uninode NegativeNode n
    | CeilingNode n <- node = uninode CeilingNode n
    | FloorNode n <- node = uninode FloorNode n
    | RoundNode n <- node = uninode RoundNode n
    | MaxNode ns <- node = nenode MaxNode ns
    | MinNode ns <- node = nenode MinNode ns
    | VectorNode n ns <- node = atLeastTwoNode VectorNode n ns
    | ResolveNode n <- node = uninode ResolveNode n
    where
      extract (RandomNode n) = n
      uninode fn n =
        [ RandomNode x
          | RandomNode r <- shrink $ RandomNode n,
            x <- [r, fn r]
        ]
      binode fn n1 n2 =
        [ RandomNode x
          | (RandomNode r1, RandomNode r2) <- shrink (RandomNode n1, RandomNode n2),
            x <- [r1, r2, fn r1 r2]
        ]
      nenode fn ns =
        [ RandomNode x
          | rs <- map (extract <$>) $ shrink $ RandomNode <$> ns,
            x <- fn rs : toList rs
        ]
      atLeastTwoNode fn n1 (n2 :| ns) =
        [ RandomNode x
          | rs <- map (extract <$>) $ shrink $ RandomNode <$> (n1 : n2 : ns),
            x <- maybeToList (maybeFn rs) ++ toList rs
        ]
        where
          maybeFn (n1 : n2 : ns) = Just $ fn n1 (n2 :| ns)
          maybeFn _ = Nothing

newtype RandomNodes = RandomNodes (NonEmpty ASTNode)

instance Show RandomNodes where
  show (RandomNodes n) = show n

extractRandom (RandomNode n) = n

instance Arbitrary RandomNodes where
  arbitrary = RandomNodes . fmap extractRandom <$> arbitrary
  shrink (RandomNodes ns) = map (RandomNodes . fmap extractRandom) $ shrink $ fmap RandomNode ns

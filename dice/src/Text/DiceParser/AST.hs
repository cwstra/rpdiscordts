{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Text.DiceParser.AST
  ( enclose,
    ASTNode (..),
    PrecDirection (..),
    Precedence,
    precedence,
    precGreater,
    printStage,
    simplify,
    resolve,
  )
where

import Data.Dice
import Data.HistoryM (HistoryM)
import qualified Data.HistoryM as HM
import Data.KeepDrop
import qualified Data.List.NonEmpty as NE
import Data.NumTest
import Data.Resolved (Resolved)
import qualified Data.Resolved as Resolved
import Data.Simplified (Simplified)
import qualified Data.Simplified as Simplified
import qualified Data.Text as T
import Data.UserNumber
import System.Random
import qualified Text.Show

liftMaybe :: Text -> Maybe a -> HistoryM g a
liftMaybe _ (Just a) = return a
liftMaybe b _ = hoistEither $ Left b

infixText :: Text -> Text -> Text -> Text
infixText inner left right = T.concat [left, inner, right]

enclose :: Semigroup s => s -> s -> s -> s
enclose left right inner = left <> inner <> right

data ASTNode
  = NumberNode GeneralNumber
  | BooleanNode Bool
  | -- Dice operations
    DiceNode ASTNode ASTNode
  | FudgeNode ASTNode
  | KeepDropNode ASTNode (forall a. Ord a => Int -> KeepDrop a) ASTNode
  | RerollNode ASTNode PartialNumTest ASTNode
  | ExplodeNode ASTNode PartialNumTest ASTNode
  | NaturalExplodeNode ASTNode
  | SuccessNode ASTNode PartialNumTest ASTNode
  | -- Logical Functions
    BoolNode ASTNode
  | NotNode ASTNode
  | AndNode ASTNode (NonEmpty ASTNode)
  | OrNode ASTNode (NonEmpty ASTNode)
  | IfNode ASTNode ASTNode ASTNode
  | -- Numeric Function
    ExponentNode ASTNode ASTNode
  | MultNode ASTNode (NonEmpty ASTNode)
  | RecipNode ASTNode
  | ModNode ASTNode ASTNode
  | AddNode ASTNode (NonEmpty ASTNode)
  | NegativeNode ASTNode
  | CeilingNode ASTNode
  | FloorNode ASTNode
  | RoundNode ASTNode
  | MaxNode (NonEmpty ASTNode)
  | MinNode (NonEmpty ASTNode)
  | -- Other nodes
    VectorNode ASTNode (NonEmpty ASTNode)
  | ResolveNode ASTNode

instance Eq ASTNode where
  NumberNode n1 == NumberNode n2 = n1 == n2
  BooleanNode b1 == BooleanNode b2 = b1 == b2
  -- Dice operations
  DiceNode n1a n1b == DiceNode n2a n2b = n1a == n2a && n1b == n2b
  FudgeNode n1 == FudgeNode n2 = n1 == n2
  KeepDropNode n1a t1 n1b == KeepDropNode n2a t2 n2b = n1a == n2a && (t1 1 :: KeepDrop Int) == t2 1 && n1b == n2b
  --KeepDropNode ASTNode (forall a. Ord a => Int -> KeepDrop a) ASTNode
  RerollNode n1a t1 n1b == RerollNode n2a t2 n2b = n1a == n2a && applyPartial t1 == applyPartial t2 && n1b == n2b
  ExplodeNode n1a t1 n1b == ExplodeNode n2a t2 n2b = n1a == n2a && applyPartial t1 == applyPartial t2 && n1b == n2b
  SuccessNode n1a t1 n1b == SuccessNode n2a t2 n2b = n1a == n2a && applyPartial t1 == applyPartial t2 && n1b == n2b
  -- Logical Functions
  BoolNode n1 == BoolNode n2 = n1 == n2
  NotNode n1 == NotNode n2 = n1 == n2
  AndNode n1 n1s == AndNode n2 n2s = n1 == n2 && n1s == n2s
  OrNode n1 n1s == OrNode n2 n2s = n1 == n2 && n1s == n2s
  IfNode n1a n1b n1c == IfNode n2a n2b n2c = n1a == n2a && n1b == n2b && n1c == n2c
  -- Numeric Function
  ExponentNode n1a n1b == ExponentNode n2a n2b = n1a == n2a && n1b == n2b
  MultNode n1 n1s == MultNode n2 n2s = n1 == n2 && n1s == n2s
  RecipNode n1 == RecipNode n2 = n1 == n2
  ModNode n1a n1b == ModNode n2a n2b = n1a == n2a && n1b == n2b
  AddNode n1 n1s == AddNode n2 n2s = n1 == n2 && n1s == n2s
  NegativeNode n1 == NegativeNode n2 = n1 == n2
  CeilingNode n1 == CeilingNode n2 = n1 == n2
  FloorNode n1 == FloorNode n2 = n1 == n2
  RoundNode n1 == RoundNode n2 = n1 == n2
  MaxNode n1 == MaxNode n2 = n1 == n2
  MinNode n1 == MinNode n2 = n1 == n2
  -- Other nodes
  VectorNode n1 n1s == VectorNode n2 n2s = n1 == n2 && n1s == n2s
  ResolveNode n1 == ResolveNode n2 = n1 == n2
  _ == _ = False

instance Show ASTNode where
  show node
    | (NumberNode n) <- node = "NumberNode " ++ show n
    | (BooleanNode n) <- node = "BooleanNode " ++ show n
    | (DiceNode n1 n2) <- node = "Dice" ++ joiner [show n1, show n2]
    | (FudgeNode n) <- node = "Fudge" ++ joiner [show n]
    | (KeepDropNode n1 kd n2) <- node =
      "KeepDrop" ++ joiner [show n1, showPartialKeepDrop kd, show n2]
    | (RerollNode n1 t n2) <- node =
      "Reroll" ++ joiner [show n1, showPartialNumTest False t, show n2]
    | (ExplodeNode n1 t n2) <- node =
      "Explode" ++ joiner [show n1, showPartialNumTest False t, show n2]
    | (NaturalExplodeNode n) <- node = "NaturalExplode" ++ joiner [show n]
    | (SuccessNode n1 t n2) <- node =
      "Success" ++ joiner [show n1, showPartialNumTest True t, show n2]
    | (BoolNode n) <- node = "Bool" ++ joiner [show n]
    | (NotNode n) <- node = "Not" ++ joiner [show n]
    | (AndNode n ns) <- node = "And" ++ joiner (map show $ n : toList ns)
    | (OrNode n ns) <- node = "Or" ++ joiner (map show $ n : toList ns)
    | (IfNode n1 n2 n3) <- node = "If" ++ joiner [show n1, show n2, show n3]
    | (ExponentNode n1 n2) <- node = "Exponent" ++ joiner [show n1, show n2]
    | (MultNode n ns) <- node = "Mult" ++ joiner (map show $ n : toList ns)
    | (RecipNode n) <- node = "Recip" ++ joiner [show n]
    | (ModNode n1 n2) <- node = "Mod" ++ joiner [show n1, show n2]
    | (AddNode n ns) <- node = "Add" ++ joiner (map show $ n : toList ns)
    | (NegativeNode n) <- node = "Negative" ++ joiner [show n]
    | (CeilingNode n) <- node = "Ceiling" ++ joiner [show n]
    | (FloorNode n) <- node = "Floor" ++ joiner [show n]
    | (RoundNode n) <- node = "Round" ++ joiner [show n]
    | (MaxNode ns) <- node = "Max" ++ joiner (map show $ toList ns)
    | (MinNode ns) <- node = "Min" ++ joiner (map show $ toList ns)
    | (VectorNode n ns) <- node = "Vector" ++ joiner (map show $ n : toList ns)
    | (ResolveNode n) <- node = "Resolve" ++ joiner [show n]
    where
      joiner = (++ ")") . ("Node (" ++) . intercalate ") ("

data PrecDirection = PrecLeft | PrecUnary | PrecRight
  deriving (Eq, Show)

type Precedence = Maybe (Int, PrecDirection)

precedence :: ASTNode -> Precedence
precedence DiceNode {} = Just (12, PrecLeft)
precedence FudgeNode {} = Just (11, PrecLeft)
precedence KeepDropNode {} = Just (10, PrecLeft)
precedence RerollNode {} = Just (10, PrecLeft)
precedence ExplodeNode {} = Just (10, PrecLeft)
precedence NaturalExplodeNode {} = Just (9, PrecUnary)
precedence SuccessNode {} = Just (1, PrecLeft)
precedence NotNode {} = Just (6, PrecUnary)
precedence AndNode {} = Just (3, PrecLeft)
precedence OrNode {} = Just (2, PrecLeft)
precedence ExponentNode {} = Just (4, PrecRight)
precedence MultNode {} = Just (3, PrecLeft)
precedence RecipNode {} = Just (3, PrecLeft)
precedence ModNode {} = Just (3, PrecLeft)
precedence AddNode {} = Just (2, PrecLeft)
precedence NegativeNode {} = Just (6, PrecUnary)
precedence NumberNode {} = Nothing
precedence BooleanNode {} = Nothing
precedence BoolNode {} = Nothing
precedence IfNode {} = Nothing
precedence CeilingNode {} = Nothing
precedence FloorNode {} = Nothing
precedence RoundNode {} = Nothing
precedence MaxNode {} = Nothing
precedence MinNode {} = Nothing
precedence VectorNode {} = Nothing
precedence ResolveNode {} = Nothing

precGreater :: Maybe PrecDirection -> Precedence -> Precedence -> Bool
precGreater side (Just (parentPrec, parentDir)) (Just (childPrec, childDir)) =
  (parentPrec > childPrec) || (parentPrec == childPrec && not (parentDir == childDir || Just parentDir == side))
precGreater _ _ _ = False

precWrap :: Precedence -> Maybe PrecDirection -> (ASTNode -> HistoryM g a) -> ASTNode -> HistoryM g a
precWrap parentPrec side fn child
  | precGreater side parentPrec (precedence child) = HM.withMap (enclose "(" ")") (fn child)
  | otherwise = fn child

printStage :: (IsString s, Semigroup s) => ASTNode -> s
printStage = go Nothing Nothing
  where
    precParen :: (IsString s, Semigroup s) => Precedence -> Precedence -> Maybe PrecDirection -> s -> s
    precParen parent child side
      | precGreater side parent child = enclose "(" ")"
      | otherwise = id
    multiStep childPrec acc (RecipNode d) = acc <> "/" <> go childPrec (Just PrecRight) d
    multiStep childPrec acc n = acc <> "*" <> go childPrec (Just PrecRight) n
    addStep childPrec acc (NegativeNode n) = acc <> "-" <> go childPrec (Just PrecRight) n
    addStep childPrec acc n = acc <> "+" <> go childPrec (Just PrecRight) n
    go :: (IsString s, Semigroup s) => Precedence -> Maybe PrecDirection -> ASTNode -> s
    go parentPrec dir node
      | NumberNode n <- node = numShow n
      | BooleanNode b <- node = show b
      | DiceNode n1 n2 <- node =
        precParen parentPrec childPrec dir $
          go childPrec (Just PrecLeft) n1 <> "d" <> go childPrec (Just PrecRight) n2
      | FudgeNode n <- node = precParen parentPrec childPrec dir $ go childPrec Nothing n <> "dF"
      | KeepDropNode n1 kdFn n2 <- node = precParen parentPrec childPrec dir $ go childPrec (Just PrecLeft) n1 <> showPartialKeepDrop kdFn <> go childPrec (Just PrecRight) n2
      | RerollNode n1 t n2 <- node =
        precParen parentPrec childPrec dir $
          go childPrec (Just PrecLeft) n1 <> "r" <> showPartialNumTest False t <> go childPrec (Just PrecRight) n2
      | ExplodeNode n1 t n2 <- node =
        precParen parentPrec childPrec dir $
          go childPrec (Just PrecLeft) n1 <> "!" <> showPartialNumTest False t <> go childPrec (Just PrecRight) n2
      | NaturalExplodeNode n <- node = precParen parentPrec childPrec dir $ go childPrec Nothing n <> "!"
      | SuccessNode n1 t n2 <- node =
        precParen parentPrec childPrec dir $
          go childPrec (Just PrecLeft) n1 <> showPartialNumTest True t <> go childPrec (Just PrecRight) n2
      | BoolNode n <- node = "bool(" <> go childPrec Nothing n <> ")"
      | NotNode n <- node = precParen parentPrec childPrec dir $ "~" <> go childPrec Nothing n
      | AndNode n ns <- node =
        precParen parentPrec childPrec dir $
          foldl' (\acc e -> acc <> "&&" <> go childPrec (Just PrecRight) e) (go childPrec (Just PrecLeft) n) ns
      | OrNode n ns <- node =
        precParen parentPrec childPrec dir $
          foldl' (\acc e -> acc <> "||" <> go childPrec (Just PrecRight) e) (go childPrec (Just PrecLeft) n) ns
      | IfNode n1 n2 n3 <- node = "if(" <> go childPrec Nothing n1 <> ", " <> go childPrec Nothing n2 <> ", " <> go childPrec Nothing n3 <> ")"
      | ExponentNode n1 n2 <- node =
        precParen parentPrec childPrec dir $
          go childPrec (Just PrecLeft) n1 <> "^" <> go childPrec (Just PrecRight) n2
      | MultNode n ns <- node =
        precParen parentPrec childPrec dir $
          foldl' (multiStep childPrec) (go childPrec (Just PrecLeft) n) ns
      | RecipNode n <- node =
        precParen parentPrec childPrec dir $
          "1/" <> go childPrec Nothing n
      | ModNode n1 n2 <- node =
        precParen parentPrec childPrec dir $
          go childPrec (Just PrecLeft) n1 <> "%" <> go childPrec (Just PrecRight) n2
      | AddNode n ns <- node =
        precParen parentPrec childPrec dir $
          foldl' (addStep childPrec) (go childPrec (Just PrecLeft) n) ns
      | NegativeNode n <- node = precParen parentPrec childPrec dir $ "-" <> go childPrec Nothing n
      | CeilingNode n <- node = "ceil(" <> go childPrec Nothing n <> ")"
      | FloorNode n <- node = "floor(" <> go childPrec Nothing n <> ")"
      | RoundNode n <- node = "round(" <> go childPrec Nothing n <> ")"
      | MinNode (n :| ns) <- node =
        "min(" <> foldl' (\acc e -> acc <> ", " <> go childPrec Nothing e) (go childPrec Nothing n) ns <> ")"
      | MaxNode (n :| ns) <- node =
        "max(" <> foldl' (\acc e -> acc <> ", " <> go childPrec Nothing e) (go childPrec Nothing n) ns <> ")"
      | VectorNode n1 (n2 :| ns) <- node =
        "(" <> foldl' (\acc e -> acc <> ", " <> go childPrec Nothing e) (go childPrec Nothing n1) (n2 : ns) <> ")"
      | ResolveNode (VectorNode n1 (n2 :| ns)) <- node =
        "[" <> foldl' (\acc e -> acc <> ", " <> go childPrec Nothing e) (go childPrec Nothing n1) (n2 : ns) <> "]"
      | ResolveNode n <- node = "[" <> go childPrec Nothing n <> "]"
      where
        childPrec = precedence node

simplify :: RandomGen g => ASTNode -> HistoryM g Simplified
simplify node
  | NumberNode n <- node = Simplified.Number <$> HM.showReturn n
  | BooleanNode b <- node = Simplified.Boolean <$> HM.showReturn b
  | DiceNode node1 node2 <- node = simplifyDiceNode prec node1 node2
  | FudgeNode node1 <- node = simplifyFudgeNode prec node1
  | KeepDropNode node1 kdfn node2 <- node = simplifyKeepDropNode prec node1 kdfn node2
  | RerollNode node1 tNode node2 <- node = simplifyRerollNode prec node1 tNode node2
  | ExplodeNode node1 tNode node2 <- node = simplifyExplodeNode prec node1 tNode node2
  | NaturalExplodeNode n <- node = simplifyNaturalExplodeNode prec n
  | SuccessNode node1 tNode node2 <- node = simplifySuccessNode prec node1 tNode node2
  | BoolNode node1 <- node = simplifyBoolNode node1
  | NotNode node1 <- node = simplifyNotNode prec node1
  | AndNode n ns <- node = simplifyAndNode prec $ n :| toList ns
  | OrNode n ns <- node = simplifyOrNode prec $ n :| toList ns
  | IfNode node1 node2 node3 <- node = simplifyIfNode node1 node2 node3
  | ExponentNode node1 node2 <- node = simplifyExponentNode prec node1 node2
  | MultNode n1 (n2 :| ns) <- node = simplifyMultNode prec $ n1 :| n2 : ns
  | RecipNode n <- node = simplifyRecipNode prec n
  | ModNode node1 node2 <- node = simplifyModNode prec node1 node2
  | AddNode n1 (n2 :| ns) <- node = simplifyAddNode prec $ n1 :| n2 : ns
  | NegativeNode n <- node = simplifyNegativeNode prec n
  | CeilingNode n <- node = simplifyCeilingNode n
  | FloorNode n <- node = simplifyFloorNode n
  | RoundNode n <- node = simplifyRoundNode n
  | MinNode ns <- node = simplifyMinNode ns
  | MaxNode ns <- node = simplifyMaxNode ns
  | VectorNode n ns <- node = simplifyVectorNode n ns
  | ResolveNode n <- node = simplifyResolveNode n
  where
    prec = precedence node

simplifyDiceNode :: RandomGen g => Precedence -> ASTNode -> ASTNode -> HistoryM g Simplified
simplifyDiceNode dicePrec node1 node2 = HM.withZip (infixText "d") $ do
  i <-
    HM.mapMaybe "Left hand argument to dice creation must be a positive integer" Resolved.getPositiveInt $
      precWrap dicePrec (Just PrecLeft) resolve node1
  resolved <- precWrap dicePrec (Just PrecRight) resolve node2
  liftMaybe "Right hand argument to dice creation must be a positive integer, or nonempty vector of numbers" $
    case resolved of
      Resolved.Number n2 -> do
        w2 <- fromGeneralNumber n2
        return $ Simplified.orderedDie $ createDicePool i $ createSimpleDie w2
      Resolved.Vector (v2 : v2s) -> do
        nv <- mapM Resolved.getNumber $ v2 :| v2s
        return $ Simplified.generalDie $ createDicePool i $ createGeneralDie nv
      _ -> Nothing

simplifyFudgeNode :: RandomGen g => Precedence -> ASTNode -> HistoryM g Simplified
simplifyFudgeNode fudgePrec node = HM.withMap (`T.append` "dF") $ do
  resolved <- precWrap fudgePrec Nothing resolve node
  w1 <-
    liftMaybe "Left hand argument to fudge dice creation must be a positive integer" $
      Resolved.getPositiveInt resolved
  return $ Simplified.Dice $ Simplified.General $ createDicePool w1 $ createGeneralDie [1, 0, -1]

simplifyKeepDropNode :: RandomGen g => Precedence -> ASTNode -> (forall a. Ord a => Int -> KeepDrop a) -> ASTNode -> HistoryM g Simplified
simplifyKeepDropNode kdPrec node1 kdFn node2 = HM.withZip (infixText $ T.pack $ showPartialKeepDrop kdFn) $ do
  dice <-
    HM.mapMaybe "Left hand argument to keep/drop must be a dice pool with real faces" Simplified.getOrderedDice $
      precWrap kdPrec (Just PrecLeft) simplify node1
  amount <-
    HM.mapMaybe "Right hand argument must be a positive integer" Resolved.getPositiveInt $
      precWrap kdPrec (Just PrecRight) resolve node2
  return $ Simplified.addKeepDrop dice kdFn amount

simplifyDiceTestNode ::
  RandomGen g =>
  Text ->
  (PartialNumTest -> Text) ->
  (forall a. DicePool a -> NumTest a -> Either Text (DicePool a)) ->
  Maybe (Precedence -> Resolved -> PartialNumTest -> ASTNode -> HistoryM g Simplified) ->
  Precedence ->
  ASTNode ->
  PartialNumTest ->
  ASTNode ->
  HistoryM g Simplified
simplifyDiceTestNode nodeName joinText applyFn fallback testPrec node1 pTest@(EqTest tFn) node2 = HM.withZip (infixText $ joinText pTest) $ do
  simplified <- precWrap testPrec (Just PrecLeft) simplify node1
  case (Simplified.getDice simplified, fallback) of
    (Nothing, Nothing) -> hoistEither $ Left $ T.concat ["Left hand argument to ", nodeName, " must be a dice pool"]
    (Nothing, Just fn) -> do
      r <- resolveSimplified simplified
      fn testPrec r pTest node2
    (Just dice, _) -> do
      resolved <- precWrap testPrec (Just PrecRight) resolve node2
      equalTo <-
        liftMaybe
          ( T.concat
              [ "Right hand argument to equality-based ",
                nodeName,
                " must be either a single number, or a non-empty vector of numbers"
              ]
          )
          $ Resolved.getNonEmptyVectorOf Resolved.getNumber resolved
      hoistEither $ Simplified.addEqTest applyFn dice tFn equalTo
simplifyDiceTestNode nodeName joinText applyFn fallback testPrec node1 pTest@(OrdTest tFn) node2 = HM.withZip (infixText $ joinText pTest) $ do
  simplified <- precWrap testPrec (Just PrecLeft) simplify node1
  case (Simplified.getOrderedDice simplified, fallback) of
    (Nothing, Nothing) ->
      hoistEither $ Left $ T.concat ["Left hand argument to ordered ", nodeName, " must be an dice pool with real faces"]
    (Nothing, Just fn) -> do
      r <- resolveSimplified simplified
      fn testPrec r pTest node2
    (Just dice, _) -> do
      resolved <- precWrap testPrec (Just PrecRight) resolve node2
      equalTo <-
        liftMaybe (T.concat ["Right hand argument to ordered ", nodeName, " must a single real number"]) $
          Resolved.getRealNumber resolved
      hoistEither $ Simplified.addOrdTest applyFn dice tFn equalTo
simplifyDiceTestNode nodeName joinText applyFn fallback testPrec node1 pTest@(IntervalTest tFn) node2 = HM.withZip (infixText $ joinText pTest) $ do
  simplified <- precWrap testPrec (Just PrecLeft) simplify node1
  case (Simplified.getOrderedDice simplified, fallback) of
    (Nothing, Nothing) ->
      hoistEither $ Left $ T.concat ["Left hand argument to ordered ", nodeName, " must be an dice pool with real faces"]
    (Nothing, Just fn) -> do
      r <- resolveSimplified simplified
      fn testPrec r pTest node2
    (Just dice, _) -> do
      resolved <- precWrap testPrec (Just PrecRight) resolve node2
      (start, end) <-
        liftMaybe (T.concat ["Right hand argument to interval ", nodeName, " must a pair of real numbers"]) $
          Resolved.getPairOf Resolved.getRealNumber resolved
      hoistEither $ Simplified.addIntervalTest applyFn dice tFn start end

simplifyRerollNode :: RandomGen g => Precedence -> ASTNode -> PartialNumTest -> ASTNode -> HistoryM g Simplified
simplifyRerollNode = simplifyDiceTestNode "reroll" (("r" <>) . T.pack . showPartialNumTest False) addReroll Nothing

simplifyExplodeNode :: RandomGen g => Precedence -> ASTNode -> PartialNumTest -> ASTNode -> HistoryM g Simplified
simplifyExplodeNode = simplifyDiceTestNode "explode" (("!" <>) . T.pack . showPartialNumTest False) addExplode Nothing

simplifyNaturalTestNode ::
  RandomGen g =>
  Text ->
  Text ->
  (forall a. DicePool a -> NumTest a -> Either Text (DicePool a)) ->
  Precedence ->
  ASTNode ->
  HistoryM g Simplified
simplifyNaturalTestNode nodeName postfix applyFn testPrec node = HM.withMap (<> postfix) $ do
  simplified <- precWrap testPrec Nothing simplify node
  case Simplified.getOrderedDice simplified of
    Nothing ->
      hoistEither $ Left $ T.concat ["Argument to postfix ", nodeName, " must be an dice pool with real faces"]
    Just dice -> hoistEither $ Simplified.addNaturalTest applyFn dice

simplifyNaturalExplodeNode :: RandomGen g => Precedence -> ASTNode -> HistoryM g Simplified
simplifyNaturalExplodeNode = simplifyNaturalTestNode "explode" "!" addExplode

successFallback ::
  RandomGen g =>
  (PartialNumTest -> Text) ->
  Precedence ->
  Resolved ->
  PartialNumTest ->
  ASTNode ->
  HistoryM g Simplified
successFallback joinText testPrec resolved1 pTest@(EqTest tFn) node2 = do
  resolved2 <- precWrap testPrec (Just PrecRight) resolve node2
  return $ Simplified.Boolean $ resolved1 == resolved2
successFallback joinText testPrec resolved1 pTest@(OrdTest tFn) node2 = do
  real1 <-
    liftMaybe "Left-hand argument to ordered comparison must be a real number" $
      Resolved.getRealNumber resolved1
  resolved2 <- precWrap testPrec (Just PrecRight) resolve node2
  real2 <-
    liftMaybe "Right-hand argument to ordered comparison must be a real number" $
      Resolved.getRealNumber resolved2
  return $ Simplified.Boolean $ numTest (tFn real2) real1
successFallback joinText testPrec resolved1 pTest@(IntervalTest tFn) node2 = do
  real1 <-
    liftMaybe "Left-hand argument to ordered comparison must be a real number" $
      Resolved.getRealNumber resolved1
  resolved2 <- precWrap testPrec (Just PrecRight) resolve node2
  (start, end) <-
    liftMaybe "Right-hand argument to ordered comparison must be a real number" $
      Resolved.getPairOf Resolved.getRealNumber resolved2
  return $ Simplified.Boolean $ numTest (tFn start end) real1

simplifySuccessNode :: RandomGen g => Precedence -> ASTNode -> PartialNumTest -> ASTNode -> HistoryM g Simplified
simplifySuccessNode = simplifyDiceTestNode "success" display addSuccess (Just $ successFallback display)
  where
    display = T.pack . showPartialNumTest True

toBool :: Resolved -> Bool
toBool (Resolved.Number n) = n /= 0
toBool (Resolved.Boolean b) = b
toBool (Resolved.Vector []) = False
toBool (Resolved.Vector _) = True

simplifyBoolNode :: RandomGen g => ASTNode -> HistoryM g Simplified
simplifyBoolNode node = fmap Simplified.Boolean $
  HM.withMapReplace (enclose "bool(" ")") $ do
    resolved <- resolve node
    return $ toBool resolved

-- TL.mapThenReplace (enclose "bool(" ")") (show b) tl

simplifyNotNode :: RandomGen g => Precedence -> ASTNode -> HistoryM g Simplified
simplifyNotNode notPrec node = fmap Simplified.Boolean $
  HM.withMapReplace ("~" <>) $ do
    resolved <- precWrap notPrec Nothing resolve node
    return $ not $ toBool resolved

liftToMany ::
  RandomGen g =>
  (ASTNode -> HistoryM g a) ->
  Precedence ->
  NonEmpty ASTNode ->
  HistoryM g (NonEmpty a)
liftToMany fn parentPrec (n :| ns) = do
  r <- precWrap parentPrec (Just PrecLeft) fn n
  (r :|) <$> mapM (precWrap parentPrec (Just PrecRight) fn) ns

resolveManyNodes :: RandomGen g => Precedence -> NonEmpty ASTNode -> HistoryM g (NonEmpty Resolved)
resolveManyNodes = liftToMany resolve

simplifyAndNode :: RandomGen g => Precedence -> NonEmpty ASTNode -> HistoryM g Simplified
simplifyAndNode andPrec nodes = HM.withZipReplace Simplified.display (infixText "&&") $ do
  f :| r <- resolveManyNodes andPrec nodes
  return $ Simplified.Boolean $ foldl' (\acc e -> acc && toBool e) (toBool f) r

simplifyOrNode :: RandomGen g => Precedence -> NonEmpty ASTNode -> HistoryM g Simplified
simplifyOrNode orPrec nodes = HM.withZipReplace Simplified.display (infixText "||") $ do
  f :| r <- resolveManyNodes orPrec nodes
  return $ Simplified.Boolean $ foldl' (\acc e -> acc || toBool e) (toBool f) r

simplifyIfNode :: RandomGen g => ASTNode -> ASTNode -> ASTNode -> HistoryM g Simplified
simplifyIfNode condNode thenNode elseNode = HM.withJoin $ do
  let (thenDis, elseDis) = (printStage thenNode, printStage elseNode)
  cond <- HM.withMap (\s -> "if(" <> s <> ", " <> thenDis <> ", " <> elseDis <> ")") $ toBool <$> resolve condNode
  HM.traceHistory "If " $ simplify $ if cond then thenNode else elseNode

simplifyExponentNode :: RandomGen g => Precedence -> ASTNode -> ASTNode -> HistoryM g Simplified
simplifyExponentNode expPrec node1 node2 = HM.withZipReplace Simplified.display (infixText "^") $ do
  num1 <-
    HM.mapMaybe "Left hand argument to exponentiation must be a number" Resolved.getNumber $
      precWrap expPrec (Just PrecLeft) resolve node1
  num2 <-
    HM.mapMaybe "Right hand argument to exponentiation must be a number" Resolved.getNumber $
      precWrap expPrec (Just PrecRight) resolve node2
  return $ Simplified.Number $ genExp num1 num2

simplifyMultNode :: RandomGen g => Precedence -> NonEmpty ASTNode -> HistoryM g Simplified
simplifyMultNode multPrec nodes = HM.withZipReplace Simplified.display (infixText "*") $ do
  ss <- mapM (HM.mapMaybe "Argument to multiplication must be a number" Resolved.getNumber . wrapper resolve) nodes
  return $ Simplified.Number $ product ss
  where
    wrapper = precWrap multPrec (Just PrecLeft)

simplifyRecipNode :: RandomGen g => Precedence -> ASTNode -> HistoryM g Simplified
simplifyRecipNode recipPrec node = HM.withMap ("1/" <>) $ do
  n <- HM.mapMaybe "Argument to division must be a number" Resolved.getNumber $ precWrap recipPrec Nothing resolve node
  return $ Simplified.Number $ recip n

simplifyModNode :: RandomGen g => Precedence -> ASTNode -> ASTNode -> HistoryM g Simplified
simplifyModNode modPrec node1 node2 = HM.withZipReplace Simplified.display (infixText "%") $ do
  num1 <- HM.mapMaybe "Left-hand argument to modulus must be a number" Resolved.getInt $ precWrap modPrec (Just PrecLeft) resolve node1
  num2 <- HM.mapMaybe "Right-hand argument to modulus must be a number" Resolved.getInt $ precWrap modPrec (Just PrecRight) resolve node2
  return $ Simplified.Number $ toGeneralNumber $ mod num1 num2

simplifyAddNode :: RandomGen g => Precedence -> NonEmpty ASTNode -> HistoryM g Simplified
simplifyAddNode addPrec (n :| ns) = do
  --nums <- mapM (HM.mapMaybe "Argument to addition must be a number" Resolved.getNumber . resolve) nodes
  --return $ Simplified.Number $ sum nums
  res <-
    HM.withMapReplace
      ( ( if isNegative n
            then "-"
            else ""
        )
          <>
      )
      $ finish $ foldl' altStep (isNegative n, extractIfNegative n :| [], return 0) ns
  return $ Simplified.Number res
  where
    isNegative (NegativeNode _) = True
    isNegative _ = False
    extractIfNegative (NegativeNode n) = n
    extractIfNegative n = n
    resolvePos h ps = HM.withZipReplace show (infixText "+") $ do
      r1 <- h
      r2 <- mapM (HM.mapMaybe "Argument to addition must be a number" Resolved.getNumber . resolve) $ NE.reverse ps
      return $ r1 + sum r2
    resolveNeg h ns = HM.withZipReplace show (infixText "-") $ do
      r1 <- h
      r2 <- mapM (HM.mapMaybe "Argument to negation must be a number" Resolved.getNumber . resolve) $ NE.reverse ns
      return $ r1 - sum r2
    altStep ::
      RandomGen g =>
      (Bool, NonEmpty ASTNode, HistoryM g GeneralNumber) ->
      ASTNode ->
      (Bool, NonEmpty ASTNode, HistoryM g GeneralNumber)
    altStep (True, nh :| nr, h) (NegativeNode n) = (True, n :| (nh : nr), h)
    altStep (False, ps, h) (NegativeNode n) = (True, n :| [], resolvePos h ps)
    altStep (False, ph :| pr, h) p = (False, p :| (ph : pr), h)
    altStep (True, ns, h) p = (False, p :| [], resolveNeg h ns)
    finish :: RandomGen g => (Bool, NonEmpty ASTNode, HistoryM g GeneralNumber) -> HistoryM g GeneralNumber
    finish (True, ns, h) = resolveNeg h ns
    finish (False, ps, h) = resolvePos h ps

simplifyNegativeNode :: RandomGen g => Precedence -> ASTNode -> HistoryM g Simplified
simplifyNegativeNode negPrec node = HM.withMap ("-" <>) $ do
  n <- HM.mapMaybe "Argument to subtraction must be a number" Resolved.getNumber $ precWrap negPrec Nothing resolve node
  return $ Simplified.Number $ negate n

simplifyCeilingNode :: RandomGen g => ASTNode -> HistoryM g Simplified
simplifyCeilingNode node = HM.withMap (enclose "ceiling(" ")") $ do
  n <- HM.mapMaybe "Argument to ceiling must be a real number" Resolved.getRealNumber $ resolve node
  return $ Simplified.Number $ toGeneralNumber (ceiling n :: Integer)

simplifyFloorNode :: RandomGen g => ASTNode -> HistoryM g Simplified
simplifyFloorNode node = HM.withMap (enclose "floor(" ")") $ do
  n <- HM.mapMaybe "Argument to floor must be a real number" Resolved.getRealNumber $ resolve node
  return $ Simplified.Number $ toGeneralNumber (ceiling n :: Integer)

simplifyRoundNode :: RandomGen g => ASTNode -> HistoryM g Simplified
simplifyRoundNode node = HM.withMap (enclose "round(" ")") $ do
  n <- HM.mapMaybe "Argument to round must be a real number" Resolved.getRealNumber $ resolve node
  return $ Simplified.Number $ toGeneralNumber (round n :: Integer)

simplifyMinNode :: RandomGen g => NonEmpty ASTNode -> HistoryM g Simplified
simplifyMinNode nodes = HM.withMap (enclose "min(" ")") $
  HM.withZipReplace Simplified.display (infixText ", ") $ do
    (n :| ns) <- mapM (HM.mapMaybe "Arguments to min must be real numbers" Resolved.getRealNumber . resolve) nodes
    return $ Simplified.Number $ toGeneralNumber $ foldl' min n ns

simplifyMaxNode :: RandomGen g => NonEmpty ASTNode -> HistoryM g Simplified
simplifyMaxNode nodes = HM.withMap (enclose "max(" ")") $
  HM.withZipReplace Simplified.display (infixText ", ") $ do
    (n :| ns) <- mapM (HM.mapMaybe "Arguments to max must be real numbers" Resolved.getRealNumber . resolve) nodes
    return $ Simplified.Number $ toGeneralNumber $ foldl' max n ns

simplifyVectorNode :: RandomGen g => ASTNode -> NonEmpty ASTNode -> HistoryM g Simplified
simplifyVectorNode node nodes = HM.withMap (enclose "(" ")") $ do
  (s :| ss) <- HM.withZip (infixText ", ") $ mapM simplify $ [node] <> nodes
  return $ Simplified.Vector $ s : ss

simplifyResolveNode :: RandomGen g => ASTNode -> HistoryM g Simplified
simplifyResolveNode node = HM.withMap (enclose "[" "]") $ simplifiedToResolved <$> resolve node

simplifiedToResolved :: Resolved -> Simplified
simplifiedToResolved (Resolved.Number n) = Simplified.Number n
simplifiedToResolved (Resolved.Boolean b) = Simplified.Boolean b
simplifiedToResolved (Resolved.Vector rs) = Simplified.Vector $ map simplifiedToResolved rs

resolveSimplified :: RandomGen g => Simplified -> HistoryM g Resolved
resolveSimplified (Simplified.Number n) = return $ Resolved.Number n
resolveSimplified (Simplified.Boolean b) = return $ Resolved.Boolean b
resolveSimplified s = step s >>= HM.injectRollResult Resolved.display
  where
    step :: RandomGen g => Simplified -> HistoryM g (Text, Resolved)
    step (Simplified.Number n) = return (numShow n, Resolved.Number n)
    step (Simplified.Boolean b) = return (show b, Resolved.Boolean b)
    step (Simplified.Dice d) = second Resolved.Number <$> HM.simpleRoll d
    step (Simplified.Vector vs) = do
      reses <- mapM step vs
      return ("(" <> T.intercalate ", " (fmap fst reses) <> ")", Resolved.Vector $ fmap snd reses)

resolve :: RandomGen g => ASTNode -> HistoryM g Resolved
resolve = simplify >=> resolveSimplified

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Text.DiceParser.Parser
  ( runExprParser,
    booleanLiteral,
    numberLiteral,
    partialNumTest,
    expr,
    neCommas,
  )
where

import Control.Monad.Combinators.Expr
import Data.Attoparsec.Text (Parser, choice, parseOnly, skipSpace, (<?>))
import qualified Data.Attoparsec.Text as A
import Data.KeepDrop
import qualified Data.List.NonEmpty as NE
import Data.NumTest
import qualified Data.Text as T
import Data.UserNumber
import Text.DiceParser.AST

runExprParser :: Text -> Either Text ASTNode
runExprParser = first T.pack . parseOnly expr

parens :: Parser a -> Parser a
parens p = "(" *> p <* ")"

brackets :: Parser a -> Parser a
brackets p = "[" *> p <* "]"

neCommas :: Parser a -> Parser (NonEmpty a)
neCommas p = (:|) <$> p <*> many (skipSpace *> "," *> skipSpace *> p)

function :: Parser ASTNode
function = singleFn <|> ifParser <|> manyFn
  where
    singleParser (fn, p) = (fn <$ p) <*> parens expr
    singleFn =
      choice $
        map
          singleParser
          [(BoolNode, "bool"), (CeilingNode, "ceil"), (FloorNode, "floor"), (RoundNode, "round")]
    ifParser =
      (IfNode <$ "if(")
        <*> (expr <* skipSpace <* "," <* skipSpace)
        <*> (expr <* skipSpace <* "," <* skipSpace)
        <*> (expr <* skipSpace <* ")")
    manyParser (fn, p) = (fn <$ p) <*> parens (neCommas expr)
    manyFn = choice $ map manyParser [(MinNode, "min"), (MaxNode, "max")]

oneOrVector :: NonEmpty ASTNode -> ASTNode
oneOrVector (l :| []) = l
oneOrVector (l1 :| (l2 : ls)) = VectorNode l1 $ l2 :| ls

vector :: Parser ASTNode
vector = oneOrVector <$> parens (neCommas expr)

resolver :: Parser ASTNode
resolver = ResolveNode . oneOrVector <$> brackets (neCommas expr)

term :: Parser ASTNode
term = skipSpace *> (vector <|> resolver <|> function <|> NumberNode <$> numberLiteral <|> BooleanNode <$> booleanLiteral) <* skipSpace

numberLiteral :: Parser GeneralNumber
numberLiteral = do
  real <- realParser
  wrapper <- A.option GReal (A.char 'j' >> return (GComp . GC 0))
  return $ wrapper real
  where
    realParser = GSimp <$> (GFlo <$> float <|> GInt <$> integer)
    integer = A.decimal
    float = (nonLeadingfloat <|> leadingFloat) >>= toInt
      where
        nonLeadingfloat = do
          A.char '.'
          following <- A.many1' A.digit
          return $ "." <> following
        leadingFloat = do
          leading <- A.many1' A.digit
          A.char '.'
          following <- A.many' A.digit
          return $ leading <> "." <> following
        toInt str = case readMaybe str of
          Just f -> return f
          Nothing -> fail "How did we get here?"

booleanLiteral :: Parser Bool
booleanLiteral = A.asciiCI "true" $> True <|> A.asciiCI "false" $> False

expr = makeExprParser term table

dummyNode :: ASTNode
dummyNode = NumberNode 0

dummyTest :: PartialNumTest
dummyTest = EqTest TestEq

prec :: ASTNode -> Int
prec n
  | Just (i, _) <- precedence n = i
  | otherwise = -1

notFollowedBy :: Parser a -> Parser b -> Parser a
notFollowedBy want dontWant = want <* ((dontWant >> fail "not followed by") <|> return ())

--good <- want
--(dontWant >> fail "not followed by") <|> return ""
--return good

partialNumTest :: Maybe Char -> Parser PartialNumTest
partialNumTest postfix =
  (eq postfix $> EqTest TestEq <?> "TestEq")
    <|> (withP "/=" $> EqTest TestNeq <?> "TestNeq")
    <|> (withP "<=" $> OrdTest TestLeq)
    <|> (withP "<" $> OrdTest TestLes)
    <|> (withP ">=" $> OrdTest TestGeq)
    <|> (withP ">" $> OrdTest TestGre)
    <|> (withP "In" $> IntervalTest TestIn)
    <|> (withP "Out" $> IntervalTest TestOut)
  where
    eq = maybe (void "==") (void <$> A.char)
    makeP (Just p) = (A.char p *>)
    makeP _ = id
    withP = makeP postfix

table :: [[Operator Parser ASTNode]]
table =
  map (map fst . toList) $
    reverse $
      NE.groupAllWith
        snd
        [ addPrecPrefix 1 (DiceNode (NumberNode 1)) $ "d" `notFollowedBy` "F",
          biInfixL DiceNode $ "d" `notFollowedBy` "F",
          postfix FudgeNode "dF",
          biInfixL keepHighNode "kh",
          biInfixL keepLowNode "kl",
          biInfixL dropHighNode "dh",
          biInfixL dropLowNode "dl",
          testNode RerollNode (Just 'r'),
          testNode ExplodeNode (Just '!'),
          postfix NaturalExplodeNode "!",
          testNode SuccessNode Nothing,
          prefix NotNode "~",
          biInfixL (multiFn AndNode extractAnd) "&&",
          biInfixL (multiFn OrNode extractOr) "||",
          biInfixL ExponentNode "^",
          biInfixL (multiFn MultNode extractMult) "*",
          biInfixL divFn "/",
          biInfixL ModNode "%",
          biInfixR (multiFn AddNode extractAdd) "+",
          biInfixL subFn "-",
          prefix NegativeNode "-"
        ]
  where
    biInfixL makeNode parser = (InfixL (makeNode <$ parser), prec $ makeNode dummyNode dummyNode)
    biInfixR makeNode parser = (InfixR (makeNode <$ parser), prec $ makeNode dummyNode dummyNode)
    keepHighNode n1 n2 = KeepDropNode n1 KeepHigh n2
    keepLowNode n1 n2 = KeepDropNode n1 KeepLow n2
    dropHighNode n1 n2 = KeepDropNode n1 DropHigh n2
    dropLowNode n1 n2 = KeepDropNode n1 DropLow n2
    testNode makeTestNode c =
      (InfixL (flip makeTestNode <$> partialNumTest c), prec $ makeTestNode dummyNode dummyTest dummyNode)
    addPrecPrefix added makeNode parser = (Prefix (makeNode <$ parser), added + prec (makeNode dummyNode))
    prefix = addPrecPrefix 0
    postfix makeNode parser = (Postfix (makeNode <$ parser), prec (makeNode dummyNode))
    multiFn maker extractor node1 node2
      | (Left n1) <- extractor node1, (Left n2) <- extractor node2 = maker n1 $ n2 :| []
      | (Left n) <- extractor node1, (Right (la, lb :| ls)) <- extractor node2 = maker n $ la :| lb : ls
      -- Should attempt to avoid these cases; maybe use trace to ensure they aren't hit
      | (Right (la, ls)) <- extractor node1, (Left n) <- extractor node2 = trace "M3Branch" $ maker la $ ls <> [n]
      | (Right (l1a, l1s)) <- extractor node1, (Right (l2a, l2s)) <- extractor node2 = trace "M4Branch" $ maker l1a $ l1s <> [l2a] <> l2s
    extractAnd (AndNode a as) = Right (a, as)
    extractAnd n = Left n
    extractOr (OrNode a as) = Right (a, as)
    extractOr n = Left n
    extractMult (MultNode a as) = Right (a, as)
    extractMult n = Left n
    extractAdd (AddNode a as) = Right (a, as)
    extractAdd n = Left n
    -- Dont want to hit these first two
    divFn (MultNode l1 l1s) (MultNode l2 l2s) = trace "D1Branch" $ MultNode l1 $ l1s <> [RecipNode l2] <> l2s
    divFn (MultNode l ls) n = trace "D2Branch" $ MultNode l $ ls <> [RecipNode n]
    divFn n (MultNode l ls) = MultNode n $ [RecipNode l] <> ls
    divFn n1 n2 = MultNode n1 [RecipNode n2]
    subFn (AddNode l1 l1s) (AddNode l2 l2s) = AddNode l1 $ l1s <> [NegativeNode l2] <> l2s
    subFn (AddNode l ls) n = AddNode l $ ls <> [NegativeNode n]
    subFn n (AddNode l ls) = AddNode n $ [NegativeNode l] <> ls
    subFn n1 n2 = AddNode n1 [NegativeNode n2]

--testNode :: Maybe Char -> (ASTNode -> PartialNumTest -> ASTNode -> ASTNode) -> (Operator Parser ASTNode, Int)
--testNode p wrapper = do
--biInfixL (flip wrapper <$> partialNumTest p)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Pact.Core.IR.Parse where

import Data.Char(isAlphaNum, isLower)
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Data.Foldable(foldl')
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text(Text)
import qualified Data.Text as T
import Data.Void(Void)

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C

import Pact.Core.IR.ParseTree
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Type(PrimType(..))

import Data.Text.Prettyprint.Doc(Pretty(..))

newtype ViaPretty a = ViaPretty a

instance (Show a, Pretty a) => Show (ViaPretty a) where
  show (ViaPretty a) =
    show $ pretty a

data ParseError = ParseError Text deriving Show

type Parser = Parsec Void Text

dot :: Parser Char
dot = C.char '.'

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

spaceConsumer :: Parser ()
spaceConsumer = L.space C.hspace1 lineComment empty

spaceConsumerNL :: Parser ()
spaceConsumerNL = L.space C.space1 lineComment empty

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

variable :: Parser Text
variable = lexeme $ do
  c <- C.lowerChar
  rest <- takeWhileP Nothing (\c' -> isAlphaNum c' || c' == '-')
  pure (T.cons c rest)

moduleDeclName :: Parser Text
moduleDeclName = do
  c <- C.upperChar
  rest <- takeWhileP Nothing isAlphaNum
  pure (T.cons c rest)

moduleName :: Parser ModuleName
moduleName = do
  a <- moduleDeclName
  b <- optional (dot *> moduleDeclName)
  case b of
    Nothing -> return (ModuleName a Nothing) <?> "module name"
    Just b' -> return (ModuleName b' (Just . NamespaceName $ a)) <?> "namespaced module name"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

intLiteral :: Parser Literal
intLiteral = LInteger <$> lexeme L.decimal

boolLiteral :: Parser Literal
boolLiteral =
  (LBool True <$ symbol "True") <|>
  (LBool False <$ symbol "False")

unitLiteral :: Parser Literal
unitLiteral =
  LUnit <$ symbol "()"

-- Todo: improve on this. this is not efficient. Alternatively, use something like
-- the string parser in parsers
stringLiteral :: Parser Literal
stringLiteral = fmap (LString . T.pack) $
  C.char '"' *> manyTill L.charLiteral (C.char '"')

statement :: Parser (Expr ParsedName ())
statement =
  (letStatement <?> "let") <|>
  (ifStatement <?> "if") <|>
  (expr <?> "expr")

typeExpr :: Parser Type
typeExpr = do
  typ <- typeExpr'
  typs <- many (symbol "->" *> typeExpr')
  case NE.reverse (typ :| typs) of
    n :| ns -> pure $ foldl' (flip TyFun) n ns
  where
  objectBraces = between (symbol "{") (symbol "}")
  rawVar = lexeme $ takeWhile1P Nothing isLower
  typeExpr' =
    parens typeExpr
    <|> primType
    <|> listType
    <|> objectType
    <|> varType
  -- todo: repeated types
  -- todo: optimize this. Parser can be made without `try` unambiguously branching on `{`
  objectType =
    (TyObject mempty Nothing <$ symbol "{}") <|>
    ((TyObject mempty . Just) <$> (try $ objectBraces rawVar)) <|>
    objectType'
  varType = do
    _ <- C.char '\''
    TyVar <$> rawVar
  objectType' = objectBraces $ do
    t <- objField
    ts <- many (symbol "," *> objField)
    o <- optional (symbol "|" *> rawVar)
    pure (TyObject (Map.fromList (t:ts)) o)
    where
    objField = do
      f <- rawVar
      _ <- symbol ":"
      t <- typeExpr
      pure (Field f, t)
  listType = do
    _ <- symbol "List"
    typ <- (parens typeExpr <|> primType <|> objectType <|> varType)
    pure (TyList typ)
  primType = fmap TyPrim $
    (PrimInt <$ symbol "Int")
    <|> (PrimDecimal <$ symbol "Decimal")
    <|> (PrimString <$ symbol "String")
    <|> (PrimTime <$ symbol "Time")
    <|> (PrimBool <$ symbol "Bool")
    <|> (PrimUnit <$ symbol "Unit")

pactIndent :: Pos -> Maybe Pos
pactIndent i = Just (mkPos (unPos i + 2))

-- todo: document tricky case :^)
ifStatement :: Parser (Expr ParsedName ())
ifStatement = do
  i <- L.indentLevel
  ifStatement' i
  where
  ifStatement' i = L.indentBlock spaceConsumerNL $ do
    condE <- between (symbol "if") (symbol "then") expr
    (thenElseExpr condE) <|> (try (singleChar '{') *> pure (L.IndentSome (pactIndent i) (\bloc -> mkIf condE . (Block (NE.fromList bloc) (),) <$> thenElseStmt) statement))
    where
    mkIf condE (b1, b2) = If condE b1 b2 ()
    thenElseStmt = L.indentBlock spaceConsumerNL $ do
      _ <- singleChar '}' *> symbol "else"
      (L.IndentNone <$> expr) <|> (try (singleChar '{') *> pure (L.IndentSome (pactIndent i) (\bloc -> Block (NE.fromList bloc) () <$ singleChar '}') statement))
    thenElseExpr condE = do
      e <- expr
      _ <- symbol "else"
      (L.IndentNone . mkIf condE . (e,) <$> expr)
        <|> (singleChar '{' *> pure (L.IndentSome (pactIndent i) (\bloc -> mkIf condE (e, Block (NE.fromList bloc) ()) <$ singleChar '}') statement))

letStatement :: Parser (Expr ParsedName ())
letStatement = do
  _ <- keyword "let"
  v <- variable
  ty <- optional (singleChar ':' *> typeExpr)
  _ <- singleChar '='
  t <- expr
  pure $ Let (BN (BareName v)) ty t ()


singleChar :: Char -> Parser Text
singleChar c = lexeme (T.singleton <$> C.char c)

keyword :: Text -> Parser Text
keyword kw = lexeme (C.string kw)

lamStatement :: Parser (Expr ParsedName ())
lamStatement = do
  currIndent <- L.indentLevel
  lamStatement' currIndent
  where
  lamStatement' currIndent = L.indentBlock spaceConsumerNL $ do
    _ <- keyword "lambda"
    arg <- lamArg
    args <- many lamArg
    _ <- lexeme (C.string "=>")
    let mkLam e = Lam (BN (BareName "#")) (arg :| args) e ()
    (L.IndentNone . mkLam <$> expr) <|> (singleChar '{' *> pure (L.IndentSome (pactIndent currIndent) (\b -> mkLam (Block (NE.fromList b) ()) <$ singleChar '}') statement))
  bareVariable = BN . BareName <$> variable
  lamArg =
    typed <|> ((,Nothing) <$> bareVariable)
  typed = parens $ do
    v <- bareVariable
    _ <- singleChar ':'
    t <- typeExpr
    pure (v, Just t)

lamExpr :: Parser (Expr ParsedName ())
lamExpr = do
  _ <- keyword "lambda"
  arg <- lamArg
  args <- many lamArg
  _ <- lexeme (C.string "=>")
  t <- expr
  pure $ Lam (BN (BareName "#")) (arg :| args) t ()
  where
  bareVariable = BN . BareName <$> variable
  lamArg =
    typed <|> ((,Nothing) <$> bareVariable)
  typed = parens $ do
    v <- bareVariable
    _ <- singleChar ':'
    t <- typeExpr
    pure (v, Just t)

operatorTable :: [[Operator Parser (Expr ParsedName ())]]
operatorTable =
  [ [ prefix "~" FlipBitsOp
    , prefix "-" NegateOp]
  , [ binary "*" MultOp
    , binary "/" DivOp
    , binary "&" BitAndOp
    , binary "|" BitOrOp ]
  , [ binary "+" AddOp
    , binary "-" SubOp]
  , [ binary "==" EQOp
    , binary "!=" NEQOp
    , binary ">=" GEQOp
    , binary ">" GTOp
    , binary "<=" LEQOp
    , binary "<" LTOp]
  ]

binary :: Text -> BinaryOp -> Operator Parser (Expr ParsedName ())
binary name op = InfixL ((\a b -> BinaryOp op a b ()) <$ symbol name)

prefix :: Text -> UnaryOp -> Operator Parser (Expr ParsedName ())
prefix name op = Prefix (flip (UnaryOp op) () <$ symbol name)

varExpr :: Parser (Expr ParsedName ())
varExpr =
  flip Var () . BN . BareName <$> variable

constantExpr :: Parser (Expr name ())
constantExpr = fmap (flip Constant ()) $
  intLiteral
  <|> boolLiteral
  <|> unitLiteral
  <|> stringLiteral

expr' :: Parser (Expr ParsedName ())
expr' = do
  b <- atom
  applications b <|> pure b
  where
  applications b = do
    bs <- many app
    pure $ foldl' (\e apps -> App e apps ()) b bs
  app = parens $ (appWithArgs <|> pure [])
  appWithArgs = do
    arg1 <- expr
    args <- many (singleChar ',' *> expr)
    pure (arg1 : args)
  obj =
    between (singleChar '{') (singleChar '}') (objWithFields <|> emptyObject)
  objWithFields = do
    f1 <- objField
    rest <- many (singleChar ',' *> objField)
    let o = Object (Map.fromList (f1:rest)) ()
    pure o
  emptyObject = pure (Object mempty ())
  objField = do
    f <- variable
    _ <- singleChar ':'
    e <- expr
    pure (Field f, e)
  list = between (singleChar '[') (singleChar ']') (listElems <|> emptyList)
  listElems = do
    e <- expr
    es <- many (singleChar ',' *> expr)
    pure (List (e:es) ())
  emptyList =  pure (List [] ())
  atom = varExpr <|> constantExpr <|> parens expr <|> try obj <|> list

expr :: Parser (Expr ParsedName ())
expr = makeExprParser expr' operatorTable

_topLevel :: Parser (Expr ParsedName ())
_topLevel = L.nonIndented spaceConsumerNL (lamStatement <|> statement <|> expr)


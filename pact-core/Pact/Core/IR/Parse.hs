{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Pact.Core.IR.Parse where

import Data.Char(isAlphaNum)
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Data.Text(Text)
import qualified Data.Text as T
import Data.List.NonEmpty(NonEmpty(..))
import Data.Void(Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C

import Pact.Core.IR.ParseTree
import Pact.Core.Literal
import Pact.Core.Names

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

lexeme :: Parser Text -> Parser Text
lexeme = L.lexeme spaceConsumer

variable :: Parser Text
variable = do
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
intLiteral = LInteger <$> L.decimal

statement :: Parser (Expr ParsedName ())
statement =
  (letExpr <?> "let") <|>
  (lamExpr <?> "lam") <|>
  (term <?> "term")

typeExpr :: Parser ParsedType
typeExpr = undefined

block :: Parser (Expr ParsedName ())
block = undefined

letExpr :: Parser (Expr ParsedName ())
letExpr = do
  _ <- try (keyword "let")
  v <- lexeme variable
  ty <- optional (singleChar ':' *> typeExpr)
  _ <- singleChar '='
  t <- term
  pure $ Let (pure $ (BN (BareName v), ty)) t ()

singleChar :: Char -> Parser Text
singleChar c = lexeme (T.singleton <$> C.char c)

keyword :: Text -> Parser Text
keyword kw = lexeme (C.string kw)

lamExpr :: Parser (Expr ParsedName ())
lamExpr = do
  _ <- try (keyword "fn")
  arg <- lamArg
  args <- some lamArg
  _ <- lexeme (C.string "->")
  t <- block <|> term
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

term :: Parser (Expr ParsedName ())
term = undefined
  -- [ parens (operatorSection <|> expr)
  -- , lambdaExpr
  -- , appExpr ]

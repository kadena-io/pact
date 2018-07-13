{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Pact.Types.SExp
  ( SExp(..)
  , SExpParser(..)
  , BraceType(..)
  , TrailingSpace(..)
  , Token(..)
  , sexp
  , _Number, _String
  , number
  , puncText
  , sexps
  ) where

import           Control.Applicative
import           Control.Lens            (makePrisms)
import           Control.Monad
import           Data.Char               (digitToInt)
import           Data.Decimal            (Decimal, DecimalRaw (Decimal))
import           Data.Foldable           (asum, foldl')
import qualified Data.HashSet            as HashSet
import           Data.String             (fromString)
import           Data.Text               (Text)
import           Pact.Types.Parser       (style)
import           Text.Parser.Token.Style
import           Text.Trifecta           hiding (ident)


data BraceType = Curly | Paren | Square
  deriving (Eq, Show)

data TrailingSpace
  = TrailingSpace
  | NoTrailingSpace
  deriving (Eq, Show)

data SExp
  = List !BraceType ![Spanned SExp]
  | Token !Token
  deriving (Eq, Show)

data Token
  = Number      !(Either Integer Decimal)
  | Ident       !Text !TrailingSpace
  | Punctuation !Text !TrailingSpace
  | String      !Text
  deriving (Eq, Show)

newtype SExpParser p a = SExpParser { unSExpParser :: p a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, Parsing,
    CharParsing, DeltaParsing)

instance TokenParsing p => TokenParsing (SExpParser p) where
  someSpace   = SExpParser $
    buildSomeSpaceParser someSpace $ CommentStyle "" "" ";" False
  nesting     = SExpParser . nesting . unSExpParser
  semi        = token $ char ';' <?> ";"
  highlight h = SExpParser . highlight h . unSExpParser
  token p     = p <* whiteSpace

type SExpParsing a = forall m.
  (Monad m,TokenParsing m,CharParsing m,DeltaParsing m) => SExpParser m a

sexp :: SExpParsing (Spanned SExp)
sexp = spanned $ asum
  [ parens   (List Paren  <$!> many sexp)
  , braces   (List Curly  <$!> many sexp)
  , brackets (List Square <$!> many sexp)
  , try $ Token . Number <$!> number
  , Token          <$!> ident style
  , Token          <$!> punctuation
  , Token . String <$!> stringLiteral
  ]

sexps :: (TokenParsing m,CharParsing m,DeltaParsing m) => m [Spanned SExp]
sexps = unSExpParser $ whiteSpace *> some sexp <* eof

punctuation :: (TokenParsing m) => m Token
punctuation = Punctuation
  <$> (asum $ text <$!> ["'", ",", ".", ":=", ":"])
  <*> trailingSpace

number :: SExpParsing (Either Integer Decimal)
number = do
  -- Tricky: note that we use `char :: CharParsing m => Char -> m Char` rather
  -- than `symbolic :: TokenParsing m => Char -> m Char` here. We use the char
  -- parser because we want to disallow whitespace following the negative sign
  -- (token parsers apply `whiteSpace` after every token). With a whitespace we
  -- consider this an expression rather than a literal.
  neg <- maybe id (const negate) <$> optional (char '-')
  num <- some digit
  dec <- optional (dot *> some digit)
  _ <- whiteSpace
  let strToNum start = foldl' (\x d -> 10*x + toInteger (digitToInt d)) start
  pure $ case dec of
    Nothing -> Left $ neg $ strToNum 0 num
    Just d -> Right $ Decimal
      (fromIntegral (length d))
      (neg (strToNum (strToNum 0 num) d))

-- | Parse a non-reserved identifier or symbol
ident :: (TokenParsing m, Monad m) => IdentifierStyle m -> m Token
ident s = do
  iden <- fmap fromString $ try $ do
    name <- highlight (_styleHighlight s)
            ((:) <$> _styleStart s <*> many (_styleLetter s) <?> _styleName s)
    when (HashSet.member name (_styleReserved s)) $ unexpected $
      "reserved " ++ _styleName s ++ " " ++ show name
    return name
  Ident iden <$!> trailingSpace

trailingSpace :: (TokenParsing m) => m TrailingSpace
trailingSpace = do
  spc <- optional someSpace
  pure $ case spc of
    Just _  -> TrailingSpace
    Nothing -> NoTrailingSpace

puncText :: Token -> Maybe Text
puncText (Punctuation txt _) = Just txt
puncText _                   = Nothing

makePrisms ''Token

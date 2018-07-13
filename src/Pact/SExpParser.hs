{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Pact.SExpParser where

import           Control.Applicative
import           Control.Lens               (Prism', prism', (^?))
import           Data.Semigroup
import           Text.Trifecta              (Span (..), Spanned (..))
import           Text.Trifecta.Delta        (bytes)

import           Pact.Types.Lang
import           Pact.Types.SExp

import           Prelude                    hiding (span)


-- This is a processor of s-expressions. Basically a parser where the lexer is
-- aware of the structure of brackets, braces, and parens; and these
-- processors are deterministic and non-backtracking.
newtype SExpProcessor a = SExpProcessor
  { unP :: [Spanned SExp] -> Maybe (a, [Spanned SExp], Maybe Span) }
  deriving Functor

-- | The result of processing a sequence of s-expressions.
data ParseResult a
  = ParsedOne !(Spanned a) ![Spanned SExp]
  | NoParse
  deriving Show

runP :: SExpProcessor a -> [Spanned SExp] -> ParseResult a
runP (SExpProcessor p) sexps = case p sexps of
  Just (result, leftover,  Just span) -> ParsedOne (result :~ span) leftover
  Just (_, _,                Nothing) -> error "TODO"
  Nothing                             -> NoParse

instance Applicative SExpProcessor where
  pure a = SExpProcessor (\exps -> pure (a, exps, Nothing))
  {-# INLINE pure #-}
  SExpProcessor f <*> SExpProcessor a = SExpProcessor $ \input -> do
    (f', input, span1) <- f input
    (a', input, span2) <- a input
    pure (f' a', input, span1 <> span2)
  {-# INLINE (<*>) #-}

instance Alternative SExpProcessor where
  empty = SExpProcessor (const empty)
  {-# INLINE empty #-}
  SExpProcessor a <|> SExpProcessor b = SExpProcessor $
    \input -> a input <|> b input
  {-# INLINE (<|>) #-}

instance Monad SExpProcessor where
  SExpProcessor a >>= f = SExpProcessor $ \input -> do
    (a', input, s1) <- a input
    (b,  input, s2) <- unP (f a') input
    pure (b, input, s1 <> s2)
  {-# INLINE (>>=) #-}

token :: (Token -> Bool) -> SExpProcessor Token
token pTok = SExpProcessor $ \case
  (Token tok :~ span) : input
    | pTok tok -> pure (tok, input, Just span)
  _ -> empty

prism :: Prism' Token a -> SExpProcessor a
prism prism' = SExpProcessor $ \case
  (Token tok :~ span) : input
    | Just a <- tok ^? prism'
    -> pure (a, input, Just span)
  _ -> empty

list :: BraceType -> SExpProcessor [a] -> SExpProcessor [a]
list brace go = SExpProcessor $ \case
  (List brace' inner :~ span) : input
    | brace' == brace -> do
      (exps, inner', _mSpan) <- unP go inner
      case inner' of
        [] -> pure (exps, input, Just span)
        _  -> empty
  _ -> empty

ident :: Text -> SExpProcessor Token
ident txt = token $ \case
  Ident txt' _ | txt' == txt -> True
  _                          -> False

punctuation :: Text -> SExpProcessor Token
punctuation txt = token $ \case
  Punctuation txt' _ | txt' == txt -> True
  _                                -> False

punctuationNoTrailing :: Text -> SExpProcessor Token
punctuationNoTrailing txt = token $ \case
  Punctuation txt' NoTrailingSpace | txt' == txt -> True
  _                                              -> False

sepBy :: SExpProcessor a -> SExpProcessor b -> SExpProcessor [a]
sepBy pa pb =
  let sepBy' = pb *> sepBy pa pb <|> pure []
  in ((:) <$> pa <*> sepBy') <|> pure []

colon :: SExpProcessor Token
colon = punctuation ":"

comma :: SExpProcessor Token
comma = punctuation ","

spanToParsed :: Span -> Parsed
spanToParsed (Span start end _bs)
  = Parsed start $ fromIntegral $ bytes end - bytes start

liftParsed :: SExpProcessor (Parsed -> a) -> SExpProcessor a
liftParsed (SExpProcessor p) = SExpProcessor $ \input -> do
  (a', input, span) <- p input
  span' <- span
  pure (a' (spanToParsed span'), input, span)

_Ident :: Prism' Token Text
_Ident = prism'
  (\txt -> Ident txt NoTrailingSpace)
  (\case
    Ident txt NoTrailingSpace -> Just txt
    Ident txt TrailingSpace   -> Just txt
    _                         -> Nothing)

_IdentNoTrailing :: Prism' Token Text
_IdentNoTrailing = prism'
  (\txt -> Ident txt NoTrailingSpace)
  (\case
    Ident txt NoTrailingSpace -> Just txt
    _                         -> Nothing)

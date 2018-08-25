{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TupleSections        #-}
module Pact.SExpParser where

import           Control.Applicative
import           Data.Semigroup
import           Text.Trifecta              (Span (..), Spanned (..))

import           Pact.Types.Lang
import           Pact.Types.SExp

import           Prelude                    hiding (span)


-- This is a processor of s-expressions. Basically a parser where the lexer is
-- aware of the structure of brackets, braces, and parens; and these
-- processors are deterministic and non-backtracking.
newtype SExpProcessor a = SExpProcessor
  { unP :: [Spanned SExp] -> Maybe ([Spanned SExp], Maybe Span, a) }
  deriving Functor

-- | The result of processing a sequence of s-expressions.
data ParseResult a
  = ParsedOne !(Spanned a) ![Spanned SExp]
  | NoParse
  deriving Show

runP :: SExpProcessor a -> [Spanned SExp] -> ParseResult a
runP (SExpProcessor p) sexps = case p sexps of
  Just (leftover, Just span, result) -> ParsedOne (result :~ span) leftover
  Just (_, Nothing,               _) -> error "TODO"
  Nothing                            -> NoParse

runP' :: SExpProcessor a -> [Spanned SExp] -> Maybe (Spanned a)
runP' (SExpProcessor p) sexps = case p sexps of
  Just ([], Just span, result) -> Just (result :~ span)
  _                            -> Nothing

instance Applicative SExpProcessor where
  pure a = SExpProcessor $ pure . (,Nothing,a)
  {-# INLINE pure #-}
  SExpProcessor f <*> SExpProcessor a = SExpProcessor $ \input -> do
    (input, span1, f') <- f input
    (input, span2, a') <- a input
    pure (input, span1 <> span2, f' a')
  {-# INLINE (<*>) #-}

instance Alternative SExpProcessor where
  empty = SExpProcessor (const Nothing)
  {-# INLINE empty #-}
  SExpProcessor a <|> SExpProcessor b = SExpProcessor $
    \input -> a input <|> b input
  {-# INLINE (<|>) #-}

instance Monad SExpProcessor where
  SExpProcessor a >>= f = SExpProcessor $ \input -> do
    (input, span1, a') <- a input
    (input, span2, b)  <- unP (f a') input
    pure (input, span1 <> span2, b)
  {-# INLINE (>>=) #-}

sepBy :: SExpProcessor a -> SExpProcessor b -> SExpProcessor [a]
sepBy pa pb =
  let sepBy' = pb *> sepBy pa pb <|> pure []
  in ((:) <$> pa <*> sepBy') <|> pure []
{-# INLINE sepBy #-}

sepBy1 :: SExpProcessor a -> SExpProcessor b -> SExpProcessor [a]
sepBy1 pa pb = (:) <$> (pa <* pb) <*> sepBy pa pb
{-# INLINE sepBy1 #-}

punctuation :: Text -> SExpProcessor Text
punctuation txt = SExpProcessor $ \case
  (Token (Punctuation txt' _) :~ span) : input
    | txt' == txt -> pure (input, Just span, txt)
  _ -> empty
{-# INLINE punctuation #-}

colon :: SExpProcessor Text
colon = punctuation ":"
{-# INLINE colon #-}

comma :: SExpProcessor Text
comma = punctuation ","
{-# INLINE comma #-}

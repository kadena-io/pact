{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TupleSections        #-}
module Pact.SExpParser where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except
-- import           Data.Functor.Identity
import           Data.Semigroup
import           Text.Trifecta              (Span (..), Spanned (..))

import           Pact.Types.Lang
import           Pact.Types.SExp

import           Prelude                    hiding (span)


-- This is a processor of s-expressions. Basically a parser where the lexer is
-- aware of the structure of brackets, braces, and parens; and these
-- processors are deterministic and non-backtracking.
newtype SExpProcessorT m a = SExpProcessor
  { unP :: [Spanned SExp] -> m ([Spanned SExp], Maybe Span, a) }
  deriving Functor

-- type SExpProcessor = SExpProcessorT Identity

-- | The result of processing a sequence of s-expressions.
data ParseResult a
  = ParsedOne !(Spanned a) ![Spanned SExp]
  | NoParse
  deriving Show

-- These instances need UndecidableInstances because they don't satisfy the
-- coverage condition (like mtl instances)
instance MonadTrans SExpProcessorT where
  lift ma = SExpProcessor $ \input -> (input, Nothing, ) <$> ma

instance MonadReader a m => MonadReader a (SExpProcessorT m) where
  ask    = lift ask
  reader = lift . reader
  local f (SExpProcessor ma) = SExpProcessor $ \input ->
    local f $ ma input

instance MonadState  a m => MonadState  a (SExpProcessorT m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance MonadError  a m => MonadError  a (SExpProcessorT m) where
  throwError = lift . throwError
  catchError ma handler = SExpProcessor $ \input ->
    unP ma input `catchError` ((`unP` input) . handler)

-- runP :: Monad m => SExpProcessorT m a -> [Spanned SExp] -> m (ParseResult a)
-- runP (SExpProcessor p) sexps = do
--   result <- runMaybeT $ p sexps
--   pure $ case result of
--     Just (leftover, Just span, result) -> ParsedOne (result :~ span) leftover
--     Just (_, Nothing,               _) -> error "TODO"
--     Nothing                            -> NoParse

instance Monad m => Applicative (SExpProcessorT m) where
  pure a = SExpProcessor $ pure . (,Nothing,a)
  {-# INLINE pure #-}
  SExpProcessor f <*> SExpProcessor a = SExpProcessor $ \input -> do
    (input, span1, f') <- f input
    (input, span2, a') <- a input
    pure (input, span1 <> span2, f' a')
  {-# INLINE (<*>) #-}

instance (Alternative m, Monad m) => Alternative (SExpProcessorT m) where
  empty = SExpProcessor $ const empty
  {-# INLINE empty #-}
  SExpProcessor a <|> SExpProcessor b = SExpProcessor $
    \input -> a input <|> b input
  {-# INLINE (<|>) #-}

instance Monad m => Monad (SExpProcessorT m) where
  SExpProcessor a >>= f = SExpProcessor $ \input -> do
    (input, span1, a') <- a input
    (input, span2, b)  <- unP (f a') input
    pure (input, span1 <> span2, b)
  {-# INLINE (>>=) #-}

sepBy
  :: (Alternative m, Monad m)
  => SExpProcessorT m a -> SExpProcessorT m b -> SExpProcessorT m [a]
sepBy pa pb =
  let sepBy' = pb *> sepBy pa pb <|> pure []
  in ((:) <$> pa <*> sepBy') <|> pure []
{-# INLINE sepBy #-}

sepBy1
  :: (Alternative m, Monad m)
  => SExpProcessorT m a -> SExpProcessorT m b -> SExpProcessorT m [a]
sepBy1 pa pb = (:) <$> (pa <* pb) <*> sepBy pa pb
{-# INLINE sepBy1 #-}

punctuation :: (Alternative m, Monad m) => Text -> SExpProcessorT m Text
punctuation txt = SExpProcessor $ \case
  (Token (Punctuation txt' _) :~ span) : input
    | txt' == txt -> pure (input, Just span, txt)
  _ -> empty
{-# INLINE punctuation #-}

colon :: (Alternative m, Monad m) => SExpProcessorT m Text
colon = punctuation ":"
{-# INLINE colon #-}

comma :: (Alternative m, Monad m) => SExpProcessorT m Text
comma = punctuation ","
{-# INLINE comma #-}

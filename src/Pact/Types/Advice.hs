{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Pact.Types.Advice
-- Copyright   :  (C) 2020 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Advise user functions in runtime for performance, coverage etc.
--

module Pact.Types.Advice
  ( Advice(..)
  , AdviceContext(..)
  , advise
  , DbContext(..)
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)
import Pact.Types.PactError
import Pact.Types.Hash

-- | Enum indicating which Db operation is running.
data DbContext =
    DbRead
  | DbWrite
  | DbKeys
  | DbTxIds
  | DbCreateUserTable
  | DbGetUserTableInfo
  | DbBeginTx
  | DbCommitTx
  | DbRollbackTx
  | DbGetTxLog
  deriving (Eq,Show,Enum,Bounded)

-- | Type of advised operation.
data AdviceContext =
    AdviceCall StackFrame
    -- ^ Advise on some interpreter call.
  | AdviceTx PactHash
    -- ^ Transaction execution wrapper
  | AdviceDb DbContext
    -- ^ Db operation
  | AdviceOther Text
    -- ^ Arbitrary advice.
  deriving (Eq,Show)


-- | Bracket some Pact operation.
newtype Advice = Advice {
  _advise :: forall m a . MonadIO m
    => AdviceContext
    -- ^ Context for advice.
    -> m a
    -- ^ Bracketed action.
    -> m a
    -- ^ Result of bracketed action.
  }

instance Default Advice where def = Advice defAdvice
instance Show Advice where show _ = "Advice"
instance Semigroup Advice where
  Advice f <> Advice g =
    Advice $ \ctx act -> f ctx $! g ctx act
instance Monoid Advice where
  mempty = def

advise :: MonadIO m => Advice -> AdviceContext -> m a -> m a
advise (Advice f) ctx act = f ctx act

defAdvice :: AdviceContext -> m a -> m a
defAdvice _ a = a
{-# INLINE defAdvice #-}

makePrisms ''DbContext
makePrisms ''AdviceContext

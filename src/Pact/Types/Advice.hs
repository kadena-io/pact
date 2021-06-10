{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Pact.Types.Advice
-- Copyright   :  (C) 2021 Stuart Popejoy
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

import Bound
import Control.Lens
import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)

import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.Term

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
    AdviceUser (Def Ref,[Term Name])
    -- ^ Advise on user function
  | AdviceNative (NativeDefName)
    -- ^ Advise on native
  | AdviceTx PactHash
    -- ^ Transaction execution wrapper
  | AdviceDb DbContext
    -- ^ Db operation
  | AdviceModule (ModuleDef (Term Name),Scope () Term Name)
    -- ^ Module or interface install/upgrade.
  | AdviceOther Text
    -- ^ Arbitrary advice.
  deriving (Eq,Show)


-- | Bracket some Pact operation.
newtype Advice = Advice {
  _advise :: forall m a . MonadIO m
    => Info
    -> AdviceContext
    -> m a
    -> m a
  }

instance Default Advice where def = Advice defAdvice
instance Show Advice where show _ = "Advice"
instance Semigroup Advice where
  Advice f <> Advice g =
    Advice $ \i ctx act -> f i ctx $! g i ctx act
instance Monoid Advice where
  mempty = def

advise :: MonadIO m => Info -> Advice -> AdviceContext -> m a -> m a
advise i (Advice f) ctx act = f i ctx act

defAdvice :: Info -> AdviceContext -> m a -> m a
defAdvice _ _ a = a
{-# INLINE defAdvice #-}

makePrisms ''DbContext
makePrisms ''AdviceContext

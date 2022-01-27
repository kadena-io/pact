{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
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
  , advisePactDb
  ) where

import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)

import Pact.Types.Hash
import Pact.Types.Info
import Pact.Types.Persistence
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

-- | Type of advised operation. GADT indicates the "intermediate return value" type of
-- the bracketed operation.
data AdviceContext r where
    -- | Advise on user function, return result
    AdviceUser :: !(Def Ref,[Term Name]) -> AdviceContext (Term Name)
    -- | Advise on native, return result
    AdviceNative :: !NativeDefName -> AdviceContext (Term Name)
    -- | Transaction execution wrapper
    AdviceTx :: !PactHash -> AdviceContext ()
    -- | Db operation
    AdviceDb :: !DbContext -> AdviceContext ()
    -- | Module or interface install/upgrade, returns loaded module data.
    AdviceModule :: !(ModuleDef (Term Name)) -> AdviceContext (ModuleData Ref)
    -- | Arbitrary advice.
    AdviceOther :: !Text -> AdviceContext Text

instance Show (AdviceContext r) where
  show AdviceUser {} = "AdviceUser"
  show AdviceNative {} = "AdviceNative"
  show AdviceTx {} = "AdviceTx"
  show AdviceDb {} = "AdviceDb"
  show AdviceModule {} = "AdviceModule"
  show AdviceOther {} = "AdviceOther"


-- | Bracket some Pact operation.
newtype Advice = Advice {
  -- | Callback for advising a pact operation. '_advise i ctx op'
  -- gives source info in 'i', advice context in 'ctx', and 'op'
  -- as the bracketed action. The implementation is expected to
  -- execute 'op' whose return value '(r,a)' contains the ultimate
  -- return value in 'a', with 'r' available for inspection.
  _advise :: forall m r a . MonadIO m
    => Info
    -> AdviceContext r
    -> m (r, a)
    -> m a
  }

instance Default Advice where def = Advice defAdvice
instance Show Advice where show _ = "Advice"
instance Semigroup Advice where
  Advice f <> Advice g =
    Advice $ \i ctx act -> f i ctx $! g i ctx $ do
      (r,a) <- act
      return (r,(r,a))
instance Monoid Advice where
  mempty = def

advise :: MonadIO m => Info -> Advice -> AdviceContext r -> m (r,a) -> m a
advise i (Advice f) ctx act = f i ctx act

defAdvice :: MonadIO m => Info -> AdviceContext r -> m (r,a) -> m a
defAdvice _ _ a = snd <$> a
{-# INLINE defAdvice #-}

-- | Instrument some 'PactDb' with advice.
advisePactDb :: Advice -> PactDb a -> PactDb a
advisePactDb pt PactDb{..} = PactDb
  { _readRow = \d k e -> perf' DbRead $ _readRow d k e
  , _writeRow = \w d k v e -> perf' DbWrite $ _writeRow w d k v e
  , _keys = \d e -> perf' DbKeys $ _keys d e
  , _txids = \t i e -> perf' DbTxIds $ _txids t i e
  , _createUserTable = \t m e -> perf' DbCreateUserTable $ _createUserTable t m e
  , _getUserTableInfo = \t e -> perf' DbGetUserTableInfo $ _getUserTableInfo t e
  , _beginTx = \m e -> perf' DbBeginTx $ _beginTx m e
  , _commitTx = \e -> perf' DbCommitTx $ _commitTx e
  , _rollbackTx = \e -> perf' DbRollbackTx $ _rollbackTx e
  , _getTxLog = \d i e -> perf' DbGetTxLog $ _getTxLog d i e
  }
  where
    perf' :: DbContext -> IO a -> IO a
    perf' t act = advise def pt (AdviceDb t) $ ((),) <$> act

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  | DbRowSize
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

-- | For an advised operation, provide pre-execution "context" and
-- specify post-execution return type.
data AdviceContext r where
    -- | Advise on user function, return result
    AdviceUser :: !(Def Ref) -> AdviceContext (Term Name)
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
  -- | Callback for advising some pact operation. '_advise i ctx'
  -- is invoked before the operation with location info in 'i' and
  -- advice context in 'ctx', and returns a continuation for
  -- processing the operation return type post-execution.
  _advise :: forall m r . MonadIO m
    => Info
    -> AdviceContext r
    -> m (r -> m ())
  }

instance Default Advice where def = Advice defAdvice
instance Show Advice where show _ = "Advice"
instance Semigroup Advice where
  Advice f <> Advice g =
    Advice $ \i ctx -> do
      c <- f i ctx
      d <- g i ctx
      pure $ \r -> c r >> d r

instance Monoid Advice where
  mempty = def

advise :: MonadIO m => Info -> Advice -> AdviceContext r -> m (r -> m ())
advise i (Advice f) ctx = f i ctx

defAdvice :: MonadIO m => Info -> AdviceContext r -> m (r -> m ())
defAdvice _ _ = pure $ const $ return ()
{-# INLINE defAdvice #-}

-- | Instrument some 'PactDb' with advice.
advisePactDb :: Advice -> PactDb a -> PactDb a
advisePactDb pt PactDb{..} = PactDb
  { _readRow = \d k e -> perf' DbRead $ _readRow d k e
  , _sizeRow = \d k e -> perf' DbRowSize $ _sizeRow d k e
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
    perf' t act = do
      c <- advise def pt (AdviceDb t)
      !r <- act
      c ()
      pure r

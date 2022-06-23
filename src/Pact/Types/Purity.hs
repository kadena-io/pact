{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      :  Pact.Types.Purity
-- Copyright   :  (C) 2016,2020 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Purity environments, to prevent db access as specified in
-- different natives.
--

module Pact.Types.Purity
  ( PureSysOnly
  , PureReadOnly
  , runSysOnly
  , runReadOnly
  ) where


import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson hiding (Object)
import Data.Default
import Data.String
import Data.Text (Text)

import Pact.Types.Orphans ()
import Pact.Types.PactError
import Pact.Types.Persistence
import Pact.Types.Pretty
import Pact.Types.Runtime



-- | Marker class for 'PSysOnly' environments.
class PureSysOnly e
-- | Marker class for 'PReadOnly' environments.
class PureReadOnly e

-- | Wraps an 'EvalEnv e' to use delegate
-- 'PactDb' that prevents illegal reads.
-- "p" type is phantom for indicating typeclass.
newtype PureEnv p e = PureEnv (EvalEnv e)

-- | Phantom type for indicating PureSysOnly
data PESysOnly
instance PureSysOnly (PureEnv PESysOnly e)

-- | Phantom type for indicating PureReadOnly
data PEReadOnly
instance PureReadOnly (PureEnv PEReadOnly e)


disallowed :: Text -> Method e a
disallowed opName _ = throwM $ PactError EvalError def def $ "Illegal database access attempt (" <> pretty opName <> ")"

-- | Construct a delegate pure eval environment.
mkPureEnv :: (EvalEnv e -> f) -> Purity ->
             (forall k v . (IsString k,FromJSON v) =>
              Domain k v -> k -> Method f (Maybe v)) ->
             EvalEnv e -> Eval e (EvalEnv f)
mkPureEnv holder purity readRowImpl env@EvalEnv{..} = do
  v <- liftIO $ newMVar (holder env)
  return $ EvalEnv
    _eeRefStore
    _eeMsgSigs
    _eeMsgBody
    _eeMode
    _eeEntity
    _eePactStep
    v
    PactDb {
      _readRow = readRowImpl
    , _writeRow = \_ _ _ _ -> disallowed "writeRow"
    , _keys = const (disallowed "keys")
    , _txids = \_ _ -> (disallowed "txids")
    , _createUserTable = \_ _ -> disallowed "createUserTable"
    , _getUserTableInfo = const (disallowed "getUserTableInfo")
    , _beginTx = const (disallowed "beginTx")
    , _commitTx = disallowed "commitTx"
    , _rollbackTx = disallowed  "rollbackTx"
    , _getTxLog = \_ _ -> disallowed "getTxLog"
    }
    purity
    _eeHash
    _eeGasEnv
    permissiveNamespacePolicy
    _eeSPVSupport
    _eePublicData
    _eeExecutionConfig
    _eeAdvice
    _eeInRepl

-- | Operationally creates the sysread-only environment.
-- Phantom type and typeclass assigned in "runXXX" functions.
mkSysOnlyEnv :: EvalEnv e -> Eval e (EvalEnv (PureEnv p e))
mkSysOnlyEnv = mkPureEnv PureEnv PSysOnly (\(dom :: Domain key v) key ->
  let read' :: forall f e'. MVar (PureEnv f e') -> IO (Maybe v)
      read' e = withMVar e $ \(PureEnv EvalEnv {..}) ->
                  _readRow _eePactDb dom key _eePactDbVar
  in case dom of
       UserTables _ -> disallowed "readRow"
       KeySets -> read'
       Modules -> read'
       Namespaces -> read'
       Pacts -> read')


-- | Operationally creates a read-only environment.
-- Phantom type and typeclass assigned in 'runReadOnly'
mkReadOnlyEnv :: EvalEnv e -> Eval e (EvalEnv (PureEnv p e))
mkReadOnlyEnv = mkPureEnv PureEnv PReadOnly $ \d k e ->
  withMVar e $ \(PureEnv EvalEnv {..}) -> _readRow _eePactDb d k _eePactDbVar


-- | Run monad in sysread-only environment.
runSysOnly :: Eval (PureEnv PESysOnly e) a -> Eval e a
runSysOnly action = ask >>= \env -> mkSysOnlyEnv env >>= runWithEnv action

-- | Run monad in read-only environment, unless calling environment is sysread,
-- in which case preserve sysread.
-- Flag controls old behavior which
-- would cause error in sysread environment; now environment is just
-- constricted appropriately.
runReadOnly :: HasInfo i => i -> Eval (PureEnv PEReadOnly e) a -> Eval e a
runReadOnly i action = ask >>= \env -> case _eePurity env of
  PSysOnly -> do
    f <- isExecutionFlagSet FlagOldReadOnlyBehavior
    if not f then
      mkSysOnlyEnv env >>= runWithEnv action
    else
      evalError' i "internal error: attempting db read in sys-only context"
  _ -> mkReadOnlyEnv env >>= runWithEnv action

-- | Re-wrap Eval monad with different environment.
runWithEnv :: Eval f b -> EvalEnv f -> Eval e b
runWithEnv action pureEnv = do
  s <- get
  (o,s') <- liftIO $ runEval' s pureEnv action
  put s'
  either throwM return o

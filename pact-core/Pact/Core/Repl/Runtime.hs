{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}


module Pact.Core.Repl.Runtime where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Catch
import Data.Default
import Data.IORef
import Data.Text(Text)

import Pact.Core.Gas
import Pact.Core.Errors

import Pact.Core.Untyped.Term
import Pact.Core.Untyped.Eval.Runtime
import Pact.Core.Untyped.Eval.CEK

data ReplEvalEnv b i
  = ReplEvalEnv
  { _emGas :: IORef Gas
  , _emGasLog :: IORef (Maybe [(Text, Gas)])
  }

newtype ReplEvalState b i
  = ReplEvalState
  { _reEnv :: CEKRuntimeEnv b i (ReplEvalM b i)
  }

-- Todo: are we going to inject state as the reader monad here?
newtype ReplEvalM b i a =
  ReplEvalM (ExceptT (PactError i) (StateT (ReplEvalState b i) (ReaderT (ReplEvalEnv b i) IO)) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ReplEvalEnv b i)
    , MonadState (ReplEvalState b i)
    , MonadError (PactError i)
    , MonadIO
    , MonadThrow
    , MonadCatch)
  via (ExceptT (PactError i) (StateT (ReplEvalState b i) (ReaderT (ReplEvalEnv b i) IO)))

makeLenses ''ReplEvalEnv
makeLenses ''ReplEvalState

instance MonadCEKEnv b i (ReplEvalM b i) where
  cekReadEnv = use reEnv
  cekLogGas msg g = do
    r <- view emGasLog
    liftIO $ modifyIORef' r (fmap ((msg, g):))
  cekChargeGas g = do
    r <- view emGas
    liftIO (modifyIORef' r (+ g))

runReplEvalM
  :: ReplEvalEnv b i
  -> ReplEvalState b i
  -> ReplEvalM b i a
  -> IO (Either (PactError i) a)
runReplEvalM env st (ReplEvalM action) = runReaderT (evalStateT (runExceptT action) st) env

runReplCEK
  :: (Default i)
  => ReplEvalEnv b i
  -> ReplEvalState b i
  -> EvalTerm b i
  -> IO (Either (PactError i) (CEKValue b i (ReplEvalM b i)))
runReplCEK env st term =
  runReplEvalM env st (eval mempty term)

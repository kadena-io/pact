{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      :  Pact.Repl.Lib
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Built-ins for repl functionality. Not part of standard library
-- for blockchain execution, but instead for testing and dev.
--

module Pact.Repl.Lib where

import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Except (catchError)
import Control.Monad.Catch
import Control.Monad.State.Strict (get)
import Control.Lens
import qualified Data.Set as S
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Control.Concurrent.MVar
import Data.Aeson (eitherDecode,toJSON)
import Data.Monoid
#if !defined(ghcjs_HOST_OS)
import Criterion
import Criterion.Types
import Statistics.Resampling.Bootstrap
#endif
import Pact.Native.Internal
import Pact.Types
import Pact.Eval
import Pact.Pure




data LibOp =
    Noop |
    UpdateEnv (Endo (EvalEnv LibState)) |
    Load FilePath
instance Default LibOp where def = Noop
data Tx = Begin|Commit|Rollback deriving (Eq,Show,Bounded,Enum,Ord)

data LibState = LibState {
      _rlsPure :: MVar PureState
    , _rlsOp :: LibOp
    , _rlsTxName :: Maybe String
}
makeLenses ''LibState

initLibState :: IO LibState
initLibState = newMVar def >>= \m -> return (LibState m Noop def)

replDefsMap :: Eval LibState (HM.HashMap String Ref)
replDefsMap = HM.map Direct . HM.fromList <$> replDefs

replDefs :: Eval LibState NativeDef
replDefs = foldDefs
     [
      defRNative "load" load [funType TyString [("file",TyString)]]
      "Load and evaluate FILE. `$(load \"accounts.repl\")`"

     ,defRNative "env-keys" setsigs [funType TyString [("keys",TyList (Just TyString))]]
      "Set transaction signature KEYS. `(env-keys [\"my-key\" \"admin-key\"])`"
     ,defRNative "env-data" setmsg [funType TyString [("json",json)]] $
      "Set transaction JSON data, either as encoded string, or as pact types coerced to JSON. " ++
      "`(env-data { \"keyset\": { \"keys\": [\"my-key\" \"admin-key\"], \"pred\": \"keys-any\" } })`"
     ,defRNative "env-step" setstep [funType TyString [],
                                     funType TyString [("step-idx",TyInteger)],
                                     funType TyString [("step-idx",TyInteger),("rollback",TyBool)]]
      ("Modify pact step state. With no arguments, unset step. STEP-IDX sets step index for " ++
       "current pact execution, ROLLBACK defaults to false. `$(env-step 1)` `$(env-step 0 true)`")
     ,defRNative "env-entity" setentity [funType TyString [("entity",TyString)]]
      "Set environment confidential ENTITY id. `$(env-entity \"my-org\")`"
     ,defRNative "begin-tx" (tx Begin) [funType TyString [],
                                        funType TyString [("name",TyString)]]
       "Begin transaction with optional NAME. `$(begin-tx \"load module\")`"
     ,defRNative "commit-tx" (tx Commit) [funType TyString []] "Commit transaction. `$(commit-tx)`"
     ,defRNative "rollback-tx" (tx Rollback) [funType TyString []] "Rollback transaction. `$(rollback-tx)`"
     ,defRNative "expect" expect [funType TyString [("doc",TyString),("expected",a),("actual",a)]]
      "Evaluate ACTUAL and verify that it equals EXPECTED. `(expect \"Sanity prevails.\" 4 (+ 2 2))`"
     ,defNative "expect-failure" expectFail [funType TyString [("doc",TyString),("exp",a)]] $
      "Evaluate EXP and succeed only if it throws an error. " ++
      "`(expect-failure \"Enforce fails on false\" (enforce false \"Expected error\"))`"
     ,defNative "bench" bench' [funType TyString [("exprs",TyRest)]]
      "Benchmark execution of EXPRS. `$(bench (+ 1 2))`"
     ]
     where
       json = TyVar "a" [TyInteger,TyString,TyTime,TyDecimal,TyBool,TyList Nothing,TyObject Nothing,TyKeySet,TyValue]
       a = TyVar "a" []

invokeEnv :: (MVar PureState -> IO b) -> MVar LibState -> IO b
invokeEnv f e = withMVar e $ \ls -> f $! _rlsPure ls
{-# INLINE invokeEnv #-}

repldb :: PactDb LibState
repldb = PactDb {

    _readRow = \d k -> invokeEnv $ _readRow puredb d k
  , _writeRow = \wt d k v -> invokeEnv $ _writeRow puredb wt d k v
  , _keys = \t -> invokeEnv $ _keys puredb t
  , _txids = \t tid -> invokeEnv $ _txids puredb t tid
  , _createUserTable = \t m k -> invokeEnv $ _createUserTable puredb t m k
  , _getUserTableInfo = \t -> invokeEnv $ _getUserTableInfo puredb t
  , _beginTx = invokeEnv $ _beginTx puredb
  , _commitTx = \tid -> invokeEnv $ _commitTx puredb tid
  , _rollbackTx = invokeEnv $ _rollbackTx puredb
  , _getTxLog = \d t -> invokeEnv $ _getTxLog puredb d t

}


load :: RNativeFun LibState
load _ [TLitString fn] = setop (Load fn) >> return (tStr $ "Loading " ++ fn ++ "...")
load i as = argsError i as


setop :: LibOp -> Eval LibState ()
setop v = do
  e <- ask
  liftIO $ modifyMVar_ (_eePactDbVar e) (return . set rlsOp v)

setenv :: Show a => Setter' (EvalEnv LibState) a -> a -> Eval LibState ()
setenv l v = setop $ UpdateEnv $ Endo (set l v)


setsigs :: RNativeFun LibState
setsigs i ts = do
  ks <- forM ts $ \t -> case t of
          (TLitString s) -> return s
          _ -> argsError i ts
  setenv eeMsgSigs (S.fromList (map (PublicKey . BS.fromString) ks))
  return $ tStr "Setting transaction keys"

setmsg :: RNativeFun LibState
setmsg i [TLitString j] =
  case eitherDecode (BSL.fromString j) of
    Left f -> evalError' i ("Invalid JSON: " ++ show f)
    Right v -> setenv eeMsgBody v >> return (tStr "Setting transaction data")
setmsg _ [a] = setenv eeMsgBody (toJSON a) >> return (tStr "Setting transaction data")
setmsg i as = argsError i as


setstep :: RNativeFun LibState
setstep _ [] = setstep' Nothing >> return (tStr "Un-setting step")
setstep _ [TLitInteger j] = do
  setstep' (Just $ PactStep (fromIntegral j) False def)
  return $ tStr "Setting step"
setstep _ [TLitInteger j,TLiteral (LBool b) _] = do
  setstep' (Just $ PactStep (fromIntegral j) b def)
  return $ tStr "Setting step and rollback"
setstep i as = argsError i as

setstep' :: Maybe PactStep -> Eval LibState ()
setstep' s = do
  setenv eePactStep s
  evalYield .= Nothing

setentity :: RNativeFun LibState
setentity _ [TLitString s] = setenv eeEntity s >> return (tStr "Setting entity")
setentity i as = argsError i as

txmsg :: Maybe String -> TxId -> String -> Term Name
txmsg n tid s = tStr $ s ++ " Tx " ++ show tid ++ maybe "" (": " ++) n


tx :: Tx -> RNativeFun LibState
tx Begin i as = do
  tname <- case as of
             [TLitString n] -> return $ Just n
             [] -> return Nothing
             _ -> argsError i as
  tid <- succ <$> view eeTxId
  setenv eeTxId tid
  evalBeginTx
  view eePactDbVar >>= \m -> liftIO $ modifyMVar_ m (return . set rlsTxName tname)
  return $ txmsg tname tid "Begin"

tx Rollback _ [] = do
  evalRollbackTx
  tid <- view eeTxId
  tname <- view eePactDbVar >>= \m -> liftIO $ withMVar m (return . view rlsTxName)
  return $ txmsg tname tid "Rollback"
tx Commit _ [] = do
  newmods <- use (evalRefs.rsNew)
  setop $ UpdateEnv $ Endo (over (eeRefStore.rsModules) (HM.union (HM.fromList newmods)))
  evalCommitTx
  tid <- view eeTxId
  tname <- view eePactDbVar >>= \m -> liftIO $ modifyMVar m $ \v -> return (set rlsTxName Nothing v,view rlsTxName v)
  return $ txmsg tname tid "Commit"
tx _ i as = argsError i as

expect :: RNativeFun LibState
expect i [TLitString a,b,c] =
  if b `termEq` c
  then return $ tStr $ "Expect: success: " ++ a
  else evalError' i $ "FAILURE: " ++ a ++ ": expected " ++ show b ++ ", received " ++ show c
expect i as = argsError i as

expectFail :: NativeFun LibState
expectFail i as@[a,b] = do
  a' <- reduce a
  case a' of
    TLitString msg -> do
      r <- catchError (Right <$> reduce b) (return . Left)
      case r of
        Right v -> evalError' i $ "FAILURE: " ++ msg ++ ": expected failure, got result = " ++ show v
        Left _ -> return $ tStr $ "Expect failure: success: " ++ msg
    _ -> argsError' i as
expectFail i as = argsError' i as

bench' :: NativeFun LibState
#if !defined(ghcjs_HOST_OS)
bench' i as = do
  e <- ask
  s <- get
  (r :: Either SomeException Report) <-
      try $ liftIO $ benchmark' $ whnfIO $ runEval s e $ do
                !ts <- mapM reduce as
                return $! toTerm (length ts)
  case r of
    Left ex -> evalError' i (show ex)
    Right rpt -> do
           let mean = estPoint (anMean (reportAnalysis rpt))
               sd = estPoint (anStdDev (reportAnalysis rpt))
               (reg,_) = splitAt 1 $ anRegress (reportAnalysis rpt)
               val = case reg of
                       [] -> mean
                       (r':_) -> case M.lookup "iters" (regCoeffs r') of
                                  Nothing -> mean
                                  Just t -> estPoint t
               tps = 1/val
               tperr = (1/(val - (sd/2))) - (1/(val + (sd/2)))
           liftIO $ putStrLn $ show (round tps :: Integer) ++ "/s, +-" ++ show (round tperr :: Integer) ++ "/s"
           return (tStr "Done")
#else
bench' i _ = evalError' i "Benchmarking not supported in GHCJS"
#endif

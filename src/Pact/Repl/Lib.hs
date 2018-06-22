{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

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

import Control.Arrow ((&&&))
import Data.Default
import Data.Semigroup
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.State.Strict (get)
import Control.Lens
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent.MVar
import Data.Aeson (eitherDecode,toJSON)
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Maybe
#if !defined(ghcjs_HOST_OS)
import Criterion
import Criterion.Types
import Pact.Analyze.Check
#if MIN_VERSION_statistics(0,14,0)
import Statistics.Types (Estimate(..))
#else
import Statistics.Resampling.Bootstrap
#endif
#endif
import Pact.Typechecker
import Pact.Types.Typecheck

import Pact.Native.Internal
import Pact.Types.Runtime
import Pact.Eval
import Pact.Persist.Pure
import Pact.PersistPactDb
import Pact.Types.Logger
import Pact.Repl.Types


initLibState :: Loggers -> IO LibState
initLibState loggers = do
  m <- newMVar (DbEnv def persister
                (newLogger loggers "Repl")
                def def)
  createSchema m
  return (LibState m Noop def def)

replDefs :: NativeModule
replDefs = ("Repl",
     [
      defRNative "load" load (funType tTyString [("file",tTyString)] <>
                              funType tTyString [("file",tTyString),("reset",tTyBool)]) $
      "Load and evaluate FILE, resetting repl state beforehand if optional NO-RESET is true. " <>
      "`$(load \"accounts.repl\")`"
     ,defRNative "env-keys" setsigs (funType tTyString [("keys",TyList tTyString)])
      "Set transaction signature KEYS. `(env-keys [\"my-key\" \"admin-key\"])`"
     ,defRNative "env-data" setmsg (funType tTyString [("json",json)]) $
      "Set transaction JSON data, either as encoded string, or as pact types coerced to JSON. " <>
      "`(env-data { \"keyset\": { \"keys\": [\"my-key\" \"admin-key\"], \"pred\": \"keys-any\" } })`"
     ,defRNative "env-step"
      setstep (funType tTyString [] <>
               funType tTyString [("step-idx",tTyInteger)] <>
               funType tTyString [("step-idx",tTyInteger),("rollback",tTyBool)] <>
               funType tTyString [("step-idx",tTyInteger),("rollback",tTyBool),("resume",TySchema TyObject (mkSchemaVar "y"))])
      ("Set pact step state. With no arguments, unset step. With STEP-IDX, set step index to execute. " <>
       "ROLLBACK instructs to execute rollback expression, if any. RESUME sets a value to be read via 'resume'." <>
       "Clears any previous pact execution state. `$(env-step 1)` `$(env-step 0 true)`")
     ,defRNative "pact-state" pactState (funType (tTyObject TyAny) [])
      ("Inspect state from previous pact execution. Returns object with fields " <>
      "'yield': yield result or 'false' if none; 'step': executed step; " <>
      "'executed': indicates if step was skipped because entity did not match. `$(pact-state)`")
     ,defRNative "env-entity" setentity
      (funType tTyString [] <> funType tTyString [("entity",tTyString)])
      ("Set environment confidential ENTITY id, or unset with no argument. " <>
      "Clears any previous pact execution state. `$(env-entity \"my-org\")` `$(env-entity)`")
     ,defRNative "begin-tx" (tx Begin) (funType tTyString [] <>
                                        funType tTyString [("name",tTyString)])
       "Begin transaction with optional NAME. `$(begin-tx \"load module\")`"
     ,defRNative "commit-tx" (tx Commit) (funType tTyString []) "Commit transaction. `$(commit-tx)`"
     ,defRNative "rollback-tx" (tx Rollback) (funType tTyString []) "Rollback transaction. `$(rollback-tx)`"
     ,defRNative "expect" expect (funType tTyString [("doc",tTyString),("expected",a),("actual",a)])
      "Evaluate ACTUAL and verify that it equals EXPECTED. `(expect \"Sanity prevails.\" 4 (+ 2 2))`"
     ,defNative "expect-failure" expectFail (funType tTyString [("doc",tTyString),("exp",a)]) $
      "Evaluate EXP and succeed only if it throws an error. " <>
      "`(expect-failure \"Enforce fails on false\" (enforce false \"Expected error\"))`"
     ,defNative "bench" bench' (funType tTyString [("exprs",TyAny)])
      "Benchmark execution of EXPRS. `$(bench (+ 1 2))`"
     ,defRNative "typecheck" tc (funType tTyString [("module",tTyString)] <>
                                 funType tTyString [("module",tTyString),("debug",tTyBool)])
       "Typecheck MODULE, optionally enabling DEBUG output."

#if !defined(ghcjs_HOST_OS)
     ,defRNative "verify" verify (funType tTyString [("module",tTyString)]) "Verify MODULE, checking that all properties hold."
#endif

     ,defRNative "json" json' (funType tTyValue [("exp",a)]) $
      "Encode pact expression EXP as a JSON value. " <>
      "This is only needed for tests, as Pact values are automatically represented as JSON in API output. " <>
      "`(json [{ \"name\": \"joe\", \"age\": 10 } {\"name\": \"mary\", \"age\": 25 }])`"
     ,defRNative "sig-keyset" sigKeyset (funType tTyKeySet [])
     "Convenience to build a keyset from keys present in message signatures, using 'keys-all' as the predicate."
     ,defRNative "print" print' (funType tTyString [("value",a)])
     "Print a string, mainly to format newlines correctly"
     ,defRNative "env-hash" envHash (funType tTyString [("hash",tTyString)])
     "Set current transaction hash. HASH must be a valid BLAKE2b 512-bit hash. `(env-hash (hash \"hello\"))`"
     ])
     where
       json = mkTyVar "a" [tTyInteger,tTyString,tTyTime,tTyDecimal,tTyBool,
                         TyList (mkTyVar "l" []),TySchema TyObject (mkSchemaVar "o"),tTyKeySet,tTyValue]
       a = mkTyVar "a" []

invokeEnv :: (MVar (DbEnv PureDb) -> IO b) -> MVar LibState -> IO b
invokeEnv f e = withMVar e $ \ls -> f $! _rlsPure ls
{-# INLINE invokeEnv #-}

repldb :: PactDb LibState
repldb = PactDb {

    _readRow = \d k -> invokeEnv $ _readRow pactdb d k
  , _writeRow = \wt d k v -> invokeEnv $ _writeRow pactdb wt d k v
  , _keys = \t -> invokeEnv $ _keys pactdb t
  , _txids = \t tid -> invokeEnv $ _txids pactdb t tid
  , _createUserTable = \t m k -> invokeEnv $ _createUserTable pactdb t m k
  , _getUserTableInfo = \t -> invokeEnv $ _getUserTableInfo pactdb t
  , _beginTx = \tid -> invokeEnv $ _beginTx pactdb tid
  , _commitTx = invokeEnv $ _commitTx pactdb
  , _rollbackTx = invokeEnv $ _rollbackTx pactdb
  , _getTxLog = \d t -> invokeEnv $ _getTxLog pactdb d t

}


load :: RNativeFun LibState
load _ [TLitString fn] = setop (Load (unpack fn) False) >> return (tStr $ "Loading " <> fn <> "...")
load _ [TLitString fn, TLiteral (LBool r) _] = setop (Load (unpack fn) r) >> return (tStr $ "Loading " <> fn <> "...")
load i as = argsError i as

modifyLibState :: (LibState -> (LibState,a)) -> Eval LibState a
modifyLibState f = view eePactDbVar >>= \m -> liftIO $ modifyMVar m (return . f)

setLibState :: (LibState -> LibState) -> Eval LibState ()
setLibState f = modifyLibState (f &&& const ())

viewLibState :: (LibState -> a) -> Eval LibState a
viewLibState f = modifyLibState (id &&& f)

setop :: LibOp -> Eval LibState ()
setop v = setLibState $ set rlsOp v

setenv :: Show a => Setter' (EvalEnv LibState) a -> a -> Eval LibState ()
setenv l v = setop $ UpdateEnv $ Endo (set l v)


setsigs :: RNativeFun LibState
setsigs i [TList ts _ _] = do
  ks <- forM ts $ \t -> case t of
          (TLitString s) -> return s
          _ -> argsError i ts
  setenv eeMsgSigs (S.fromList (map (PublicKey . encodeUtf8) ks))
  return $ tStr "Setting transaction keys"
setsigs i as = argsError i as

setmsg :: RNativeFun LibState
setmsg i [TLitString j] =
  case eitherDecode (BSL.fromStrict $ encodeUtf8 j) of
    Left f -> evalError' i ("Invalid JSON: " ++ show f)
    Right v -> setenv eeMsgBody v >> return (tStr "Setting transaction data")
setmsg _ [a] = setenv eeMsgBody (toJSON a) >> return (tStr "Setting transaction data")
setmsg i as = argsError i as


setstep :: RNativeFun LibState
setstep i as = case as of
  [] -> setstep' Nothing >> return (tStr "Un-setting step")
  [TLitInteger j] -> do
    setstep' (Just $ PactStep (fromIntegral j) False def def)
    return $ tStr "Setting step"
  [TLitInteger j,TLiteral (LBool b) _] -> do
    setstep' (Just $ PactStep (fromIntegral j) b def def)
    return $ tStr "Setting step and rollback"
  [TLitInteger j,TLiteral (LBool b) _,o@TObject{}] -> do
    setstep' (Just $ PactStep (fromIntegral j) b def (Just o))
    return $ tStr "Setting step, rollback, and resume value"
  _ -> argsError i as
  where
    setstep' s = do
      setenv eePactStep s
      evalPactExec .= Nothing

setentity :: RNativeFun LibState
setentity i as = case as of
  [TLitString s] -> do
    setenv eeEntity $ Just (EntityName s)
    evalPactExec .= Nothing
    return (tStr $ "Set entity to " <> s)
  [] -> do
    setenv eeEntity Nothing
    evalPactExec .= Nothing
    return (tStr "Unset entity")
  _ -> argsError i as

pactState :: RNativeFun LibState
pactState i [] = do
  e <- use evalPactExec
  case e of
    Nothing -> evalError' i "pact-state: no pact exec in context"
    Just PactExec{..} -> return $ (\o -> TObject o TyAny def)
      [(tStr "yield",fromMaybe (toTerm False) _peYield)
      ,(tStr "executed",toTerm _peExecuted)
      ,(tStr "step",toTerm _peStep)]
pactState i as = argsError i as

txmsg :: Maybe Text -> Maybe TxId -> Text -> Term Name
txmsg n tid s = tStr $ s <> " Tx " <> pack (show tid) <> maybe "" (": " <>) n


tx :: Tx -> RNativeFun LibState
tx Begin i as = do
  tname <- case as of
             [TLitString n] -> return $ Just n
             [] -> return Nothing
             _ -> argsError i as
  setop $ Tx (_faInfo i) Begin tname
  setLibState $ set rlsTxName tname
  return (tStr "")
tx t i [] = do
  tname <- modifyLibState (set rlsTxName Nothing &&& view rlsTxName)
  setop (Tx (_faInfo i) t tname)
  return (tStr "")
tx _ i as = argsError i as

recordTest :: Text -> Maybe (FunApp,Text) -> Eval LibState ()
recordTest name failure = setLibState $ over rlsTests (++ [TestResult name failure])

testSuccess :: Text -> Text -> Eval LibState (Term Name)
testSuccess name msg = recordTest name Nothing >> return (tStr msg)

testFailure :: FunApp -> Text -> Text -> Eval LibState (Term Name)
testFailure i name msg = recordTest name (Just (i,msg)) >> return (tStr msg)

expect :: RNativeFun LibState
expect i [TLitString a,b,c] =
  if b `termEq` c
  then testSuccess a $ "Expect: success: " <> a
  else testFailure i a $ "FAILURE: " <> a <> ": expected " <> pack (show b) <> ", received " <> pack (show c)
expect i as = argsError i as

expectFail :: NativeFun LibState
expectFail i as@[a,b] = do
  a' <- reduce a
  case a' of
    TLitString msg -> do
      r <- catch (Right <$> reduce b) (\(_ :: SomeException) -> return $ Left ())
      case r of
        Right v -> testFailure i msg $ "FAILURE: " <> msg <> ": expected failure, got result = " <> pack (show v)
        Left _ -> testSuccess msg $ "Expect failure: success: " <> msg
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

tc :: RNativeFun LibState
tc i as = case as of
  [TLitString s] -> go s False
  [TLitString s,TLiteral (LBool d) _] -> go s d
  _ -> argsError i as
  where
    go modname dbg = do
      mdm <- HM.lookup (ModuleName modname) <$> view (eeRefStore . rsModules)
      case mdm of
        Nothing -> evalError' i $ "No such module: " ++ show modname
        Just md -> do
          r :: Either CheckerException ([TopLevel Node],[Failure]) <-
            try $ liftIO $ typecheckModule dbg md
          case r of
            Left (CheckerException ei e) -> evalError ei ("Typechecker Internal Error: " ++ e)
            Right (_,fails) -> case fails of
              [] -> return $ tStr $ "Typecheck " <> modname <> ": success"
              _ -> do
                setop $ TcErrors $ map (\(Failure ti s) -> renderInfo (_tiInfo ti) ++ ":Warning: " ++ s) fails
                return $ tStr $ "Typecheck " <> modname <> ": Unable to resolve all types"

#if !defined(ghcjs_HOST_OS)
verify :: RNativeFun LibState
verify i as = case as of
  [TLitString modName] -> do
    modules <- view (eeRefStore . rsModules)
    let mdm = HM.lookup (ModuleName modName) modules
    case mdm of
      Nothing -> evalError' i $ "No such module: " ++ show modName
      Just md -> do
        results <- liftIO $ verifyModule modules md
        case results of
          Left failures  -> setop $ TcErrors $
            fmap (Text.unpack . describeParseFailure) $ failures
          Right results' -> setop $ TcErrors $
            fmap (Text.unpack . describeCheckResult) $
            toListOf (traverse . each) results'

        return (tStr "")

  _ -> argsError i as
#endif

json' :: RNativeFun LibState
json' _ [a] = return $ TValue (toJSON a) def
json' i as = argsError i as

sigKeyset :: RNativeFun LibState
sigKeyset _ _ = view eeMsgSigs >>= \ss -> return $ toTerm $ KeySet (S.toList ss) (asString KeysAll)

print' :: RNativeFun LibState
print' _ [v] = setop (Print v) >> return (tStr "")
print' i as = argsError i as

envHash :: RNativeFun LibState
envHash i [TLitString s] = case fromText' s of
  Left err -> evalError' i $ "Bad hash value: " ++ show s ++ ": " ++ err
  Right h -> do
    setenv eeHash h
    return $ tStr $ "Set tx hash to " <> s
envHash i as = argsError i as

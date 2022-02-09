{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

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
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict (get,put)

import Data.Aeson (eitherDecode,toJSON)
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Semigroup (Endo(..))
import qualified Data.Set as S
import Data.Text (Text, unpack)
import Data.Text.Encoding
import Pact.Time
import qualified Data.Vector as V
import Data.List (isInfixOf)


#if defined(ghcjs_HOST_OS)
import qualified Pact.Analyze.Remote.Client as RemoteClient
import Data.Maybe
#else

import Criterion
import Criterion.Types

import Statistics.Types (Estimate(..))

# ifdef BUILD_TOOL
import qualified Pact.Analyze.Check as Check
# endif
import qualified Pact.Types.Crypto as Crypto
#endif

import Pact.Typechecker
import qualified Pact.Types.Typecheck as TC
import Pact.Native
-- intentionally hidden unused functions to prevent lib functions from consuming gas
import Pact.Native.Internal hiding (defRNative,defGasRNative,defNative)
import qualified Pact.Native.Internal as Native
import Pact.Types.Runtime
import Pact.Eval
import Pact.Persist.Pure
import Pact.PersistPactDb
import Pact.Types.Logger
import Pact.Types.Pretty
import Pact.Repl.Types
import Pact.Native.Capabilities (evalCap)
import Pact.Gas.Table
import Pact.Types.PactValue
import Pact.Types.Capability
import Pact.Runtime.Utils


initLibState :: Loggers -> Maybe String -> IO LibState
initLibState loggers verifyUri = do
  m <- newMVar (DbEnv (def :: PureDb) persister
                (newLogger loggers "Repl")
                def 0 def)
  initLibState' (LibDb m) verifyUri

initLibState' :: LibDb -> Maybe String -> IO LibState
initLibState' ldb@(LibDb db') verifyUri = do
  createSchema db'
  return (LibState ldb Noop def def verifyUri M.empty def)


-- | Native function with no gas consumption.
type ZNativeFun e = FunApp -> [Term Ref] -> Eval e (Term Name)

zeroGas :: Eval e a -> Eval e (Gas,a)
zeroGas = fmap (0,)

defZNative :: NativeDefName -> ZNativeFun e -> FunTypes (Term Name) -> [Example] -> Text -> NativeDef
defZNative name fun = Native.defNative name $ \fi as -> zeroGas $ fun fi as

defZRNative :: NativeDefName -> RNativeFun e -> FunTypes (Term Name) -> [Example] -> Text -> NativeDef
defZRNative name fun = Native.defNative name (reduced fun)
    where reduced f fi as = mapM reduce as >>= zeroGas . f fi

replDefs :: NativeModule
replDefs = ("Repl",
     [
      defZRNative "load" load (funType tTyString [("file",tTyString)] <>
                              funType tTyString [("file",tTyString),("reset",tTyBool)])
      [LitExample "(load \"accounts.repl\")"]
      "Load and evaluate FILE, resetting repl state beforehand if optional RESET is true."
     ,defZRNative "format-address" formatAddr (funType tTyString [("scheme", tTyString), ("public-key", tTyString)])
      []
      "Transform PUBLIC-KEY into an address (i.e. a Pact Runtime Public Key) depending on its SCHEME."
     ,defZRNative "env-keys" setsigs (funType tTyString [("keys",TyList tTyString)])
      ["(env-keys [\"my-key\" \"admin-key\"])"]
      ("DEPRECATED in favor of 'set-sigs'. Set transaction signer KEYS. "<>
       "See 'env-sigs' for setting keys with associated capabilities.")
     ,defZNative "env-sigs" setsigs' (funType tTyString [("sigs",TyList (tTyObject TyAny))])
      [LitExample $ "(env-sigs [{'key: \"my-key\", 'caps: [(accounts.USER_GUARD \"my-account\")]}, " <>
        "{'key: \"admin-key\", 'caps: []}"]
      ("Set transaction signature keys and capabilities. SIGS is a list of objects with \"key\" " <>
       "specifying the signer key, and \"caps\" specifying a list of associated capabilities.")

     ,defZRNative "env-data" setmsg (funType tTyString [("json",json)])
      ["(env-data { \"keyset\": { \"keys\": [\"my-key\" \"admin-key\"], \"pred\": \"keys-any\" } })"]
      "Set transaction JSON data, either as encoded string, or as pact types coerced to JSON."

     ,defZRNative "continue-pact" continuePact
      (funType tTyString [("step",tTyInteger)] <>
       funType tTyString [("step",tTyInteger),("rollback",tTyBool)] <>
       funType tTyString [("step",tTyInteger),("rollback",tTyBool),("pact-id",tTyString)] <>
       funType tTyString [("step",tTyInteger),("rollback",tTyBool),("pact-id",tTyString),("yielded",tTyObject (mkSchemaVar "y"))])
      [LitExample "(continue-pact 1)", LitExample "(continue-pact 1 true)",
       LitExample "(continue-pact 1 false \"[pact-id-hash]\"))",
       LitExample "(continue-pact 2 1 false \"[pact-id-hash]\" { \"rate\": 0.9 })"]
      ("Continue previously-initiated pact identified STEP, " <>
       "optionally specifying ROLLBACK (default is false), " <>
       "PACT-ID of the pact to be continued (defaults to the pact initiated in the current transaction, if one is present), and " <>
       "YIELDED value to be read with 'resume' (if not specified, uses yield in most recent pact exec, if any).")

     ,defZRNative "pact-state" pactState
      (funType (tTyObject TyAny) [] <> funType (tTyObject TyAny) [("clear",tTyBool)])
      [LitExample "(pact-state)", LitExample "(pact-state true)"]
      ("Inspect state from most recent pact execution. Returns object with fields " <>
      "'pactId': pact ID; 'yield': yield result or 'false' if none; 'step': executed step; " <>
      "'executed': indicates if step was skipped because entity did not match. " <>
      "With CLEAR argument, erases pact from repl state.")

     ,defZRNative "env-entity" setentity
      (funType tTyString [] <> funType tTyString [("entity",tTyString)])
      [LitExample "(env-entity \"my-org\")", LitExample "(env-entity)"]
      ("Set environment confidential ENTITY id, or unset with no argument.")
     ,defZRNative "begin-tx" (tx Begin) (funType tTyString [] <>
                                        funType tTyString [("name",tTyString)])
      [ExecExample "(begin-tx \"load module\")"] "Begin transaction with optional NAME."
     ,defZRNative "commit-tx" (tx Commit) (funType tTyString [])
      [ExecExample "(begin-tx) (commit-tx)"] "Commit transaction."
     ,defZRNative "rollback-tx" (tx Rollback) (funType tTyString [])
      [ExecExample "(begin-tx \"Third Act\") (rollback-tx)"] "Rollback transaction."
     ,defZNative "expect" expect
      (funType tTyString [("doc",tTyString),("expected",a),("actual",a)])
      ["(expect \"Sanity prevails.\" 4 (+ 2 2))"]
      "Evaluate ACTUAL and verify that it equals EXPECTED."

     ,defZNative "expect-failure" expectFail
      (funType tTyString [("doc",tTyString),("exp",a)] <>
       funType tTyString [("doc",tTyString),("err",tTyString),("exp",a)])
      ["(expect-failure \"Enforce fails on false\" (enforce false \"Expected error\"))"
      ,"(expect-failure \"Enforce fails with message\" \"Expected error\" (enforce false \"Expected error\"))"
      ]
      "Evaluate EXP and succeed only if it throws an error."

     ,defZNative "expect-that" expectThat
      (funType tTyString
       [("doc",tTyString)
       ,("pred",TyFun (funType' tTyBool [("value",a)]))
       ,("exp",a)])
      [ExecExample "(expect-that \"addition\" (< 2) (+ 1 2))"
      ,ExecErrExample "(expect-that \"addition\" (> 2) (+ 1 2))"]
      "Evaluate EXP and succeed if value passes predicate PRED."
     ,defZNative "bench" bench' (funType tTyString [("exprs",TyAny)])
      [LitExample "(bench (+ 1 2))"] "Benchmark execution of EXPRS."
     ,defZRNative "typecheck" tc (funType tTyString [("module",tTyString)] <>
                                 funType tTyString [("module",tTyString),("debug",tTyBool)])
       []
       "Typecheck MODULE, optionally enabling DEBUG output."
     ,defZRNative "env-gaslimit" setGasLimit (funType tTyString [("limit",tTyInteger)])
       []
       "Set environment gas limit to LIMIT."
     ,defZRNative "env-gas" envGas (funType tTyInteger [] <> funType tTyString [("gas",tTyInteger)])
       ["(env-gasmodel \"table\") (env-gaslimit 10) (env-gas 0) (map (+ 1) [1 2 3]) (env-gas)"]
       ("Query gas state, or set it to GAS. Note that certain plaforms may charge additional gas that is not captured " <>
        "by the interpreter gas model, such as an overall transaction-size cost.")
     ,defZRNative "env-gasprice" setGasPrice (funType tTyString [("price",tTyDecimal)])
       []
       "Set environment gas price to PRICE."
     ,defZRNative "env-gasrate" setGasRate (funType tTyString [("rate",tTyInteger)])
       []
       "Update gas model to charge constant RATE."
     ,defZRNative "env-gasmodel" setGasModel
      (funType tTyString [("model",tTyString)] <>
       funType tTyString [] <>
       funType tTyString [("model",tTyString),("rate",tTyInteger)])
      [ExecExample "(env-gasmodel)",ExecExample "(env-gasmodel 'table)",ExecExample "(env-gasmodel 'fixed 1)"]
      ("Update or query current gas model. With just MODEL, \"table\" is supported; " <>
       "with MODEL and RATE, 'fixed' is supported. With no args, output current model.")
     ,defZRNative "env-gaslog" gasLog (funType tTyString [])
       ["(env-gasmodel \"table\") (env-gaslimit 10) (env-gaslog) (map (+ 1) [1 2 3]) (env-gaslog)"]
       "Enable and obtain gas logging. Bracket around the code whose gas logs you want to inspect."
     ,defZRNative "env-exec-config" envExecConfig
      (funType (TyList tTyString) [("flags",TyList tTyString)] <>
       funType (TyList tTyString) [])
      ["(env-exec-config ['DisableHistoryInTransactionalMode]) (env-exec-config)"]
      ("Queries, or with arguments, sets execution config flags. Valid flags: " <>
       tShow (M.keys flagReps))
     ,defZRNative "verify" verify (funType tTyString [("module",tTyString)])
       []
       "Verify MODULE, checking that all properties hold."
     ,defZRNative "sig-keyset" sigKeyset (funType tTyKeySet [])
       []
       "Convenience function to build a keyset from keys present in message signatures, using 'keys-all' as the predicate."
     ,defZRNative "print" print' (funType tTyString [("value",a)])
       []
       "Output VALUE to terminal as unquoted, unescaped text."
     ,defZRNative "env-hash" envHash (funType tTyString [("hash",tTyString)])
       ["(env-hash (hash \"hello\"))"]
       "Set current transaction hash. HASH must be an unpadded base64-url encoded BLAKE2b 256-bit hash."
     ,defZNative "test-capability" testCapability
      (funType tTyString [("capability", TyFun $ funType' tTyBool [])])
      [LitExample "(test-capability (MY-CAP))"] $
     "Acquire (if unmanaged) or install (if managed) CAPABILITY. CAPABILITY and any composed capabilities are in scope " <>
     "for the rest of the transaction."
     ,defZRNative "mock-spv" mockSPV
       (funType tTyString [("type",tTyString),("payload",tTyObject TyAny),("output",tTyObject TyAny)])
      [LitExample "(mock-spv \"TXOUT\" { 'proof: \"a54f54de54c54d89e7f\" } { 'amount: 10.0, 'account: \"Dave\", 'chainId: \"1\" })"]
       "Mock a successful call to 'spv-verify' with TYPE and PAYLOAD to return OUTPUT."
     , envChainDataDef
     ,defZNative "env-namespace-policy" envNamespacePolicy
      (funType tTyString
       [("allow-root", tTyBool),
        ("ns-policy-fun",TyFun $ funType' tTyBool [("ns",tTyString),("ns-admin",tTyGuard Nothing)])])
      [LitExample "(env-namespace-policy (my-ns-policy-fun))"]
      "Install a managed namespace policy specifying ALLOW-ROOT and NS-POLICY-FUN."
     ,defZRNative "env-events" envEvents
      (funType (TyList (tTyObject TyAny)) [("clear",tTyBool)])
      [LitExample "(env-events true)"]
      ("Retreive any accumulated events and optionally clear event state. " <>
       "Object returned has fields 'name' (fully-qualified event name), " <>
       "'params' (event parameters), 'module-hash' (hash of emitting module).")
     ,defZRNative "env-enable-repl-natives" enableReplNatives
      (funType tTyString [("enable",tTyBool)])
      [ExecExample "(env-enable-repl-natives true)"]
      ("Control whether REPL native functions are allowed in module code. " <>
       "When enabled, fixture functions like 'env-sigs' are allowed in module code.")
     ,defZNative "with-applied-env" withEnv
      (funType a [("exec",a)])
      [ExecExample "(let ((a 1)) (env-data { 'b: 1 }) (with-applied-env (+ a (read-integer 'b))))"]
      ("Evaluate EXEC with any pending environment changes applied. " <>
       "Normally, environment changes must execute at top-level for the change to take effect. " <>
       "This allows scoped application of non-toplevel environment changes.")
     ,defZRNative "env-dynref" envDynRef
      (funType tTyString [("iface",TyModule Nothing),("impl",TyModule (Just []))] <>
       funType tTyString [])
      [LitExample "(env-dynref fungible-v2 coin)"]
      ("Substitute module IMPL in any dynamic usages of IFACE in typechecking and analysis. " <>
       "With no arguments, remove all substitutions.")
     ])
     where
       json = mkTyVar "a" [tTyInteger,tTyString,tTyTime,tTyDecimal,tTyBool,
                         TyList (mkTyVar "l" []),TySchema TyObject (mkSchemaVar "o") def,tTyKeySet]
       a = mkTyVar "a" []

invokeEnv :: (LibDb -> IO b) -> MVar LibState -> IO b
invokeEnv f e = withMVar e $ \ls -> f $! (_rlsDb ls)
{-# INLINE invokeEnv #-}

repldb :: PactDb LibState
repldb = PactDb {

    _readRow = \d k -> invokeEnv $ \(LibDb e) -> _readRow pactdb d k e
  , _writeRow = \wt d k v -> invokeEnv $ \(LibDb e) ->  _writeRow pactdb wt d k v e
  , _keys = \t -> invokeEnv $ \(LibDb e) ->  _keys pactdb t e
  , _txids = \t tid -> invokeEnv $ \(LibDb e) ->  _txids pactdb t tid e
  , _createUserTable = \t m -> invokeEnv $ \(LibDb e) ->  _createUserTable pactdb t m e
  , _getUserTableInfo = \t -> invokeEnv $ \(LibDb e) ->  _getUserTableInfo pactdb t e
  , _beginTx = \tid -> invokeEnv $ \(LibDb e) ->  _beginTx pactdb tid e
  , _commitTx = invokeEnv $ \(LibDb e) ->  _commitTx pactdb e
  , _rollbackTx = invokeEnv $ \(LibDb e) ->  _rollbackTx pactdb e
  , _getTxLog = \d t -> invokeEnv $ \(LibDb e) ->  _getTxLog pactdb d t e

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
setop v = setLibState $ over rlsOp (<> v)

setenv :: Setter' (EvalEnv LibState) a -> a -> Eval LibState ()
setenv l v = setop $ UpdateEnv $ Endo (set l v)

envDynRef :: RNativeFun LibState
envDynRef i [TModRef iface _,TModRef impl _] =
  lookupModule i (_modRefName impl) >>= \r -> case r of
    Nothing -> evalError' i "Unable to resolve impl module"
    Just md -> do
      setLibState $ over rlsDynEnv $ M.insert (_modRefName iface) md
      return $ tStr "Added dynamic ref to environment."
envDynRef _i [] = do
  setLibState $ set rlsDynEnv def
  return $ tStr "Cleared dynamic ref environment."
envDynRef i as = argsError i as

mockSPV :: RNativeFun LibState
mockSPV i as = case as of
  [TLitString spvType, TObject payload _, TObject out _] -> do
    setLibState $ over rlsMockSPV (M.insert (SPVMockKey (spvType,payload)) out)
    return $ tStr $ "Added mock SPV for " <> spvType
  _ -> argsError i as

formatAddr :: RNativeFun LibState
#if !defined(ghcjs_HOST_OS)
formatAddr i [TLitString scheme, TLitString cryptoPubKey] = do
  let eitherEvalErr :: Either String a -> String -> (a -> b) -> Eval LibState b
      eitherEvalErr res effectStr transformFunc =
        case res of
          Left e  -> evalError' i $ prettyString effectStr <> ": " <> prettyString e
          Right v -> return (transformFunc v)
  sppk  <- eitherEvalErr (fromText' scheme)
           "Invalid PPKScheme"
           Crypto.toScheme
  pubBS <- eitherEvalErr (parseB16TextOnly cryptoPubKey)
           "Invalid Public Key format"
           Crypto.PubBS
  addr  <- eitherEvalErr (Crypto.formatPublicKeyBS sppk pubBS)
           "Unable to convert Public Key to Address"
           toB16Text
  return (tStr addr)
formatAddr i as = argsError i as
#else
formatAddr i _ = evalError' i "Address formatting not supported in GHCJS"
#endif


setsigs :: RNativeFun LibState
setsigs i [TList ts _ _] = do
  ks <- forM ts $ \t -> case t of
          (TLitString s) -> return s
          _ -> argsError i (V.toList ts)
  setenv eeMsgSigs $ M.fromList ((,mempty) . PublicKey . encodeUtf8 <$> V.toList ks)
  return $ tStr "Setting transaction keys"
setsigs i as = argsError i as

setsigs' :: ZNativeFun LibState
setsigs' _ [TList ts _ _] = do
  sigs <- forM ts $ \t -> case t of
    (TObject (Object (ObjectMap om) _ _ _) _) -> do
      case (M.lookup "key" om,M.lookup "caps" om) of
        (Just k'',Just (TList clist _ _)) -> do
          reduce k'' >>= \k' -> case k' of
            TLitString k -> do
              caps <- forM clist $ \cap -> case cap of
                (TApp a _) -> view _1 <$> appToCap a
                o -> evalError' o $ "Expected capability invocation"
              return (PublicKey $ encodeUtf8 k,S.fromList (V.toList caps))
            _ -> evalError' k' "Expected string value"
        _ -> evalError' t "Expected object with 'key': string, 'caps': [capability]"
    _ -> evalError' t $ "Expected object"
  setenv eeMsgSigs $ M.fromList $ V.toList sigs
  return $ tStr "Setting transaction signatures/caps"
setsigs' i as = argsError' i as


setmsg :: RNativeFun LibState
setmsg i as = case as of
  [TLitString j] ->
    case eitherDecode (BSL.fromStrict $ encodeUtf8 j) of
      Left f -> evalError' i ("Invalid JSON: " <> prettyString f)
      Right v -> go v
  [TObject (Object om _ _ _) _] -> go (toJSON (fmap toPactValueLenient om))
  [a] -> go (toJSON a)
  _ -> argsError i as
  where go v = setenv eeMsgBody v >> return (tStr "Setting transaction data")

continuePact :: RNativeFun LibState
continuePact i as = case as of
  [TLitInteger step] ->
    go step False Nothing Nothing
  [TLitInteger step,TLitBool rollback] ->
    go step rollback Nothing Nothing
  [TLitInteger step,TLitBool rollback,TLitString pid] ->
    go step rollback (Just pid) Nothing
  [TLitInteger step,TLitBool rollback,TLitString pid,TObject (Object o _ _ _) _] ->
    go step rollback (Just pid) (Just o)
  _ -> argsError i as
  where
    go :: Integer -> Bool -> Maybe Text -> Maybe (ObjectMap (Term Name)) -> Eval LibState (Term Name)
    go step rollback pid userResume = do
      pe <- use evalPactExec
      (pactId, y) <- unwrapExec pid userResume pe

      let pactStep = PactStep (fromIntegral step) rollback pactId y
      evalPactExec .= Nothing
      local (set eePactStep $ Just pactStep) $ resumePact (_faInfo i) Nothing

    unwrapExec mpid mobj Nothing = do
      pid <- case mpid of
        Nothing -> evalError' i
          "continue-pact: No pact id supplied and no pact exec in context"
        Just pid -> return $ PactId pid
      y <- maybe (return Nothing) (toYield Nothing) mobj
      return (pid, y)
    unwrapExec mpid mobj (Just ex) = do
      let pid = maybe (_pePactId ex) PactId mpid
      y <- case mobj of
        Nothing -> return $ _peYield ex
        Just o -> case _peYield ex of
          Just (Yield _ p _) -> toYield p o
          Nothing -> toYield Nothing o
      return (pid, y)

    toYield p = fmap (Just . (\v -> Yield v p Nothing)) . enforcePactValue'

setentity :: RNativeFun LibState
setentity i as = case as of
  [TLitString s] -> do
    setenv eeEntity $ Just (EntityName s)
    return (tStr $ "Set entity to " <> s)
  [] -> do
    setenv eeEntity Nothing
    return (tStr "Unset entity")
  _ -> argsError i as

pactState :: RNativeFun LibState
pactState i as = case as of
  [] -> go False
  [TLitBool clear] -> go clear
  _ -> argsError i as
  where
    go clear = do
      e <- use evalPactExec
      when clear $ evalPactExec .= Nothing
      case e of
        Nothing -> evalError' i "pact-state: no pact exec in context"
        Just PactExec{..} -> return $ toTObject TyAny def $
          [("yield",maybe (toTerm False) (toTObjectMap TyAny def . fmap fromPactValue . _yData) _peYield)
          ,("executed",toTerm (_peExecuted /= Just False))
          ,("step",toTerm _peStep)
          ,("pactId",toTerm _pePactId)]


tx :: Tx -> RNativeFun LibState
tx t fi as = do

  (tid,tname) <- case (t,as) of
    (Begin,[TLitString n]) -> doBegin (Just n)
    (Begin,[]) -> doBegin Nothing
    (Commit,[]) -> evalCommitTx i >> resetTx
    (Rollback,[]) -> evalRollbackTx i >> resetTx
    _ -> argsError fi as

  -- reset to repl lib, preserve call stack
  cs <- use evalCallStack
  put $ set (evalRefs.rsLoaded) (moduleToMap replDefs) $ set evalCallStack cs def
  return $ tStr $ tShow t <> " Tx"
      <> maybeDelim " " tid <> maybeDelim ": " tname

  where

    i = getInfo fi

    doBegin tname = do
      tid <- evalBeginTx i
      setLibState $ set rlsTx (tid,tname)
      return (tid,tname)

    resetTx = modifyLibState
      (set rlsTx (Nothing,Nothing) &&& view rlsTx)


recordTest :: Text -> Maybe (FunApp,Text) -> Eval LibState ()
recordTest name failure = setLibState $ over rlsTests (++ [TestResult name failure])

testSuccess :: Text -> Text -> Eval LibState (Term Name)
testSuccess doc msg = do
  recordTest doc Nothing
  return $ tStr $ msg <> ": success: " <> doc

testFailure :: FunApp -> Text -> Doc -> Eval LibState (Term Name)
testFailure i doc msg = recordTest doc (Just (i,rmsg)) >> return (tStr rmsg)
  where
    rmsg = "FAILURE: " <> doc <> ": " <> renderCompactText' msg

testDoc :: FunApp -> [Term Ref] -> Eval e Text
testDoc _ (doc'':_) = reduce doc'' >>= \doc' -> case doc' of
  TLitString doc -> enrichTestName doc
  _ -> evalError' doc'' "Expected string"
  where
    -- | Use stack frames for 'FunApp's to enrich test name, using
    -- presence of module name as a heuristic of the "user stack".
    enrichTestName :: Text -> Eval e Text
    enrichTestName msg = use evalCallStack >>= return . foldl' go msg

    go m StackFrame{..} = case preview (_Just . _1 . faModule . _Just) _sfApp of
      Just {} -> _sfName <> "." <> m
      _ -> m
testDoc i as = argsError' i as

testCatch :: FunApp -> Text -> Eval LibState a -> Doc ->
             (a -> Eval LibState (Term Name)) -> Eval LibState (Term Name)
testCatch i doc expr errMsg cont = catchesPactError expr >>= \r -> case r of
  Right v -> cont v
  Left e -> testFailure i doc $ errMsg <> ": " <> prettyErr e
  where
    prettyErr e = prettyInfo (peInfo e) <> peDoc e
    prettyInfo a
        | a == def = ""
        | otherwise = pretty (renderInfo a) <> ": "


expect :: ZNativeFun LibState
expect i as@[_,b',c'] = do
  doc <- testDoc i as
  testCatch i doc (reduce c') "evaluation of actual failed" $ \c ->
    testCatch i doc (reduce b') "evaluation of expected failed" $ \b ->
      if b `termEq` c
      then testSuccess doc "Expect"
      else testFailure i doc $
           "expected " <> pretty b <> ":" <> pretty (typeof' b) <>
           ", received " <> pretty c <> ":" <> pretty (typeof' c)
expect i as = argsError' i as

expectFail :: ZNativeFun LibState
expectFail i as = case as of
  [_,expr] -> go Nothing expr
  [_,err,expr] -> reduce err >>= \e -> case e of
    TLitString errmsg -> go (Just $ unpack errmsg) expr
    _ -> argsError' i as
  _ -> argsError' i as
  where
    tsuccess msg = testSuccess msg "Expect failure"
    go errM expr = do
      msg' <- testDoc i as
      r <- catch (Right <$> reduce expr) (\(e :: SomeException) -> return $ Left (show e))
      case r of
        Right v -> testFailure i msg' $ "expected failure, got result = " <> pretty v
        Left e -> case errM of
          Nothing -> tsuccess msg'
          Just err
              | err `isInfixOf` e -> tsuccess msg'
              | otherwise ->
                  testFailure i msg' $ "expected error message to contain '" <> pretty err
                  <> "', got '" <> pretty e <> "'"

expectThat :: ZNativeFun LibState
expectThat i as@[_,tLamToApp -> TApp pred' predi,expr'] = do
  doc <- testDoc i as
  testCatch i doc (reduce expr') "evaluation of expression failed" $ \v ->
    testCatch i doc (apply pred' [v]) "evaluation of predicate failed" $ \p -> case p of
      TLitBool b
          | b -> testSuccess doc $ "Expect-that"
          | otherwise ->
              testFailure i doc $ "did not satisfy"
              <> prettyPred predi <> ": " <> pretty v <> ":" <> pretty (typeof' v)
      t -> testFailure i doc $ "predicate did not return boolean: " <> pretty t
  where
    prettyPred (Info (Just (c,_))) = " " <> pretty c
    prettyPred _ = ""
expectThat i as = argsError' i as




bench' :: ZNativeFun LibState
#if !defined(ghcjs_HOST_OS)
bench' i as = do
  e <- ask
  s <- get
  (r :: Either SomeException Report) <-
      try $ liftIO $ benchmark' $ whnfIO $ runEval s e $ do
                !ts <- mapM reduce as
                return $! toTerm (length ts)
  case r of
    Left ex -> evalError' i (viaShow ex)
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
      md <- getModule i (ModuleName modname Nothing)
      de <- viewLibState _rlsDynEnv
      r :: Either TC.CheckerException ([TC.TopLevel TC.Node],[TC.Failure]) <-
        try $ liftIO $ typecheckModule dbg de md
      case r of
        Left (TC.CheckerException ei e) -> evalError ei ("Typechecker Internal Error: " <> prettyString e)
        Right (_,fails) -> case fails of
          [] -> return $ tStr $ "Typecheck " <> modname <> ": success"
          _ -> do
            setop $ Output $ map TC.renderTcFailure fails
            return $ tStr $ "Typecheck " <> modname <> ": Unable to resolve all types"

verify :: RNativeFun LibState
verify i _as@[TLitString modName] = do
#if defined(ghcjs_HOST_OS)
    -- ghcjs: use remote server
    (md,modules) <- _loadModules
    uri <- fromMaybe "localhost" <$> viewLibState (view rlsVerifyUri)
    renderedLines <- liftIO $
                     RemoteClient.verifyModule modules md uri
    setop $ Output renderedLines
    return _failureMessage
#elif defined(BUILD_TOOL)
    -- ghc + build-tool: run verify
    (md,modules) <- _loadModules
    de <- viewLibState _rlsDynEnv
    modResult <- liftIO $ Check.verifyModule de modules md
    let renderedLines = Check.renderVerifiedModule modResult
    setop $ Output renderedLines
    if any ((== OutputFailure) . _roType) renderedLines
      then return _failureMessage
      else return $ tStr $ "Verification of " <> modName <> " succeeded"
#else
    -- ghc - build-tool: typecheck only
    tc i _as
#endif
  where
    _failureMessage = tStr $ "Verification of " <> modName <> " failed"
    _loadModules = (,)
        <$> getModule i (ModuleName modName Nothing)
        <*> getAllModules i
verify i as = argsError i as


sigKeyset :: RNativeFun LibState
sigKeyset _ _ = view eeMsgSigs >>= \ss ->
  return $ toTerm $ mkKeySet (M.keys ss) (asString KeysAll)

print' :: RNativeFun LibState
print' _ [v] = setop (Print v) >> return (tStr "")
print' i as = argsError i as

envHash :: RNativeFun LibState
envHash i [TLitString s] = case fromText' s of
  Left err -> evalError' i $ "Bad hash value: " <> pretty s <> ": " <> prettyString err
  Right h -> do
    setenv eeHash h
    return $ tStr $ "Set tx hash to " <> s
envHash i as = argsError i as

setGasLimit :: RNativeFun LibState
setGasLimit _ [TLitInteger l] = do
  setenv (eeGasEnv . geGasLimit) (fromIntegral l)
  return $ tStr $ "Set gas limit to " <> tShow l
setGasLimit i as = argsError i as

envGas :: RNativeFun LibState
envGas _ [] = do
  use evalGas >>= \g -> return (tLit $ LInteger $ fromIntegral g)
envGas _ [TLitInteger g] = do
  evalGas .= fromIntegral g
  return $ tStr $ "Set gas to " <> tShow g
envGas i as = argsError i as


gasLog :: RNativeFun LibState
gasLog _ _ = do
  gl <- use evalLogGas
  evalLogGas .= Just []
  case gl of
    Nothing -> return $ tStr $ "Enabled gas log"
    Just logs -> let total = sum (map snd logs) in
      return $ toTList tTyString def $ (tStr ("TOTAL: " <> tShow total):) $
        reverse $ flip map logs $ \(n,g) ->
        tStr $ renderCompactText' $ pretty n <> ": " <> pretty g


setGasPrice :: RNativeFun LibState
setGasPrice _ [TLiteral (LDecimal d) _] = do
  setenv (eeGasEnv . geGasPrice) (wrap (wrap d))
  return $ tStr $ "Set gas price to " <> tShow d
setGasPrice i as = argsError i as

envExecConfig :: RNativeFun LibState
envExecConfig i as = case as of
  [TList reps _ _] -> do
    fs <- forM reps $ \s -> case s of
      TLitString r -> case M.lookup r flagReps of
        Nothing -> evalError' i $ "Invalid flag, allowed: " <> viaShow (M.keys flagReps)
        Just f -> return f
      _ -> argsError i as
    let ec = ExecutionConfig $ S.fromList $ V.toList fs
    setenv eeExecutionConfig ec
    report ec
  [] -> view eeExecutionConfig >>= report
  _ -> argsError i as
  where
    report ExecutionConfig{..} = return $ toTList tTyString def $
      map (tStr . flagRep) $ S.toList _ecFlags


setGasRate :: RNativeFun LibState
setGasRate _ [TLitInteger r] = do
  let model = constGasModel $ fromIntegral r
  setenv (eeGasEnv . geGasModel) model
  return $ tStr $ "Set gas model to " <> gasModelDesc model
setGasRate i as = argsError i as

setGasModel :: RNativeFun LibState
setGasModel _ [] = do
  model <- asks (_geGasModel . _eeGasEnv)
  return $ tStr $ "Current gas model is '" <> gasModelName model <> "': " <> gasModelDesc model
setGasModel _ as = do
  let mMod = case as of
        [TLitString "table"] -> Just $ tableGasModel defaultGasConfig
        [TLitString "fixed", TLitInteger r] -> Just $ constGasModel (fromIntegral r)
        _ -> Nothing
  case mMod of
    Nothing -> return $ tStr "Unrecognized model, perhaps try (env-gasmodel \"table\") or (env-gasmodel \"fixed\" 1)"
    Just model -> do
      setenv (eeGasEnv . geGasModel) model
      return $ tStr $ "Set gas model to " <> gasModelDesc model

-- | This is the only place we can do an external call to a capability,
-- using 'evalCap False'.
testCapability :: ZNativeFun ReplState
testCapability i [ (TApp app _) ] = do
  (_,d,_) <- appToCap app
  let scope = maybe CapCallStack (const CapManaged) (_dDefMeta d)
  r <- evalCap i scope False $ app
  return . tStr $ case r of
    AlreadyAcquired -> "Capability already acquired"
    NewlyAcquired -> "Capability acquired"
    NewlyInstalled _ -> "Capability installed"
testCapability i as = argsError' i as

-- | Modify existing env chain data with new data, replacing just those
-- environment items only.
envChainDataDef :: NativeDef
envChainDataDef = defZRNative "env-chain-data" envChainData
    (funType tTyString [("new-data", objectType)])
    ["(env-chain-data { \"chain-id\": \"TestNet00/2\", \"block-height\": 20 })"]
    "Update existing entries of 'chain-data' with NEW-DATA, replacing those items only."
  where
    objectType = TySchema
      TyObject
      (TyUser $ snd chainDataSchema)
      AnySubschema

    envChainData :: RNativeFun LibState
    envChainData i as = case as of
      [TObject (Object (ObjectMap ks) _ _ _) _] -> do
        pd <- view eePublicData

        ud <- foldM (go (_faInfo i)) pd (M.toList ks)
        setenv eePublicData ud
        return $ tStr "Updated public metadata"
      _ -> argsError i as

    go _i pd (k, (TLiteral (LInteger l) _))
      | k == cdGasLimit = pure $ set (pdPublicMeta . pmGasLimit) (wrap (wrap l)) pd
      | k == cdBlockHeight = pure $ set pdBlockHeight (fromIntegral l) pd

    go _i pd (k, (TLiteral (LDecimal l) _))
      | k == cdGasPrice = pure $ set (pdPublicMeta . pmGasPrice) (wrap (wrap l)) pd

    go _i pd (k, (TLiteral (LTime l) _))
      | k == cdBlockTime = pure $ set pdBlockTime (toPosixTimestampMicros l) pd

    go _i pd (k, (TLiteral (LString l) _))
      | k == cdChainId = pure $ set (pdPublicMeta . pmChainId) (ChainId l) pd
      | k == cdSender = pure $ set (pdPublicMeta . pmSender) l pd
      | k == cdPrevBlockHash = pure $ set pdPrevBlockHash l pd

    go i _ as = evalError i $ "envChainData: bad public metadata values: " <> pretty as


envNamespacePolicy :: ZNativeFun LibState
envNamespacePolicy i as@[ar,TApp app _] = reduce ar >>= \ar' -> case ar' of
  (TLiteral (LBool allowRoot) _) -> requireDefApp Defun app >>= \d -> do
    setenv eeNamespacePolicy (SmartNamespacePolicy allowRoot (toQName d))
    return $ tStr $ "Installed namespace policy"
  _ -> argsError' i as
  where
    toQName Def{..} = QualifiedName _dModule (asString _dDefName) _dInfo
envNamespacePolicy i as = argsError' i as


envEvents :: RNativeFun LibState
envEvents _i [TLiteral (LBool clear) _] = do
  es <- use evalEvents
  when clear $ evalEvents .= []
  return $ toTList TyAny def $ (`map` es) $ \PactEvent{..} -> toTObject TyAny def $
    [("name",toTerm (renderCompactText _eventModule <> "." <> _eventName))
    ,("params",toTList TyAny def $ map fromPactValue _eventParams)
    ,("module-hash",toTerm $ asString _eventModuleHash)]
envEvents i as = argsError i as

enableReplNatives :: RNativeFun LibState
enableReplNatives _i [TLiteral (LBool enable) _] = do
  let (msg,defs) | enable = ("enabled",nativeDefs <> moduleToMap replDefs)
                 | otherwise = ("disabled",nativeDefs)
  setenv ( eeRefStore . rsNatives ) defs
  return $ tStr $ "Repl natives " <> msg
enableReplNatives i as = argsError i as

withEnv :: ZNativeFun LibState
withEnv _ [exec] = do
  updates <- modifyLibState $ \ls -> case _rlsOp ls of
    UpdateEnv e -> (set rlsOp Noop ls,e)
    _ -> (ls,Endo id)
  local (appEndo updates) $ reduce exec
withEnv i as = argsError' i as

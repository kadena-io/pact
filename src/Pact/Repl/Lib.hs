{-# LANGUAGE TupleSections #-}
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
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Catch

import Data.Aeson (eitherDecode,toJSON)
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import qualified Data.Map.Strict as M
import Data.Semigroup (Endo(..))
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Thyme.Time.Core
import qualified Data.Vector as V
import Data.List (isInfixOf)

#if defined(ghcjs_HOST_OS)
import qualified Pact.Analyze.Remote.Client as RemoteClient
import Data.Maybe
#else

import Criterion
import Criterion.Types

import Control.Monad.State.Strict (get)

import Statistics.Types (Estimate(..))

import qualified Pact.Analyze.Check as Check
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


initLibState :: Loggers -> Maybe String -> IO LibState
initLibState loggers verifyUri = do
  m <- newMVar (DbEnv def persister
                (newLogger loggers "Repl")
                def 0 def)
  createSchema m
  return (LibState m Noop def def verifyUri M.empty M.empty)

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
      [LitExample "(begin-tx \"load module\")"] "Begin transaction with optional NAME."
     ,defZRNative "commit-tx" (tx Commit) (funType tTyString []) [LitExample "(commit-tx)"] "Commit transaction."
     ,defZRNative "rollback-tx" (tx Rollback) (funType tTyString []) [LitExample "(rollback-tx)"] "Rollback transaction."
     ,defZRNative "expect" expect (funType tTyString [("doc",tTyString),("expected",a),("actual",a)])
      ["(expect \"Sanity prevails.\" 4 (+ 2 2))"]
      "Evaluate ACTUAL and verify that it equals EXPECTED."

     ,defZNative "expect-failure" expectFail
      (funType tTyString [("doc",tTyString),("exp",a)] <>
       funType tTyString [("doc",tTyString),("err",tTyString),("exp",a)])
      ["(expect-failure \"Enforce fails on false\" (enforce false \"Expected error\"))"
      ,"(expect-failure \"Enforce fails with message\" \"Expected error\" (enforce false \"Expected error\"))"
      ]
      "Evaluate EXP and succeed only if it throws an error."

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
     ,defZRNative "env-gasmodel" setGasModel (funType tTyString [("model",tTyString)])
       []
       "Update gas model to the model named MODEL."
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
     ])
     where
       json = mkTyVar "a" [tTyInteger,tTyString,tTyTime,tTyDecimal,tTyBool,
                         TyList (mkTyVar "l" []),TySchema TyObject (mkSchemaVar "o") def,tTyKeySet]
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
  , _createUserTable = \t m -> invokeEnv $ _createUserTable pactdb t m
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

setenv :: Setter' (EvalEnv LibState) a -> a -> Eval LibState ()
setenv l v = setop $ UpdateEnv $ Endo (set l v)

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
        (Just (TLitString k),Just (TList clist _ _)) -> do
          caps <- forM clist $ \cap -> case cap of
            (TApp a _) -> view _1 <$> appToCap a
            o -> evalError' o $ "Expected capability invocation"
          return (PublicKey $ encodeUtf8 k,S.fromList (V.toList caps))
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
      viewLibState (view rlsPacts) >>= \pacts ->
        case M.lookup pactId pacts of
          Nothing -> evalError' i $ "Invalid pact id: " <> pretty pactId
          Just PactExec{} -> do
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
          Just (Yield _ p) -> toYield p o
          Nothing -> toYield Nothing o
      return (pid, y)

    toYield p = fmap (Just . flip Yield p) . enforcePactValue'

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
  else testFailure i a $ renderCompactText' $
       "FAILURE: " <> pretty a <> ": expected " <> pretty b <> ":" <> pretty (typeof' b) <>
       ", received " <> pretty c <> ":" <> pretty (typeof' c)
expect i as = argsError i as

expectFail :: ZNativeFun LibState
expectFail i as = case as of
  [doc,expr] -> go doc Nothing expr
  [doc,err,expr] -> reduce err >>= \e -> case e of
    TLitString errmsg -> go doc (Just $ unpack errmsg) expr
    _ -> argsError' i as
  _ -> argsError' i as
  where
    tsuccess msg = testSuccess msg $ "Expect failure: success: " <> msg
    tfailure msg details = testFailure i msg $ "FAILURE: " <> msg <> ": " <> details
    go doc errM expr = reduce doc >>= \d -> case d of
      TLitString msg -> do
        r <- catch (Right <$> reduce expr) (\(e :: SomeException) -> return $ Left (show e))
        case r of
          Right v -> tfailure msg $ "expected failure, got result = " <> pack (showPretty v)
          Left e -> case errM of
            Nothing -> tsuccess msg
            Just err | err `isInfixOf` e -> tsuccess msg
                     | otherwise -> tfailure msg $ "expected error message to contain '" <> pack err <> "', got '" <> pack e <> "'"
      _ -> argsError' i as

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
      r :: Either TC.CheckerException ([TC.TopLevel TC.Node],[TC.Failure]) <-
        try $ liftIO $ typecheckModule dbg md
      case r of
        Left (TC.CheckerException ei e) -> evalError ei ("Typechecker Internal Error: " <> prettyString e)
        Right (_,fails) -> case fails of
          [] -> return $ tStr $ "Typecheck " <> modname <> ": success"
          _ -> do
            setop $ TcErrors $ map (\(TC.Failure ti s) -> renderInfo (TC._tiInfo ti) ++ ":Warning: " ++ s) fails
            return $ tStr $ "Typecheck " <> modname <> ": Unable to resolve all types"

verify :: RNativeFun LibState
verify i as = case as of
  [TLitString modName] -> do
    md <- getModule i (ModuleName modName Nothing)
    -- reading all modules from db here, but should be fine in repl
    modules <- getAllModules i
    let failureMessage = tStr $ "Verification of " <> modName <> " failed"
#if defined(ghcjs_HOST_OS)
    uri <- fromMaybe "localhost" <$> viewLibState (view rlsVerifyUri)
    renderedLines <- liftIO $
                     RemoteClient.verifyModule modules md uri
    setop $ TcErrors $ Text.unpack <$> renderedLines
    return failureMessage
#else
    modResult <- liftIO $ Check.verifyModule modules md
    let renderedLines = Check.renderVerifiedModule modResult
    case modResult of
      Right modResult'
        | not (Check.hasVerificationError modResult')
        -> return $ tStr $ mconcat renderedLines
      _ -> do
        setop $ TcErrors $ Text.unpack <$> renderedLines
        return failureMessage
#endif

  _ -> argsError i as

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
envGas _ [] = use evalGas >>= \g -> return (tLit $ LInteger $ fromIntegral g)
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
      | k == cdBlockTime = pure $ set pdBlockTime (toMicroseconds $ utcTimeToPOSIXSeconds l) pd

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

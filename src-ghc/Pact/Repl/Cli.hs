{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Pact.Repl.Cli
-- Copyright   :  (C) 2020 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Client library
--

module Pact.Repl.Cli
  ( loadCli
  ) where


import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson as Aeson hiding (Object)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Decimal
import Data.Default
import Data.Function
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy,elemIndex)
import qualified Data.Set as S
import Data.String
import Data.Text (Text,pack,unpack,intercalate)
import Data.Text.Encoding
import Data.Thyme.Time.Core
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import Network.Connection
import Network.HTTP.Client.TLS
import Servant.Client.Core
import Servant.Client
import System.IO
-- import System.FilePath
import Text.Trifecta as TF hiding (line,err,try,newline)

import Pact.ApiReq
import Pact.Compile
import Pact.Eval
import Pact.Native
-- intentionally hidden unused functions to prevent lib functions from consuming gas
import Pact.Native.Internal hiding (defRNative,defGasRNative,defNative)
import Pact.Parse
import Pact.Repl
import Pact.Repl.Lib
import Pact.Repl.Types
import Pact.Types.API
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Server.API

data KeyPairFile = KeyPairFile Text Text
instance FromJSON KeyPairFile where
  parseJSON = withObject "KeyPairFile" $ \o ->
    KeyPairFile <$> o .: "public" <*> o .: "secret"

cliDefs :: NativeModule
cliDefs = ("Cli",
     [
      defZNative "local" local'
      (funType a [("exec",a)] <>
       funType a [])
      [LitExample "(local (+ 1 2))",LitExample "(local)"]
      "Evaluate EXEC on server, or without argument, current code value (see 'code')."
      ,
      defZRNative "eval-local" evalLocal
      (funType a [("exp",a)])
      [LitExample "(eval \"(+ 1 2)\")"]
      "Evaluate EXP locally on server."
      ,
      defZNative "add-cap" addCap
      (funType tTyString [("cap",TyFun $ funType' tTyBool []),("signers",TyList (tTyString))])
      [LitExample "(add-cap (coin.TRANSFER \"alice\" \"bob\" 10.0) [\"alice\"])"]
      "Add signer capability CAP for SIGNERS"
      ,
      defZRNative "import" import'
      (funType tTyString [("modules", TyList (tTyString))])
      [LitExample "(import ['fungible-v2,'coin])"]
      "Import and load code for MODULES from remote."
      ,
      defZRNative "add-keyset" addKeyset
      (funType (TyList (tTyObject TyAny)) [("keyset",tTyGuard (Just GTyKeySet))])
      [LitExample "(add-keyset (local (describe-keyset 'abc)))"]
      "Add all keys in KEYSET to keystore, returning keydata objects."
      ,
      defZNative "code" code'
      (funType tTyString [("exprs",a)])
      [LitExample "(code (+ 1 2) (/ 3 4))"]
      "Store EXPRS as current command code."
      ,
      defZRNative "code-file" codeFile
      (funType tTyString [("file",a)])
      [LitExample "(code-file \"transfers.repl\")"]
      "Set current command code to contents of FILE"
      ,
      defZRNative "rehash" rehash
      (funType tTyString [])
      [LitExample "(rehash)"]
      "Build current command and hash, and update 'env-hash' accordingly."
      ,
      defZRNative "add-keyfile" addKeyfile
      (funType tTyString [("keyfile",tTyString)])
      [LitExample "(add-keyfile \"keys/000.yaml\")"]
      "Add a key yaml file for signing."
      ,
      defZRNative "now" now (funType tTyTime []) [LitExample "(now)"]
      "Return current time"
      ,
      defZRNative "send" send (funType tTyString []) [LitExample "(send)"]
      "Build and send current command."
      ,
      defZRNative "poll" poll
      (funType a [] <> funType a [("req-key",tTyString)])
      [LitExample "(poll)", LitExample "(poll \"DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g\")"]
      "Poll for result on server."
      ,
      defZRNative "prep-offline" prepOffline
      (funType tTyString [("file",tTyString)])
      [LitExample "(prep-offline \"offline-unsigned.yaml\")"]
      "Make offline 'unsigned' yaml FILE."
      ,
      defZRNative "read-offline" readOffline
      (funType tTyString [("file",tTyString)])
      [LitExample "(read-offline \"sig.yaml\")"]
      "Read offline signature yaml FILE."
      ,
      defZRNative "show" showCmd
      (funType tTyString [])
      [LitExample "(show)"]
      "Show/print current command."
     ])

  where
       a = mkTyVar "a" []


addKeyset :: RNativeFun LibState
addKeyset _ [TGuard (GKeySet KeySet {..}) _] = do
  fmap (toTList TyAny def) $ forM (S.toList _ksKeys) $ \k ->
    evalPact1 $ "(cli.add-key \"" <> renderCompactText k <> "\")"
addKeyset i as = argsError i as

instance Pretty Y.ParseException where
  pretty = pretty . show

addKeyfile :: RNativeFun LibState
addKeyfile i [TLitString f'] = do
  f <- computeCurPath f'
  (KeyPairFile p _s) <- eitherDie i =<< liftIO (Y.decodeFileEither f)
  evalApp i "cli.add-keyfile1" [tStr p, tStr $ pack f]
addKeyfile i as = argsError i as

import' :: RNativeFun LibState
import' i as = do
  ms <- forM as $ \a -> case a of
    TLitString m -> return $ "(describe-module \"" <> m <> "\")"
    _ -> evalError' a "Expected string"
  mds <- localExec i False $ "[" <> (intercalate " " ms) <> "]"
  rs <- case mds of
    PList vs -> forM vs $ \md ->
      case (preview (_PAt "code" . _PString) md, preview (_PAt "name". _PString) md) of
        (Just code,Just mname) -> case fromString (unpack mname) of
          (ModuleName _ (Just (NamespaceName ns))) ->
            evalPact' ("(namespace \"" <> ns <> "\") " <> code)
          _ -> evalPact' code
        (_,_) -> evalError' i "Expected code, module name"
    _ -> evalError' i "Expected list"
  return $ toTList TyAny def (concat rs)

now :: RNativeFun LibState
now _ _ = toTerm <$> liftIO getCurrentTime


prepOffline :: RNativeFun LibState
prepOffline i [TLitString file] = do
  cmd <- buildCurrentCode i
  sgs <- getSigners i
  f <- computeCurPath file
  liftIO $ Y.encodeFile @(SigData Text) f $
      SigData (_cmdHash cmd) (map toSigs (fst sgs)) Nothing
  return $ tStr $ "Wrote " <> (pack f)
  where
    toSigs (Signer _ s _ _) = (PublicKeyHex s,Nothing)
prepOffline i as = argsError i as

computeCurPath :: Text -> Eval LibState FilePath
computeCurPath file = do
  curFileM <- viewLibState _rlsFile
  return $ computePath curFileM (unpack file)

readOffline :: RNativeFun LibState
readOffline i [TLitString file] = do
  SigData{..} <- eitherDie i =<< liftIO (Y.decodeFileEither @(SigData Text) (unpack file))
  cmd <- buildCurrentCode i
  when (_cmdHash cmd /= _sigDataHash) $ evalError' i $ "Sig data file hash mismatch: " <> pretty _sigDataHash
  rs <- forM _sigDataSigs $ \(PublicKeyHex k,sigM) -> forM sigM $ \(UserSig sig) ->
    evalApp i "cli.sign" [toTerm k,toTerm sig]
  return $ toTList TyAny def $ catMaybes rs

readOffline i as = argsError i as

termToCode :: Term Ref -> Text
termToCode (TLiteral l _) = renderCompactText l
termToCode (TList vs _ _) = "[" <> intercalate " " (V.toList $ termToCode <$> vs) <> "]"
termToCode (TObject (Object (ObjectMap om) _ ko _) _) =
  "{" <> intercalate ", " (map go (psort ko $ M.toList $ (termToCode <$> om))) <> "}"
  where psort Nothing = id
        psort (Just o) = sortBy (compare `on` ((`elemIndex` o) . fst))
        go (k,v) = renderCompactText k <> ": " <> v
termToCode (TApp (App f as _) _) =
  "(" <> termToCode f <> " " <> intercalate " " (map termToCode as) <> ")"
termToCode (TConst (Arg n _ _) mn _ _ _) = case mn of
  Nothing -> n
  Just m -> renderCompactText m <> "." <> n
termToCode (TVar v _) = case v of
  Direct d -> termToCode $ fmap (\_ -> error "Direct with var unsupported") d
  Ref r -> termToCode r
termToCode TNative {..} = renderCompactText _tNativeName
termToCode (TDef Def {..} _) = renderCompactText _dModule <> "." <> renderCompactText _dDefName
termToCode t = renderCompactText t

buildCurrentCode :: HasInfo i => i -> Eval LibState (Command Text)
buildCurrentCode i = do
  code <- cliState i "code" (_PLiteral . _LString)
  buildCmd i True code

jsonToTerm
  :: (ToJSON b, HasInfo i, Show b) => i -> b -> Eval e (Term Name)
jsonToTerm i r = case fromJSON $ toJSON r of
    Aeson.Success s -> return $ fromPactValue s
    e -> evalError' i $ "Failed to coerce JSON: " <> (pretty $ show (e,r))

valueToTerm
  :: HasInfo i => i -> Value -> Eval e (Term Name)
valueToTerm i v = case fromJSON v of
    Aeson.Success s -> return $ fromPactValue s
    e -> evalError' i $ "Failed to coerce JSON: " <> (pretty $ show (e,v))

send :: RNativeFun LibState
send i _ = do
  cmd <- buildCurrentCode i
  env <- buildEndpoint i
  r <- sendTx i env (mkSubmitBatch cmd [])
  jsonToTerm i r

poll :: RNativeFun LibState
poll i as = case as of
  [TLitString k] -> (go . RequestKey) =<< eitherDie i (fromText' k)
  [] -> do
    cmd <- buildCurrentCode i
    go $ RequestKey $ toUntypedHash $ _cmdHash cmd
  _ -> argsError i as
  where
    go :: RequestKey -> Eval LibState (Term Name)
    go rk = do
      env <- buildEndpoint i
      PollResponses r <- sendPoll i env (mkPoll rk [])
      case HM.lookup rk r of
        Just CommandResult{..} -> do
          met <- valueToTerm i (maybe (String "[empty]") id _crMetaData)
          return $ toTObject TyAny def $
            [("result", case _crResult of
                 (PactResult (Left e)) -> tStr (tShow e)
                 (PactResult (Right sr)) -> fromPactValue sr)
            ,("gas",toTerm @Int $ fromIntegral _crGas)
            ,("meta",met)
            ,("events",toTList TyAny def $ map ev _crEvents)
            ]
        Nothing -> return $ tStr "No response"
    ev PactEvent{..} = toTObject TyAny def $
      [("name",toTerm _eventName)
      ,("params",toTList TyAny def (map fromPactValue _eventParams))
      ,("module",toTerm (renderCompactText _eventModule))
      ,("module-hash",toTerm (renderCompactText _eventModuleHash))]

local' :: ZNativeFun LibState
local' i [] = do
  code <- cliState i "code" (_PLiteral . _LString)
  fromPactValue <$> localExec i True code
local' i as = do
  let code = intercalate " " $ map termToCode as
  fromPactValue <$> localExec i False code

evalLocal :: RNativeFun LibState
evalLocal i [TLitString e] = do
  fromPactValue <$> localExec i False e
evalLocal i as = argsError i as

code' :: ZNativeFun LibState
code' i as = do
  let code = intercalate " " $ map termToCode as
  evalApp i "cli.set-code" [tStr code]

codeFile :: RNativeFun LibState
codeFile i [TLitString f] = do
  f' <- computeCurPath f
  code <- liftIO $ readFile f'
  evalApp i "cli.set-code" [tStr $ pack code]
codeFile i as = argsError i as

rehash :: RNativeFun LibState
rehash i _ = do
  code <- cliState i "code" (_PLiteral . _LString)
  cmd <- buildCmd i True code
  evalApp i "env-hash" $ [tStr $ asString $ _cmdHash cmd]

showCmd :: RNativeFun LibState
showCmd i _ = do
  code <- cliState i "code" (_PLiteral . _LString)
  c@Command{..} <- buildCmd i True code
  liftIO $ do
    putStrLn "Hash:"
    BS.putStr $ "  " <> Y.encode _cmdHash
    putStrLn "Sigs:"
    forM_ _cmdSigs $ \s -> BS.putStr $ "  " <> Y.encode s
    putStrLn "Payload:"
    BS.putStr . Y.encode . decodeStrict @Value . encodeUtf8 $ _cmdPayload
    putStrLn "JSON:"
    BSL.putStrLn $ encode c
    return $ tStr "---"




localExec :: HasInfo i => i -> Bool -> Text -> Eval LibState PactValue
localExec i includeSigners code = do
  cmd <- buildCmd i includeSigners code
  env <- buildEndpoint i
  r <- sendLocal i env cmd
  case _pactResult (_crResult r) of
    Left e -> throwM e
    Right v -> return v

evalAppExpect :: HasInfo i => i -> Text -> [Term Name] -> Fold PactValue a -> Eval e a
evalAppExpect i an as f = evalApp i an as >>= toPV >>= \r -> case preview f r of
    Nothing -> evalError' i $ "unexpected result for app " <> pretty (an,as)
    Just v -> return v

evalApp :: HasInfo i => i -> Text -> [Term Name] -> Eval e (Term Name)
evalApp i an as = do
  let qn = parseQualifiedName (getInfo i) an
      nn = case qn of
        Left {} -> Name (BareName an def)
        Right q -> QName $ q
  eval (TApp (App (TVar nn def) as def) def)

addCap :: ZNativeFun LibState
addCap i [TApp (App n as _) _,signers] = do
  let name = tStr $ termToCode n
  as' <- mapM reduce as
  signers' <- reduce signers
  evalApp i "cli.add-cap1"
      [ name
      , TList (V.fromList as') TyAny def
      , signers' ]


addCap i as = argsError' i as

cliState :: HasInfo i => i -> FieldKey -> Fold PactValue a -> Eval LibState a
cliState i k p = evalExpect i (asString k) (_head . _PAt k . p) "(cli.cli-state)"

buildEndpoint :: HasInfo i => i -> Eval LibState ClientEnv
buildEndpoint i = do
  cid <- cliState i "chain-id" (_PLiteral . _LInteger)
  pactCid <- evalExpect1 i "integer" (_PLiteral . _LInteger) "PACT_CHAIN_ID"
  let isPact = cid == pactCid
  host <- cliState i "host" (_PLiteral . _LString)
  nw <- cliState i "network-id" (_PLiteral . _LString)
  let url | isPact = "http://" <> host <> "/"
          | otherwise = "https://" <> host <> "/chainweb/0.0/" <> nw <> "/chain/" <> tShow cid <> "/pact"
  burl <- parseBaseUrl (unpack url)
  -- mgr <- liftIO $ newManager defaultManagerSettings
  liftIO $ getClientEnv burl

_PString :: Fold PactValue Text
_PString = _PLiteral . _LString
_PInteger :: Fold PactValue Integer
_PInteger = _PLiteral . _LInteger
_PDecimal :: Fold PactValue Decimal
_PDecimal = _PLiteral . _LDecimal
_PTime :: Fold PactValue UTCTime
_PTime = _PLiteral . _LTime
_PBool :: Fold PactValue Bool
_PBool = _PLiteral . _LBool
_PObjectMap :: Fold PactValue (M.Map FieldKey PactValue)
_PObjectMap = _PObject . objectMap
_PAt :: FieldKey -> Fold PactValue PactValue
_PAt k = _PObjectMap . ix k

buildCmd :: HasInfo i => i -> Bool -> Text -> Eval LibState (Command Text)
buildCmd i includeSigners cmd = do
  ttl <- cliState i "ttl" _PInteger
  nw <- NetworkId <$> cliState i "network-id" _PString
  ctime <- cliState i "creation-time" _PTime
  nonce <- cliState i "nonce" _PString
  cid <- cliState i "chain-id" _PInteger
  sender <- cliState i "sender" _PString
  gasLimit <- view (eeGasEnv . geGasLimit)
  gasPrice <- view (eeGasEnv . geGasPrice)
  md <- view eeMsgBody
  let toCT = TxCreationTime . fromIntegral . (`div` 1000000) . toMicroseconds . utcTimeToPOSIXSeconds
  (signers,sigs) <- if includeSigners then getSigners i else return ([],Nothing)
  cmdu <- liftIO $ mkUnsignedExec
      cmd
      md
      (PublicMeta (ChainId (tShow cid))
       sender gasLimit gasPrice (fromIntegral ttl)
       (toCT ctime))
      signers
      (Just nw)
      (Just nonce)
  return $ case sigs of
    Nothing -> cmdu
    Just ss -> set cmdSigs ss cmdu

getSigners :: HasInfo i => i -> Eval LibState ([Signer],Maybe [UserSig])
getSigners i = do
  sss <- cliState i "signers" _PList >>= \ss -> forM (V.toList ss) $ \s ->
    case (str s "signer",str s "signature") of
      (Just a,Just b)
          | b /= "" -> return (a, Just $ UserSig b)
          | otherwise -> do
              ks <- evalAppExpect i "cli.get-key" [tStr a] (_PAt "file" . _PString)
              case ks of
                "" -> return (a, Nothing)
                kf -> ((a,) . Just) <$> signHashWithFile (unpack kf)
      _ -> evalError' i $ "invalid signer: " <> pretty s
  caps <- fmap (M.fromListWith (++) . concat) $ cliState i "caps" _PList >>= \ss -> forM (V.toList ss) $ \s ->
    case (str s "name", lst s "args", lst s "signers") of
        (Just n,Just as,Just css) -> do
          qn <- eitherDie i $ parseQualifiedName (getInfo i) n
          let cap = [SigCapability qn (V.toList as)]
          forM (V.toList css) $ \cs -> case preview _PString cs of
            Just ps -> return (ps,cap)
            _ -> evalError' i $ "invalid signer in cap: " <> pretty cs
        _ -> evalError' i $ "invalid cap: " <> pretty s
  return (map (mkSigner caps) (map fst sss),sequence (map snd sss))
  where
    str o k = preview (_PAt k . _PString) o
    lst o k = preview (_PAt k . _PList) o
    mkSigner caps s = Signer Nothing s Nothing $ case M.lookup s caps of
      Nothing -> []
      Just cs -> cs


signHashWithFile :: FilePath -> Eval LibState UserSig
signHashWithFile fp = do
  skp <- liftIO $ importKeyFile fp
  h <- view eeHash
  liftIO $ signHash (fromUntypedHash h) skp

getClientEnv :: BaseUrl -> IO ClientEnv
getClientEnv url = flip mkClientEnv url <$> newTlsManagerWith mgrSettings
    where
      mgrSettings = mkManagerSettings
       (TLSSettingsSimple True False False)
       Nothing

evalExpect1 :: HasInfo i => i -> Text -> Fold PactValue a -> Text -> Eval LibState a
evalExpect1 i msg f = evalExpect i msg (_head . f)

evalExpect :: HasInfo i => i -> Text -> Fold [PactValue] a -> Text -> Eval LibState a
evalExpect i msg f cmd = do
  r <- evalPactValue cmd
  case preview f r of
    Nothing -> evalError' i $ "Expected " <> pretty msg <> " for command " <> pretty cmd
    Just v -> return v

evalPactValue :: Text -> Eval e [PactValue]
evalPactValue e = evalPact' e >>= traverse toPV

toPV :: Term Name -> Eval e PactValue
toPV t = eitherDie t $ toPactValue t

evalPact1 :: Text -> Eval e (Term Name)
evalPact1 = fmap head . evalPact'

evalPact' :: Text -> Eval e [Term Name]
evalPact' cmd = compilePact cmd >>= mapM eval

compilePact :: Text -> Eval e [Term Name]
compilePact cmd = case TF.parseString exprsOnly mempty (unpack cmd) of
  TF.Success es -> mapM go es
  TF.Failure f -> evalError def $ unAnnotate $ _errDoc f
  where
    go e = case compile (mkTextInfo cmd) e of
      Right t -> return t
      Left l -> evalError (peInfo l) (peDoc l)


loadCli :: Maybe FilePath -> Repl ()
loadCli _confm = do
  rEnv . eeRefStore . rsNatives %= HM.union (moduleToMap cliDefs)
  void $ loadFile def "cli/cli.repl"


_cli :: IO ()
_cli = do
  s <- initReplState Interactive Nothing
  void $ (`evalStateT` s) $ do
    useReplLib
    loadCli Nothing
    forever $ pipeLoop True stdin Nothing

_eval :: Eval LibState a -> IO a
_eval e = do
  s <- initReplState Interactive Nothing
  (r,_) <- (`evalStateT` s) $ do
    useReplLib
    loadCli Nothing
    evalEval def e
  either (error . show) return r

_run :: String -> IO ()
_run f = do
  r <- _eval $ evalPact' $ pack f
  mapM_ (putStrLn . unpack . renderCompactText) r

_testCode :: Text -> IO [Text]
_testCode code = _eval (fmap termToCode <$> (compilePact code >>= mapM enscope))


sendTx :: HasInfo i => i -> ClientEnv -> SubmitBatch -> Eval e RequestKeys
sendTx i env sb =
  liftIO (runClientM (sendClient sb) env) >>= eitherDie i

sendLocal :: HasInfo i => i -> ClientEnv -> Command Text -> Eval e (CommandResult Hash)
sendLocal i env cmd =
  liftIO (runClientM (localClient cmd) env) >>= eitherDie i

sendPoll :: HasInfo i => i -> ClientEnv -> Poll -> Eval e (PollResponses)
sendPoll i env p =
  liftIO (runClientM (pollClient p) env) >>= eitherDie i

eitherDie :: HasInfo i => Pretty a => i -> Either a b -> Eval e b
eitherDie i = either (evalError' i . pretty) return

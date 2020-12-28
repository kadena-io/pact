{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

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
import Data.Aeson hiding (Object)
import Data.Default
import Data.Function
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy,elemIndex)
import qualified Data.Set as S
import Data.Text (Text,unpack,intercalate)
import Data.Thyme.Time.Core
import Data.Thyme.Clock
import qualified Data.Vector as V
import Network.Connection
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client.TLS
import Servant.Client.Core
import Servant.Client
import Servant.Server
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



cliDefs :: NativeModule
cliDefs = ("Cli",
     [
      defZNative "local" local'
      (funType a [("exec",a)])
      [LitExample "(local (+ 1 2))"]
      "Evaluate EXEC on server."
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
      (funType tTyString [("keyset",tTyGuard (Just GTyKeySet))])
      [LitExample "(add-keyset (local (describe-keyset 'abc)))"]
      "Add all keys in KEYSET to keystore."
      ,
      defZNative "code" code'
      (funType tTyString [("exprs",a)])
      [LitExample "(code (+ 1 2) (/ 3 4))"]
      "Store EXPRS as current command code."
      ,
      defZRNative "rehash" rehash
      (funType tTyString [])
      [LitExample "(rehash)"]
      "Build current command and hash, and update 'env-hash' accordingly."
     ])

  where
       a = mkTyVar "a" []

addKeyset :: RNativeFun LibState
addKeyset _ [TGuard (GKeySet KeySet {..}) _] = do
  forM_ (S.toList _ksKeys) $ \k -> evalPact1 $ "(cli.add-key1 \"" <> renderCompactText k <> "\")"
  return $ tStr $ "Added " <> renderCompactText (S.toList _ksKeys) <> " to keystore"
addKeyset i as = argsError i as

import' :: RNativeFun LibState
import' i as = do
  ms <- forM as $ \a -> case a of
    TLitString m -> return $ "(describe-module \"" <> m <> "\")"
    _ -> evalError' a "Expected string"
  mds <- localExec i $ "[" <> (intercalate " " ms) <> "]"
  rs <- case mds of
    PList vs -> forM vs $ \md ->
      case preview (_PObject . to _objectMap . ix "code" . _PLiteral . _LString) md of
        Just code -> evalPact' code
        Nothing -> evalError' i "Expected code"
    _ -> evalError' i "Expected list"
  return $ toTList TyAny def (concat rs)


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

local' :: ZNativeFun LibState
local' i as = do
  let code = intercalate " " $ map termToCode as
  fromPactValue <$> localExec i code

code' :: ZNativeFun LibState
code' i as = do
  let code = intercalate " " $ map termToCode as
  evalApp i "cli.set-code" [tStr code]

rehash :: RNativeFun LibState
rehash i _ = do
  code <- cliState i "code" (_PLiteral . _LString)
  cmd <- buildCmd i True code
  evalApp i "env-hash" $ [tStr $ asString $ _cmdHash cmd]

localExec :: HasInfo i => i -> Text -> Eval LibState PactValue
localExec i code = do
  cmd <- buildCmd i False code
  env <- buildEndpoint i
  r <- sendLocal i env cmd
  case _pactResult (_crResult r) of
    Left e -> throwM e
    Right v -> return v


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
cliState i k p = evalExpect i (asString k) (_head . _PObject . to _objectMap . ix k . p) "(cli.cli-state)"

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

buildCmd :: HasInfo i => i -> Bool -> Text -> Eval LibState (Command Text)
buildCmd i includeSigners cmd = do
  ttl <- cliState i "ttl" (_PLiteral . _LInteger)
  nw <- NetworkId <$> cliState i "network-id" (_PLiteral . _LString)
  ctime <- cliState i "creation-time" (_PLiteral . _LTime)
  nonce <- cliState i "nonce" (_PLiteral . _LString)
  cid <- cliState i "chain-id" (_PLiteral . _LInteger)
  sender <- cliState i "sender" (_PLiteral . _LString)
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
    case preview (_PObject . to _objectMap) s of
      Just o -> case (str o "signer",str o "signature") of
        (Just a,Just b) | b == "" -> return (a,Nothing)
                        | otherwise -> return (a, Just $ UserSig b)
        _ -> evalError' i $ "invalid signer: " <> pretty s
      _ -> evalError' i $ "invalid signers"
  caps <- fmap (M.fromListWith (++) . concat) $ cliState i "caps" _PList >>= \ss -> forM (V.toList ss) $ \s ->
    case preview (_PObject . to _objectMap) s of
      Just o -> case (str o "name", lst o "args", lst o "signers") of
        (Just n,Just as,Just css) -> do
          qn <- eitherDie i $ parseQualifiedName (getInfo i) n
          let cap = [SigCapability qn (V.toList as)]
          forM (V.toList css) $ \cs -> case preview (_PLiteral . _LString) cs of
            Just ps -> return (ps,cap)
            _ -> evalError' i $ "invalid signer in cap: " <> pretty cs
        _ -> evalError' i $ "invalid cap: " <> pretty s
      _ -> evalError' i $ "invalid caps"
  return (map (mkSigner caps) (map fst sss),sequence (map snd sss))
  where
    str o k = preview (ix k . _PLiteral . _LString) o
    lst o k = preview (ix k . _PList) o
    mkSigner caps s = Signer Nothing s Nothing $ case M.lookup s caps of
      Nothing -> []
      Just cs -> cs


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
  where
    toPV t = eitherDie t $ toPactValue t

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

_testCode :: Text -> IO [Text]
_testCode code = _eval (fmap termToCode <$> (compilePact code >>= mapM enscope))


send :: HasInfo i => i -> ClientEnv -> SubmitBatch -> Eval e RequestKeys
send i env sb =
  liftIO (runClientM (sendClient sb) env) >>= eitherDie i

sendLocal :: HasInfo i => i -> ClientEnv -> Command Text -> Eval e (CommandResult Hash)
sendLocal i env cmd =
  liftIO (runClientM (localClient cmd) env) >>= eitherDie i

eitherDie :: HasInfo i => Pretty a => i -> Either a b -> Eval e b
eitherDie i = either (evalError' i . pretty) return

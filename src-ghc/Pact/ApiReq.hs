{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Pact.ApiReq
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.ApiReq
    (
     ApiKeyPair(..)
    ,ApiReq(..)
    ,apiReq
    ,apiReq'
    ,uapiReq
    ,mkApiReq
    ,mkExec
    ,mkCont
    ,mkKeyPairs
    ,SignReq(..),signReq
    ,AddSigsReq(..),addSigsReq
    ,SigData(..)
    ,commandToSigData
    ,sampleSigData
    ,combineSigs
    ,combineSigDatas
    ) where

import Control.Applicative
import Control.Error
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.Default (def)
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Thyme.Clock
import qualified Data.Yaml as Y
import GHC.Generics
import Prelude
import System.Directory
import System.FilePath

import Pact.Parse
import Pact.Types.API
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.SPV

-- | For fully-signed commands
data ApiKeyPair = ApiKeyPair {
  _akpSecret :: PrivateKeyBS,
  _akpPublic :: Maybe PublicKeyBS,
  _akpAddress :: Maybe Text,
  _akpScheme :: Maybe PPKScheme,
  _akpCaps :: Maybe [SigCapability]
  } deriving (Eq, Show, Generic)
instance ToJSON ApiKeyPair where toJSON = lensyToJSON 4
instance FromJSON ApiKeyPair where parseJSON = lensyParseJSON 4

-- | For unsigned commands
data ApiSigner = ApiSigner {
  _asPublic :: Text,
  _asAddress :: Maybe Text,
  _asScheme :: Maybe PPKScheme,
  _asCaps :: Maybe [SigCapability]
  } deriving (Eq, Show, Generic)
instance ToJSON ApiSigner where toJSON = lensyToJSON 3
instance FromJSON ApiSigner where parseJSON = lensyParseJSON 3

data ApiPublicMeta = ApiPublicMeta
  { _apmChainId :: Maybe ChainId
  , _apmSender :: Maybe Text
  , _apmGasLimit :: Maybe GasLimit
  , _apmGasPrice :: Maybe GasPrice
  , _apmTTL :: Maybe TTLSeconds
  , _apmCreationTime :: Maybe TxCreationTime
  } deriving (Eq, Show, Generic)

instance ToJSON ApiPublicMeta where
  toJSON (ApiPublicMeta cid s gl gp ttl ct) = object $ concat
    [ "chainId" .?= cid
    , "sender" .?= s
    , "gasLimit" .?= gl
    , "gasPrice" .?= gp
    , "ttl" .?= ttl
    , "creationTime" .?= ct
    ]
    where
      k .?= v = case v of
        Nothing -> mempty
        Just v' -> [k .= v']


instance FromJSON ApiPublicMeta where
  parseJSON = withObject "ApiPublicMeta" $ \o -> ApiPublicMeta
    <$> o .:? "chainId"
    <*> o .:? "sender"
    <*> o .:? "gasLimit"
    <*> o .:? "gasPrice"
    <*> o .:? "ttl"
    <*> o .:? "creationTime"


data ApiReq = ApiReq {
  _ylType :: Maybe String,
  _ylPactTxHash :: Maybe Hash,
  _ylStep :: Maybe Int,
  _ylRollback :: Maybe Bool,
  _ylData :: Maybe Value,
  _ylProof :: Maybe ContProof,
  _ylDataFile :: Maybe FilePath,
  _ylCode :: Maybe String,
  _ylCodeFile :: Maybe FilePath,
  _ylKeyPairs :: Maybe [ApiKeyPair],
  _ylSigners :: Maybe [ApiSigner],
  _ylNonce :: Maybe String,
  _ylPublicMeta :: Maybe ApiPublicMeta,
  _ylNetworkId :: Maybe NetworkId
  } deriving (Eq,Show,Generic)
instance ToJSON ApiReq where toJSON = lensyToJSON 3
instance FromJSON ApiReq where parseJSON = lensyParseJSON 3


data SignReq = SignReq
  { _srHash :: Hash
  , _srKeyPairs :: [ApiKeyPair]
  } deriving (Eq,Show,Generic)
instance ToJSON SignReq where toJSON = lensyToJSON 3
instance FromJSON SignReq where parseJSON = lensyParseJSON 3


data AddSigsReq = AddSigsReq
  { _asrUnsigned :: Command Text
  , _asrSigs :: [UserSig]
  } deriving (Eq,Show,Generic)
instance ToJSON AddSigsReq where toJSON = lensyToJSON 4
instance FromJSON AddSigsReq where parseJSON = lensyParseJSON 4

newtype PublicKeyHex = PublicKeyHex { unPublicKeyHex :: Text }
  deriving (Eq,Ord,Show,Generic)

instance IsString PublicKeyHex where
  fromString = PublicKeyHex . pack
instance ToJSONKey PublicKeyHex
instance FromJSONKey PublicKeyHex
instance ToJSON PublicKeyHex where
  toJSON (PublicKeyHex hex) = String hex
instance FromJSON PublicKeyHex where
  parseJSON = withText "PublicKeyHex" $ \t -> do
    if T.length t == 64 && isRight (parseB16TextOnly t)
      then return $ PublicKeyHex t
      else fail "Public key must have 64 hex characters"

-- | This type is designed to represent signatures in all possible situations
-- where they might be wanted. It must satisfy at least the following
-- requirements:
--
-- 1. It must support multisig.
-- 2. It must have values representing state both before and after all the
-- signatures have been supplied.
-- 3. It must be usable in a cold wallet setting where transfer may be high
-- latency with low bandwidth. This means having the option to leave out the
-- payload to facilitate easier transmission via QR codes and other low
-- bandwidth channels.
data SigData a = SigData
  { _sigDataHash :: PactHash
  , _sigDataSigs :: [(PublicKeyHex, Maybe UserSig)]
  -- ^ This is not a map because the order must be the same as the signers inside the command.
  , _sigDataCmd :: Maybe a
  } deriving (Eq,Show,Generic)

commandToSigData :: Command Text -> Either String (SigData Text)
commandToSigData c = do
  let ep = traverse parsePact =<< (eitherDecodeStrict' $ encodeUtf8 $ _cmdPayload c)
  case ep :: Either String (Payload Value ParsedCode) of
    Left e -> Left $ "Error decoding payload: " <> e
    Right p -> do
      let sigs = map (\s -> (PublicKeyHex $ _siPubKey s, Nothing)) $ _pSigners p
      Right $ SigData (_cmdHash c) sigs (Just $ _cmdPayload c)

sigDataToCommand :: SigData Text -> Either String (Command Text)
sigDataToCommand (SigData _ _ Nothing) = Left "Can't reconstruct command"
sigDataToCommand (SigData h sigList (Just c)) = do
  payload :: Payload Value ParsedCode <- traverse parsePact =<< eitherDecodeStrict' (encodeUtf8 c)
  let sigMap = M.fromList sigList
  let sigs = catMaybes $ map (\signer -> join $ M.lookup (PublicKeyHex $ _siPubKey signer) sigMap) $ _pSigners payload
  pure $ Command c sigs h

sampleSigData :: SigData Text
sampleSigData = SigData (either error id $ fromText' "b57_gSRIwDEo6SAYseppem57tykcEJkmbTFlCHDs0xc") [("acbe76b30ccaf57e269a0cd5eeeb7293e7e84c7d68e6244a64c4adf4d2df6ea1", Nothing)] Nothing

instance ToJSON a => ToJSON (SigData a) where
  toJSON (SigData h s c) = object $ concat
    [ ["hash" .= h]
    , ["sigs" .= object (map (\(k,ms) -> (unPublicKeyHex k, maybe Null (toJSON . _usSig) ms)) s)]
    , "cmd" .?= c
    ]
    where
      k .?= v = case v of
        Nothing -> mempty
        Just v' -> [k .= v']

instance FromJSON a => FromJSON (SigData a) where
  parseJSON = withObject "SigData" $ \o -> do
    h <- o .: "hash"
    s <- withObject "SigData Pairs" f =<< (o .: "sigs")
    c <- o .:? "cmd"
    pure $ SigData h s c
    where
      f = mapM g . HM.toList
      g (k,Null) = pure (PublicKeyHex k, Nothing)
      g (k,String t) = pure (PublicKeyHex k, Just $ UserSig t)
      g (_,v) = typeMismatch "Signature should be String or Null" v

combineSigs :: [FilePath] -> IO ()
combineSigs fs = do
  sigs <- mapM loadSigData fs
  case partitionEithers sigs of
    ([], rs) -> combineSigDatas rs
    (ls, _) -> do
      putStrLn "One or more files had errors:"
      mapM_ putStrLn ls

combineSigDatas :: [SigData Text] -> IO ()
combineSigDatas [] = error "Nothing to combine"
combineSigDatas sds@(sd:_) = do
  let hashes = S.fromList $ map _sigDataHash sds
      cmds = S.fromList $ map _sigDataCmd sds
  when (S.size hashes > 1 || S.size cmds > 1) $ do
    error "SigData files contain more than one unique hash or command.  Aborting..."
  let sigMap = foldl1 f $ map _sigDataSigs sds
  printCommandIfDone $ sd { _sigDataSigs = sigMap }
  where
    f accum sigs
      | length accum /= length sigs = error "Sig lists have different lengths"
      | otherwise = zipWith g accum sigs
    g (pAccum,sAccum) (p,s) =
      if pAccum /= p
        then error $ unlines [ "Sig mismatch:"
                             , show pAccum
                             , show p
                             , "All signatures must be in the same order"
                             ]
        else (pAccum, sAccum <|> s)


loadSigData :: FilePath -> IO (Either String (SigData Text))
loadSigData fp = do
  res <- Y.decodeFileEither fp
  return $ case res of
    Left e -> Left $ "Error loading SigData file " <> fp <> ": " <> show e
    Right sd -> Right sd

addSigToSigData :: SomeKeyPair -> SigData a -> IO (SigData a)
addSigToSigData kp sd = do
  sig <- signHash (_sigDataHash sd) kp
  let k = PublicKeyHex $ toB16Text $ getPublic kp
  return $ sd { _sigDataSigs = M.toList $ M.adjust (const $ Just sig) k $ M.fromList $ _sigDataSigs sd }

addSigsReq :: [FilePath] -> ByteString -> IO ()
addSigsReq keyFiles bs = do
  sd <- either (error . show) return $ Y.decodeEither' bs
  printCommandIfDone =<< foldM addSigReq sd keyFiles

printCommandIfDone :: SigData Text -> IO ()
printCommandIfDone sd =
  case sigDataToCommand sd of
    Left _ -> BS.putStrLn $ Y.encodeWith yamlOptions sd
    Right c -> do
      let res = verifyCommand $ fmap encodeUtf8 c
      case res :: ProcessedCommand Value ParsedCode of
        ProcSucc _ -> putJSON (SubmitBatch $ c :| [])
        ProcFail _ -> BS.putStrLn $ Y.encodeWith yamlOptions sd

addSigReq :: SigData Text -> FilePath -> IO (SigData Text)
addSigReq sd keyFile = do
  v :: Value <- decodeYaml keyFile

  let ekp = do
        -- These keys are from genKeys in Main.hs. Might want to convert to a
        -- dedicated data type at some point.
        pub <- parseB16TextOnly =<< (note "Error parsing public key" $ v ^? key "public" . _String)
        sec <- parseB16TextOnly =<< (note "Error parsing secret key" $ v ^? key "secret" . _String)

        importKeyPair defaultScheme (Just $ PubBS pub) (PrivBS sec)
  case ekp of
    Left e -> dieAR $ "Could not parse key file " <> keyFile <> ": " <> e
    Right kp -> addSigToSigData kp sd


yamlOptions :: Y.EncodeOptions
yamlOptions = Y.setFormat (Y.setWidth Nothing Y.defaultFormatOptions) Y.defaultEncodeOptions

apiReq :: FilePath -> Bool -> IO ()
apiReq f l = apiReq' f l False

apiReq' :: FilePath -> Bool -> Bool -> IO ()
apiReq' fp local unsignedReq = do
  (_,exec) <- mkApiReq' unsignedReq fp
  let doEncode :: ToJSON b => b -> IO ()
      doEncode | unsignedReq = BS.putStrLn . Y.encodeWith yamlOptions
               | otherwise = putJSON
  if local || unsignedReq then
    doEncode exec
    else
    doEncode $ SubmitBatch $ exec :| []

uapiReq :: FilePath -> Bool -> IO ()
uapiReq fp tiny = do
  (_,exec) <- mkApiReq' True fp
  let doEncode :: ToJSON b => b -> IO ()
      doEncode = BS.putStrLn . Y.encodeWith yamlOptions
      applyTiny a = if tiny then a { _sigDataCmd = Nothing } else a
  case commandToSigData exec of
    Left e -> dieAR $ "Error decoding command: " <> e
    Right a -> doEncode $ applyTiny a

mkApiReq :: FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReq fp = mkApiReq' False fp

mkApiReq' :: Bool -> FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReq' unsignedReq fp = do
  ar@ApiReq {..} <- decodeYaml fp
  case _ylType of
    Just "exec" -> mkApiReqExec unsignedReq ar fp
    Just "cont" -> mkApiReqCont unsignedReq ar fp
    Nothing -> mkApiReqExec unsignedReq ar fp -- Default
    _ -> dieAR "Expected a valid message type: either 'exec' or 'cont'"

decodeYaml :: FromJSON b => FilePath -> IO b
decodeYaml fp = either (dieAR . show) return =<<
                 liftIO (Y.decodeFileEither fp)

putJSON :: ToJSON b => b -> IO ()
putJSON = BSL.putStrLn . encode

signReq :: FilePath -> IO ()
signReq fp = do
  SignReq{..} <- decodeYaml fp
  kps <- mkKeyPairs _srKeyPairs
  sigs <- mapM (signHash (fromUntypedHash _srHash)) (map fst kps)
  BS.putStrLn $ Y.encode sigs

withKeypairsOrSigner
  :: Bool
  -> ApiReq
  -> ([SomeKeyPairCaps] -> IO a)
  -> ([Signer] -> IO a)
  -> IO a
withKeypairsOrSigner unsignedReq ApiReq{..} keypairAction signerAction =
  case (_ylSigners,_ylKeyPairs,unsignedReq) of
    (Nothing,Just kps,False) -> mkKeyPairs kps >>= keypairAction
    (Nothing,Nothing,False) -> keypairAction []
    (Just {},_,False) -> dieAR "'signers' invalid in command request"
    (Just ss,Nothing,True) -> signerAction $ map toSigner ss
    (Nothing,Nothing,True) -> dieAR "'signers' required for unsigned request"
    (_,Just {},True) -> dieAR "'keyPairs' invalid in unsigned request"
  where
    toSigner ApiSigner{..} = Signer _asScheme _asPublic _asAddress (fromMaybe [] _asCaps)


mkApiReqExec :: Bool -> ApiReq -> FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReqExec unsignedReq ar@ApiReq{..} fp = do
  (code,cdata) <- withCurrentDirectory (takeDirectory fp) $ do
    code <- case (_ylCodeFile,_ylCode) of
      (Nothing,Just c) -> return c
      (Just f,Nothing) -> liftIO (readFile f)
      _ -> dieAR "Expected either a 'code' or 'codeFile' entry"
    cdata <- case (_ylDataFile,_ylData) of
      (Nothing,Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f,Nothing) -> liftIO (BSL.readFile f) >>=
                          either (\e -> dieAR $ "Data file load failed: " ++ show e) return .
                          eitherDecode
      (Nothing,Nothing) -> return Null
      _ -> dieAR "Expected either a 'data' or 'dataFile' entry, or neither"
    return (code,cdata)
  pubMeta <- mkPubMeta _ylPublicMeta
  cmd <- withKeypairsOrSigner unsignedReq ar
    (\ks -> mkExec code cdata pubMeta ks _ylNetworkId _ylNonce)
    (\ss -> mkUnsignedExec code cdata pubMeta ss _ylNetworkId _ylNonce)
  return ((ar,code,cdata,pubMeta), cmd)

mkPubMeta :: Maybe ApiPublicMeta -> IO PublicMeta
mkPubMeta apm = case apm of
  Nothing -> return def
  (Just ApiPublicMeta {..}) -> do
    ct <- case _apmCreationTime of
      Nothing -> getCurrentCreationTime
      Just t -> return t
    return $ PublicMeta
      { _pmChainId = fromMaybe "" _apmChainId
      , _pmSender = fromMaybe "" _apmSender
      , _pmGasLimit = fromMaybe 0 _apmGasLimit
      , _pmGasPrice = fromMaybe 0 _apmGasPrice
      , _pmTTL = fromMaybe 1800 _apmTTL
      , _pmCreationTime = ct
      }




mkNonce :: Maybe String -> IO Text
mkNonce = maybe (pack . show <$> getCurrentTime) (return . pack)

-- | Construct an Exec request message
--
mkExec
  :: String
    -- ^ code
  -> Value
    -- ^ optional environment data
  -> PublicMeta
    -- ^ public metadata
  -> [SomeKeyPairCaps]
    -- ^ signing keypairs + caplists
  -> Maybe NetworkId
    -- ^ optional 'NetworkId'
  -> Maybe String
    -- ^ optional nonce
  -> IO (Command Text)
mkExec code mdata pubMeta kps nid ridm = do
  rid <- mkNonce ridm
  cmd <- mkCommand
         kps
         pubMeta
         rid
         nid
         (Exec (ExecMsg (pack code) mdata))
  return $ decodeUtf8 <$> cmd

-- | Construct an Exec request message
--
mkUnsignedExec
  :: String
    -- ^ code
  -> Value
    -- ^ optional environment data
  -> PublicMeta
    -- ^ public metadata
  -> [Signer]
    -- ^ payload signers
  -> Maybe NetworkId
    -- ^ optional 'NetworkId'
  -> Maybe String
    -- ^ optional nonce
  -> IO (Command Text)
mkUnsignedExec code mdata pubMeta kps nid ridm = do
  rid <- mkNonce ridm
  cmd <- mkUnsignedCommand
         kps
         pubMeta
         rid
         nid
         (Exec (ExecMsg (pack code) mdata))
  return $ decodeUtf8 <$> cmd


mkApiReqCont :: Bool -> ApiReq -> FilePath -> IO ((ApiReq,String,Value,PublicMeta),Command Text)
mkApiReqCont unsignedReq ar@ApiReq{..} fp = do
  apiPactId <- case _ylPactTxHash of
    Just t -> return t
    Nothing -> dieAR "Expected a 'pactTxHash' entry"

  step <- case _ylStep of
    Just s -> return s
    Nothing -> dieAR "Expected a 'step' entry"

  rollback <- case _ylRollback of
    Just r -> return r
    Nothing -> dieAR "Expected a 'rollback' entry"

  cdata <- withCurrentDirectory (takeDirectory fp) $ do
    case (_ylDataFile,_ylData) of
      (Nothing,Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f,Nothing) -> liftIO (BSL.readFile f) >>=
                          either (\e -> dieAR $ "Data file load failed: " ++ show e) return .
                          eitherDecode
      (Nothing,Nothing) -> return Null
      _ -> dieAR "Expected either a 'data' or 'dataFile' entry, or neither"
  let pactId = toPactId apiPactId
  pubMeta <- mkPubMeta _ylPublicMeta
  cmd <- withKeypairsOrSigner unsignedReq ar
    (\ks -> mkCont pactId step rollback cdata pubMeta ks _ylNonce _ylProof _ylNetworkId)
    (\ss -> mkUnsignedCont pactId step rollback cdata pubMeta ss _ylNonce _ylProof _ylNetworkId)
  return ((ar,"",cdata,pubMeta), cmd)

-- | Construct a Cont request message
--
mkCont
  :: PactId
    -- ^ pact tx hash of the continuation
  -> Int
    -- ^ cont step
  -> Bool
    -- ^ has rollback?
  -> Value
    -- ^ environment data
  -> PublicMeta
    -- ^ command public metadata
  -> [SomeKeyPairCaps]
    -- ^ signing keypairs
  -> Maybe String
    -- ^ optional nonce
  -> Maybe ContProof
    -- ^ optional continuation proof (required for cross-chain)
  -> Maybe NetworkId
    -- ^ optional network id
  -> IO (Command Text)
mkCont txid step rollback mdata pubMeta kps ridm proof nid = do
  rid <- mkNonce ridm
  cmd <- mkCommand
         kps
         pubMeta
         rid
         nid
         (Continuation (ContMsg txid step rollback mdata proof) :: (PactRPC ContMsg))
  return $ decodeUtf8 <$> cmd


-- | Construct a Cont request message
--
mkUnsignedCont
  :: PactId
    -- ^ pact tx hash of the continuation
  -> Int
    -- ^ cont step
  -> Bool
    -- ^ has rollback?
  -> Value
    -- ^ environment data
  -> PublicMeta
    -- ^ command public metadata
  -> [Signer]
    -- ^ payload signers
  -> Maybe String
    -- ^ optional nonce
  -> Maybe ContProof
    -- ^ optional continuation proof (required for cross-chain)
  -> Maybe NetworkId
    -- ^ optional network id
  -> IO (Command Text)
mkUnsignedCont txid step rollback mdata pubMeta kps ridm proof nid = do
  rid <- mkNonce ridm
  cmd <- mkUnsignedCommand
         kps
         pubMeta
         (pack $ show rid)
         nid
         (Continuation (ContMsg txid step rollback mdata proof) :: (PactRPC ContMsg))
  return $ decodeUtf8 <$> cmd

mkKeyPairs :: [ApiKeyPair] -> IO [SomeKeyPairCaps]
mkKeyPairs keyPairs = traverse mkPair keyPairs
  where importValidKeyPair ApiKeyPair{..} = fmap (,maybe [] id _akpCaps) $
          case _akpScheme of
            Nothing -> importKeyPair defaultScheme _akpPublic _akpSecret
            Just ppk -> importKeyPair (toScheme ppk) _akpPublic _akpSecret

        mkPair akp = case _akpAddress akp of
          Nothing -> either dieAR return (importValidKeyPair akp)
          Just addrT -> do
            addrBS <- either dieAR return (parseB16TextOnly addrT)
            kp     <- either dieAR return (importValidKeyPair akp)

            -- Enforces that user provided address matches the address derived from the Public Key
            -- for transparency and a better user experience. User provided address not used except
            -- for this purpose.

            case (addrBS, formatPublicKey (fst kp)) of
              (expectAddr, actualAddr)
                | expectAddr == actualAddr  -> return kp
                | otherwise                 -> dieAR $ "Address provided "
                                               ++ show (toB16Text expectAddr)
                                               ++ " does not match actual Address "
                                               ++ show (toB16Text actualAddr)

dieAR :: String -> IO a
dieAR errMsg = throwM . userError $ intercalate "\n" $
  ["Failure reading request yaml. Valid fields are: "
  ,"Common fields:"
  ,"  type: 'exec' or 'cont', indicating Exec or Cont message type"
  ,"  data|dataFile: (optional) JSON transaction data, as yaml (data) or a yaml file (dataFile)"
  ,"  keyPairs: list of key pairs for signing (use pact -g to generate): ["
  ,"    public: base 16 public key"
  ,"    secret: base 16 secret key"
  ,"    address: (required for ETH) base 16 address"
  ,"    scheme: optional, 'ETH' or 'ED25519', default ED25519"
  ,"    caps: capability list as strings, in form \"[(module.CAP param1 param2)]\""
  ,"    ] "
  ,"  nonce: (optional) request nonce, defaults to current time"
  ,"  publicMeta: (optional) data for public-chain execution: ["
  ,"    chainId: chain or shard identifier"
  ,"    sender: gas-paying sender account"
  ,"    gasLimit: integer gas max limit"
  ,"    gasPrice: decimal gas unit price"
  ,"    ttl: TTL value in seconds"
  ,"    creationTime: epoch time integer value in seconds"
  ,"Exec-only fields:"
  ,"  code|codeFile: [exec only] Pact code, as a string (code) or file path (codeFile)"
  ,"Cont-only fields:"
  ,"  pactTxHash: pact ID to continue"
  ,"  step: step index to continue"
  ,"  rollback: rollback/cancel flag"
  ,"  proof: platform-specific continuation proof data"
  ,"Error message: " ++ errMsg
  ]

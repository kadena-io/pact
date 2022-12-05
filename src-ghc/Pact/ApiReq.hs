{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Pact.ApiReq
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.ApiReq
    (
     ApiKeyPair(..)
    ,ApiSigner(..)
    ,ApiPublicMeta(..)
    ,ApiReq(..)
    ,apiReq
    ,uapiReq
    ,mkApiReq
    ,mkApiReqCmd
    ,ApiReqParts
    ,mkExec
    ,mkCont
    ,mkKeyPairs
    ,AddSigsReq(..)
    ,addSigsReq
    ,combineSigs
    ,combineSigDatas
    ,signCmd
    ,decodeYaml
    ,returnCommandIfDone
    ,loadSigData
    ) where

import Control.Applicative
import Control.Error
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.Default (def)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Foldable(foldrM)
import Pact.Time
import qualified Data.Yaml as Y
import GHC.Generics
import Prelude
import System.Exit hiding (die)
import System.FilePath
import System.IO

import Pact.Parse
import Pact.Types.API
import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.RPC
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Types.SigData
import Pact.Types.SPV
import Pact.Utils.LegacyValue

-- | For fully-signed commands
data ApiKeyPair = ApiKeyPair {
  _akpSecret :: PrivateKeyBS,
  _akpPublic :: Maybe PublicKeyBS,
  _akpAddress :: Maybe Text,
  _akpScheme :: Maybe PPKScheme,
  _akpCaps :: Maybe [SigCapability]
  } deriving (Eq, Show, Generic)

apiKeyPairProperties :: JsonProperties ApiKeyPair
apiKeyPairProperties o =
  [ "address" .= _akpAddress o
  , "secret" .= _akpSecret o
  , "scheme" .= _akpScheme o
  , "caps" .= _akpCaps o
  , "public" .= _akpPublic o
  ]
{-# INLINE apiKeyPairProperties #-}

instance ToJSON ApiKeyPair where
  toJSON = enableToJSON "Pact.ApiReq.ApiKeyPair" . lensyToJSON 4
  toEncoding = pairs . mconcat . apiKeyPairProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON ApiKeyPair where parseJSON = lensyParseJSON 4

-- | For unsigned commands
data ApiSigner = ApiSigner {
  _asPublic :: Text,
  _asAddress :: Maybe Text,
  _asScheme :: Maybe PPKScheme,
  _asCaps :: Maybe [SigCapability]
  } deriving (Eq, Show, Generic)

apiSignerProperties :: JsonProperties ApiSigner
apiSignerProperties o =
  [ "address" .= _asAddress o
  , "scheme" .= _asScheme o
  , "caps" .= _asCaps o
  , "public" .= _asPublic o
  ]
{-# INLINE apiSignerProperties #-}

instance ToJSON ApiSigner where
  toJSON = enableToJSON "Pact.ApiReq.ApiSigner" . lensyToJSON 3
  toEncoding = pairs . mconcat . apiSignerProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON ApiSigner where parseJSON = lensyParseJSON 3

data ApiPublicMeta = ApiPublicMeta
  { _apmChainId :: Maybe ChainId
  , _apmSender :: Maybe Text
  , _apmGasLimit :: Maybe GasLimit
  , _apmGasPrice :: Maybe GasPrice
  , _apmTTL :: Maybe TTLSeconds
  , _apmCreationTime :: Maybe TxCreationTime
  } deriving (Eq, Show, Generic)

apiPublicMetaProperties :: Monoid kv => KeyValue kv => ApiPublicMeta -> [kv]
apiPublicMetaProperties o =
  [ "creationTime" .?= _apmCreationTime o
  , "ttl" .?= _apmTTL o
  , "gasLimit" .?= _apmGasLimit o
  , "chainId" .?= _apmChainId o
  , "gasPrice" .?= _apmGasPrice o
  , "sender" .?= _apmSender o
  ]
{-# INLINE apiPublicMetaProperties #-}

instance ToJSON ApiPublicMeta where
  toJSON = enableToJSON "Pact.ApiReq.ApiPublicMeta" . Data.Aeson.Object . mconcat . apiPublicMetaProperties
  toEncoding = pairs . mconcat . apiPublicMetaProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}


instance FromJSON ApiPublicMeta where
  parseJSON = withObject "ApiPublicMeta" $ \o -> ApiPublicMeta
    <$> o .:? "chainId"
    <*> o .:? "sender"
    <*> o .:? "gasLimit"
    <*> o .:? "gasPrice"
    <*> o .:? "ttl"
    <*> o .:? "creationTime"


data ApiReq = ApiReq {
  _ylType :: Maybe Text,
  _ylPactTxHash :: Maybe Hash,
  _ylStep :: Maybe Int,
  _ylRollback :: Maybe Bool,
  _ylData :: Maybe Value,
  _ylProof :: Maybe ContProof,
  _ylDataFile :: Maybe FilePath,
  _ylCode :: Maybe Text,
  _ylCodeFile :: Maybe FilePath,
  _ylKeyPairs :: Maybe [ApiKeyPair],
  _ylSigners :: Maybe [ApiSigner],
  _ylNonce :: Maybe Text,
  _ylPublicMeta :: Maybe ApiPublicMeta,
  _ylNetworkId :: Maybe NetworkId
  } deriving (Eq,Show,Generic)

apiReqProperties :: JsonProperties ApiReq
apiReqProperties o =
  [ "publicMeta" .= _ylPublicMeta o
  , "proof" .= _ylProof o
  , "data" .= _ylData o
  , "networkId" .= _ylNetworkId o
  , "rollback" .= _ylRollback o
  , "signers" .= _ylSigners o
  , "step" .= _ylStep o
  , "code" .= _ylCode o
  , "pactTxHash" .= _ylPactTxHash o
  , "type" .= _ylType o
  , "codeFile" .= _ylCodeFile o
  , "keyPairs" .= _ylKeyPairs o
  , "dataFile" .= _ylDataFile o
  , "nonce" .= _ylNonce o
  ]
{-# INLINE apiReqProperties #-}

instance ToJSON ApiReq where
  toJSON = enableToJSON "Pact.ApiReq.ApiReq" . lensyToJSON 3
  toEncoding = pairs . mconcat . apiReqProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON ApiReq where parseJSON = lensyParseJSON 3

data AddSigsReq = AddSigsReq
  { _asrUnsigned :: Command Text
  , _asrSigs :: [UserSig]
  } deriving (Eq,Show,Generic)

addSigsReqProperties :: JsonProperties AddSigsReq
addSigsReqProperties o =
  [ "sigs" .= _asrSigs o
  , "unsigned" .= _asrUnsigned o
  ]
{-# INLINE addSigsReqProperties #-}

instance ToJSON AddSigsReq where
  toJSON = enableToJSON "Pact.ApiReq.AddSigsReq" . object . addSigsReqProperties
  toEncoding = pairs . mconcat . addSigsReqProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON AddSigsReq where
    parseJSON = withObject "AddSigsReq" $ \o -> AddSigsReq
        <$> o .: "unsigned"
        <*> o .: "sigs"

combineSigs :: [FilePath] -> Bool -> IO ByteString
combineSigs fs outputLocal = do
  sigs <- mapM loadSigData fs
  case partitionEithers sigs of
    ([], rs) -> combineSigDatas rs outputLocal
    (ls, _) -> do
      error $ unlines $ "One or more files had errors:" : ls

combineSigDatas :: [SigData Text] -> Bool -> IO ByteString
combineSigDatas [] _ = error "Nothing to combine"
combineSigDatas sds outputLocal = do
  let hashes = S.fromList $ map _sigDataHash sds
      cmds = S.fromList $ mapMaybe _sigDataCmd sds
  when (S.size hashes /= 1 || S.size cmds /= 1) $ do
    error "SigData files must contain exactly one unique hash and command.  Aborting..."
  let sigs = foldl1 f $ map _sigDataSigs sds
  returnCommandIfDone outputLocal $ SigData (head $ S.toList hashes) sigs (Just $ head $ S.toList cmds)
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
  return $ sd { _sigDataSigs = addSigToList k sig $ _sigDataSigs sd }

addSigToList
  :: PublicKeyHex
  -> UserSig
  -> [(PublicKeyHex, Maybe UserSig)]
  -> [(PublicKeyHex, Maybe UserSig)]
addSigToList _ _ [] = []
addSigToList k s ((pk,pus):ps) =
  if k == pk
    then (pk, Just s) : addSigToList k s ps
    else (pk, pus) : addSigToList k s ps

addSigsReq :: [FilePath] -> Bool -> ByteString -> IO ByteString
addSigsReq keyFiles outputLocal bs = do
  sd <- either (error . show) return $ Y.decodeEither' bs
  returnSigDataOrCommand outputLocal =<< foldM addSigReq sd keyFiles

-- | `returnSigDataOrCommand` will either:
--   - validate partial sig(s) added for a particular SigData, then re-serialize as SigData
--   - validate all signatures if all signatures are provided, and output the resulting `Command`
--   TODO: `IO ByteString` is fairly opaque, low hanging fruit to provide (Either Command SigData)
--   and serialize upstream most likely, but is not of much concern atm.
returnSigDataOrCommand :: Bool -> SigData Text -> IO ByteString
returnSigDataOrCommand  outputLocal sd
  | isPartialSigData = do
    case verifyPartialSigData sd of
      Right _ ->
        pure $ Y.encodeWith yamlOptions sd
      Left e -> do
        let msg = unlines ["Command verification failed!", e]
        hPutStrLn stderr msg >> hFlush stderr >> exitFailure
  | otherwise = returnCommandIfDone outputLocal sd
  where
  isPartialSigData = any (isn't _Just . snd) (_sigDataSigs sd)
  verifyPartialSigData (SigData h sigs (Just cmd)) = do
    payload :: Payload Value ParsedCode <- traverse parsePact =<< eitherDecodeStrict' (encodeUtf8 cmd)
    let sigMap = Map.fromList sigs
    when (length (_pSigners payload) /= length sigs) $
      Left "Number of signers in the payload does not match number of signers in the sigData"
    usrSigs <- traverse (toSignerPair sigMap) (_pSigners payload)
    let failedSigs = filter (not . verifySig h) usrSigs
    when (length failedSigs /= 0) $ Left $ "Invalid sig(s) found: " ++ show (encode <$> failedSigs)
    _ <- verifyHash h (encodeUtf8 cmd)
    pure ()
    where
    verifySig hsh (signer, usrSig) = case usrSig of
      Nothing -> True
      Just sig -> verifyUserSig hsh sig signer
    toSignerPair sigMap signer =
      case Map.lookup (PublicKeyHex $ _siPubKey signer) sigMap of
        Nothing -> Left $ "Signer in payload does not show up in signatures" <> show (_siPubKey signer)
        Just v -> pure (signer, v)
  verifyPartialSigData (SigData h sigs Nothing) = do
    sigs' <- foldrM toVerifPair [] sigs
    let scheme = toScheme ED25519
        failedSigs = filter (\(pk, sig) -> not $ verify scheme (toUntypedHash h) pk sig) sigs'
    when (length failedSigs /= 0) $ Left $ "Invalid sig(s) found: " ++ show (encode <$> failedSigs)
    pure ()
    where
    toVerifPair (PublicKeyHex pktext, Just UserSig{..}) m = do
      pk <- PubBS <$> parseB16TextOnly pktext
      sig <- SigBS <$> parseB16TextOnly _usSig
      pure $ (pk, sig):m
    toVerifPair (_, Nothing) m = pure m

returnCommandIfDone :: Bool -> SigData Text -> IO ByteString
returnCommandIfDone outputLocal sd =
  case sigDataToCommand sd of
    Left _ -> return $ Y.encodeWith yamlOptions sd
    Right c -> do
      let res = verifyCommand $ fmap encodeUtf8 c
          out = if outputLocal then encode c else encode (SubmitBatch (c :| []))
      case res :: ProcessedCommand Value ParsedCode of
        ProcSucc _ -> pure $ BSL.toStrict out
        ProcFail e -> do
          let msg = unlines ["Command verification failed!", e]
          hPutStrLn stderr msg >> hFlush stderr >> exitFailure

addSigReq :: SigData Text -> FilePath -> IO (SigData Text)
addSigReq sd keyFile = do
  kp <- importKeyFile keyFile
  addSigToSigData kp sd

importKeyFile :: FilePath -> IO SomeKeyPair
importKeyFile keyFile = do
  v :: Value <- decodeYaml keyFile
  let ekp = do
        -- These keys are from genKeys in Main.hs. Might want to convert to a
        -- dedicated data type at some point.
        pub <- parseB16TextOnly =<< note "Error parsing public key" (v ^? key "public" . _String)
        sec <- parseB16TextOnly =<< note "Error parsing secret key" (v ^? key "secret" . _String)

        importKeyPair defaultScheme (Just $ PubBS pub) (PrivBS sec)
  case ekp of
    Left e -> dieAR $ "Could not parse key file " <> keyFile <> ": " <> e
    Right kp -> return kp

yamlOptions :: Y.EncodeOptions
yamlOptions = Y.setFormat (Y.setWidth Nothing Y.defaultFormatOptions) Y.defaultEncodeOptions

apiReq :: FilePath -> Bool -> IO ()
apiReq fp local = do
  (_,exec) <- mkApiReq' False fp
  if local then
    putJSON exec
    else
    putJSON $ SubmitBatch $ exec :| []

uapiReq :: FilePath -> IO ()
uapiReq fp = do
  (_,exec) <- mkApiReq' True fp
  let doEncode :: ToJSON b => b -> IO ()
      doEncode = BS.putStrLn . Y.encodeWith yamlOptions
  case commandToSigData exec of
    Left e -> dieAR $ "Error decoding command: " <> e
    Right a -> doEncode a

-- | parts read/rationalized from a processed ApiReq:
-- the ApiReq, code, msg data, PublicMeta
type ApiReqParts = (ApiReq,Text,Value,PublicMeta)

-- | Assemble a command and parts from a ApiReq YAML file.
mkApiReq :: FilePath -> IO (ApiReqParts,Command Text)
mkApiReq fp = mkApiReq' False fp

mkApiReq' :: Bool -> FilePath -> IO (ApiReqParts,Command Text)
mkApiReq' unsignedReq fp = mkApiReqCmd unsignedReq fp =<< decodeYaml fp

-- | Assemble a command and parts from an ApiReq.
mkApiReqCmd
  :: Bool
     -- ^ make "unsigned command" for offline signing
  -> FilePath
     -- ^ filepath for chdir for loading files
  -> ApiReq
     -- ^ the ApiReq
  -> IO (ApiReqParts, Command Text)
mkApiReqCmd unsignedReq fp ar@ApiReq{..} =
  case _ylType of
    Just "exec" -> mkApiReqExec unsignedReq ar fp
    Just "cont" -> mkApiReqCont unsignedReq ar fp
    Nothing -> mkApiReqExec unsignedReq ar fp -- Default
    _ -> dieAR "Expected a valid message type: either 'exec' or 'cont'"

-- | Decode yaml or fail in IO.
decodeYaml :: FromJSON b => FilePath -> IO b
decodeYaml fp = either (dieAR . show) return =<<
                 liftIO (Y.decodeFileEither fp)

putJSON :: ToJSON b => b -> IO ()
putJSON = BSL.putStrLn . encode

-- | The formatting of the result and in particular the sorting items in the
-- result is not specified. Do not use this function if deterministc and
-- repeatble formatting is needed.
--
signCmd
  :: [FilePath]
  -> ByteString
  -- ^ Takes a base64url encoded ByteString
  -> IO ByteString
signCmd keyFiles bs = do
  case decodeBase64UrlUnpadded bs of
    Left e -> dieAR $ "stdin was not valid base64url: " <> e
    Right h -> do
      kps <- mapM importKeyFile keyFiles
      fmap (Y.encode . HM.fromList) $ forM kps $ \kp -> do
            sig <- signHash (fromUntypedHash $ Hash h) kp
            return (B16JsonBytes (getPublic kp), _usSig sig)

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


mkApiReqExec :: Bool -> ApiReq -> FilePath -> IO (ApiReqParts,Command Text)
mkApiReqExec unsignedReq ar@ApiReq{..} fp = do
  (code,cdata) <- do
    let dir = takeDirectory fp
    code <- case (_ylCodeFile,_ylCode) of
      (Nothing,Just c) -> return c
      (Just f,Nothing) -> liftIO (decodeUtf8 <$> BS.readFile (dir </> f))
      _ -> dieAR "Expected either a 'code' or 'codeFile' entry"
    cdata <- case (_ylDataFile,_ylData) of
      (Nothing,Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f,Nothing) -> liftIO (BSL.readFile (dir </> f)) >>=
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




mkNonce :: Maybe Text -> IO Text
mkNonce = maybe (pack . show <$> getCurrentTime) return

-- | Construct an Exec request message
--
mkExec
  :: Text
    -- ^ code
  -> Value
    -- ^ optional environment data
  -> PublicMeta
    -- ^ public metadata
  -> [SomeKeyPairCaps]
    -- ^ signing keypairs + caplists
  -> Maybe NetworkId
    -- ^ optional 'NetworkId'
  -> Maybe Text
    -- ^ optional nonce
  -> IO (Command Text)
mkExec code mdata pubMeta kps nid ridm = do
  rid <- mkNonce ridm
  cmd <- mkCommand
         kps
         pubMeta
         rid
         nid
         (Exec (ExecMsg code (toLegacyJson mdata)))
  return $ decodeUtf8 <$> cmd

-- | Construct an Exec request message
--
mkUnsignedExec
  :: Text
    -- ^ code
  -> Value
    -- ^ optional environment data
  -> PublicMeta
    -- ^ public metadata
  -> [Signer]
    -- ^ payload signers
  -> Maybe NetworkId
    -- ^ optional 'NetworkId'
  -> Maybe Text
    -- ^ optional nonce
  -> IO (Command Text)
mkUnsignedExec code mdata pubMeta kps nid ridm = do
  rid <- mkNonce ridm
  cmd <- mkUnsignedCommand
         kps
         pubMeta
         rid
         nid
         (Exec (ExecMsg code (toLegacyJson mdata)))
  return $ decodeUtf8 <$> cmd


mkApiReqCont :: Bool -> ApiReq -> FilePath -> IO (ApiReqParts,Command Text)
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

  cdata <- do
    let dir = takeDirectory fp
    case (_ylDataFile,_ylData) of
      (Nothing,Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f,Nothing) -> liftIO (BSL.readFile (dir </> f)) >>=
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
  -> Maybe Text
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
         (Continuation (ContMsg txid step rollback (toLegacyJson mdata) proof) :: (PactRPC ContMsg))
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
  -> Maybe Text
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
         (Continuation (ContMsg txid step rollback (toLegacyJson mdata) proof) :: (PactRPC ContMsg))
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

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
    ,mkApiReq
    ,mkExec
    ,mkCont
    ,mkKeyPairs
    ,SignReq(..),signReq
    ,AddSigsReq(..),addSigsReq
    ,signCmd
    ,signCmdFile
    ,normalizeSigs
    ,validateNonemptySigs
    ) where

import Control.Applicative
import Control.Error
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.Default (def)
import Data.Int
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Thyme.Clock
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
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



apiReq :: FilePath -> Bool -> IO ()
apiReq f l = apiReq' f l False

apiReq' :: FilePath -> Bool -> Bool -> IO ()
apiReq' fp local unsignedReq = do
  (_,exec) <- mkApiReq' unsignedReq fp
  let doEncode :: ToJSON b => b -> IO ()
      doEncode | unsignedReq = BS.putStrLn . Y.encodeWith options
               | otherwise = putJSON
      options = Y.setFormat (Y.setWidth Nothing Y.defaultFormatOptions) Y.defaultEncodeOptions
  if local || unsignedReq then
    doEncode exec
    else
    doEncode $ SubmitBatch $ exec :| []

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

signCmdFile :: FilePath -> FilePath -> IO ()
signCmdFile cmdFile keyFile = do
  c :: Command Text <- decodeYaml cmdFile
  v :: Value <- decodeYaml keyFile

  let ekp = do
        -- These keys are from genKeys in Main.hs. Might want to convert to a
        -- dedicated data type at some point.
        pub <- parseB16TextOnly =<< (note "Error parsing public key" $ v ^? key "public" . _String)
        sec <- parseB16TextOnly =<< (note "Error parsing secret key" $ v ^? key "secret" . _String)

        payload <- traverse parsePact =<< (eitherDecodeStrict' $ encodeUtf8 $ _cmdPayload c)
        kp <- importKeyPair defaultScheme (Just $ PubBS pub) (PrivBS sec)
        return (kp, payload :: Payload Value ParsedCode)
  case ekp of
    Left e -> dieAR $ "Could not parse key file: " <> e
    Right (kp, payload) -> do
      c2 <- signCmd kp $ normalizeSigs $ Command payload (_cmdSigs c) (_cmdHash c)
      BS.putStrLn $ Y.encode (c2 { _cmdPayload = _cmdPayload c })

-- | Normalizes a 'Command'. This function is idempotent and the returned
-- 'Command' will satisfy the following properties:
--
-- * The sigs array will be the same length as the signers array
-- * All sigs in the input command that are valid for any of the signers will be present in the output command
-- * All non-empty sigs will be valid signatures for the corresponding signers entry
-- * All other elements of the sigs array will be empty strings
-- * normalizeSigs c >>= validateNonemptySigs == True
--
-- TODO Write tests for these properties
normalizeSigs :: Command (Payload Value ParsedCode) -> Command (Payload Value ParsedCode)
normalizeSigs c@Command{..} = do
  if numSigners == numSigs && validateNonemptySigs c
    then c
    else Command _cmdPayload (V.toList normalizedSigs) _cmdHash
  where
    signers = _pSigners _cmdPayload
    numSigners = length signers
    numSigs = length _cmdSigs
    initialSigs = V.replicate numSigners (UserSig "")
    normalizedSigs = V.modify (\v -> mapM_ (addSig v) _cmdSigs) initialSigs
    addSig v s =
      case findSigLocation _cmdHash signers s of
        Nothing -> return ()
        Just i -> MV.write v i s

-- | Signs a 'Command' with the supplied key pair.
signCmd :: SomeKeyPair -> Command (Payload v p) -> IO (Command (Payload v p))
signCmd kp c = do
  sig <- signHash (_cmdHash c) kp
  let signers = _pSigners $ _cmdPayload c
  let numSigners = length signers
  let initialSigs =
        if length (_cmdSigs c) == numSigners
          then V.fromList $ _cmdSigs c
          else V.replicate numSigners (UserSig "")
      sigs = V.modify (addSigs (_cmdHash c) signers sig) initialSigs
  return $ Command (_cmdPayload c) (V.toList sigs) (_cmdHash c)
  where
    addSigs h signers newSig sigs =
      case findSigLocation h signers newSig of
        Nothing -> return ()
        Just i -> MV.write sigs i newSig

-- | Returns Left if there was some kind of decoding error, otherwise returns
-- Right Bool indicating whether the nonempty signatures in the command were
-- valid.
validateNonemptySigs :: Command (Payload Value ParsedCode) -> Bool
validateNonemptySigs Command{..} =
  all (\(sig, signer) -> emptySig sig || verifyUserSig _cmdHash sig signer) sigSignerPairs
  where
    sigSignerPairs = zip _cmdSigs $ _pSigners _cmdPayload
    emptySig (UserSig s) = s == ""

-- | Returns the index of a particular 'UserSig' in a list of signers or Nothing
-- if the signature does not match any of the signers.
findSigLocation :: PactHash -> [Signer] -> UserSig -> Maybe Int
findSigLocation h signers sig = go 0 signers
  where
    go _ [] = Nothing
    go i (s:ss) = if verifyUserSig h sig s then Just i else go (i+1) ss

addSigsReq :: FilePath -> Bool -> IO ()
addSigsReq fp local = do
  AddSigsReq{..} <- decodeYaml fp
  let c = cmd _asrUnsigned _asrSigs
  case verifyCommand (encodeUtf8 <$> c) of
    ProcFail s -> dieAR $ "Validate failed: " ++ s
    ProcSucc (_ :: Command (Payload Value ParsedCode)) -> return ()
  if local then
    putJSON c
    else
    putJSON $ SubmitBatch $ c :| []
  where
    cmd Command{..} sigs = Command _cmdPayload sigs _cmdHash

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

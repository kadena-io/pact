{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module SchemeSpec (spec) where

import Test.Hspec
import System.IO.Error
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString          as BS
import Data.Aeson as A
import qualified Control.Lens             as Lens
import qualified Data.ByteString.Base16   as B16
import qualified Data.ByteString.Lazy     as BSL

import Pact.ApiReq
import Pact.Types.Crypto
import Pact.Types.Command
import Pact.Types.Util (toB16Text, fromJSON')
import Pact.Types.RPC
import Pact.Types.Hash


---- HELPER DATA TYPES AND FUNCTIONS ----

getByteString :: ByteString -> ByteString
getByteString = fst . B16.decode


type Address = Maybe Text

getKeyPairComponents :: SomeKeyPairCaps -> (PublicKeyBS, PrivateKeyBS, Address, PPKScheme)
getKeyPairComponents (kp,_) = (PubBS $ getPublic kp,
                           PrivBS $ getPrivate kp,
                           addy,
                           scheme)
  where
    scheme = kpToPPKScheme kp
    addy = case scheme of
      ED25519 -> Nothing
      ETH -> Just $ toB16Text $ formatPublicKey kp


someED25519Pair :: (PublicKeyBS, PrivateKeyBS, Address, PPKScheme)
someED25519Pair = (PubBS $ getByteString
                   "ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d",
                   PrivBS $ getByteString
                   "8693e641ae2bbe9ea802c736f42027b03f86afe63cae315e7169c9c496c17332",
                   Nothing,
                   ED25519)




someETHPair :: (PublicKeyBS, PrivateKeyBS, Address, PPKScheme)
someETHPair = (PubBS $ getByteString
               "836b35a026743e823a90a0ee3b91bf615c6a757e2b60b9e1dc1826fd0dd16106f7bc1e8179f665015f43c6c81f39062fc2086ed849625c06e04697698b21855e",
               PrivBS $ getByteString
               "208065a247edbe5df4d86fbdc0171303f23a76961be9f6013850dd2bdc759bbb",
               Just $ "0bed7abd61247635c1973eb38474a2516ed1d884",
               ETH)


toApiKeyPairs :: [(PublicKeyBS, PrivateKeyBS, Address, PPKScheme)] -> [ApiKeyPair]
toApiKeyPairs kps = map makeAKP kps
  where makeAKP (pub, priv, add, scheme) =
          ApiKeyPair priv (Just pub) add (Just scheme) Nothing


mkCommandTest :: [SomeKeyPairCaps] -> [Signer] -> Text -> IO (Command ByteString)
mkCommandTest kps signers code = mkCommand' kps $ toExecPayload signers code


toSigners :: [(PublicKeyBS, PrivateKeyBS, Address, PPKScheme)] -> IO [Signer]
toSigners kps = return $ map makeSigner kps
  where makeSigner (PubBS pub, _, add, scheme) =
          Signer (Just scheme) (toB16Text pub) add []


toExecPayload :: [Signer] -> Text -> ByteString
toExecPayload signers t = BSL.toStrict $ A.encode payload
  where
    payload = Payload (Exec (ExecMsg t Null)) "nonce" () signers Nothing


shouldBeProcFail ::  ProcessedCommand () ParsedCode -> Expectation
shouldBeProcFail pcmd = pcmd `shouldSatisfy` isProcFail
  where isProcFail result = case result of
          ProcFail _ -> True
          _ -> False



---- HSPEC TESTS ----

#if !defined(ghcjs_HOST_OS)
spec :: Spec
spec = describe "working with crypto schemes" $ do
  describe "test importing Key Pair for each Scheme" testKeyPairImport
  describe "test default scheme in ApiKeyPair" testDefSchemeApiKeyPair
  describe "test for correct address in ApiKeyPair" testAddrApiKeyPair
  describe "test PublicKey import" testPublicKeyImport
  describe "test UserSig creation and verificaton" testUserSig
  describe "test signature non-malleability" testSigNonMalleability
  describe "testSigsRoundtrip" testSigsRoundtrip
#else
spec = return ()
#endif

testKeyPairImport :: Spec
testKeyPairImport = do
  it "imports ED25519 Key Pair" $ do
    kp <- mkKeyPairs (toApiKeyPairs [someED25519Pair])
    (map getKeyPairComponents kp) `shouldBe` [someED25519Pair]

  it "imports ETH Key Pair" $ do
    kp <- mkKeyPairs (toApiKeyPairs [someETHPair])
    (map getKeyPairComponents kp) `shouldBe` [someETHPair]



testDefSchemeApiKeyPair :: Spec
testDefSchemeApiKeyPair =
  context "when scheme not provided in API" $
    it "makes the scheme the default PPKScheme" $ do
      let (pub, priv, addr, _) = someED25519Pair
          apiKP = ApiKeyPair priv (Just pub) addr Nothing Nothing
      kp <- mkKeyPairs [apiKP]
      (map getKeyPairComponents kp) `shouldBe` [someED25519Pair]



testAddrApiKeyPair :: Spec
testAddrApiKeyPair =
  it "throws error when address provided in API doesn't match derived address" $ do
     let (pub, priv, _, scheme) = someETHPair
         apiKP = ApiKeyPair priv (Just pub)
                 (Just "9f491e44a3f87df60d6cb0eefd5a9083ae6c3f32") (Just scheme)
                 Nothing
     mkKeyPairs [apiKP] `shouldThrow` isUserError



testPublicKeyImport :: Spec
testPublicKeyImport = do
  it "derives PublicKey from the PrivateKey when PublicKey not provided" $ do
    let (_, priv, addr, scheme) = someETHPair
        apiKP = ApiKeyPair priv Nothing addr (Just scheme) Nothing
    kp <- mkKeyPairs [apiKP]
    (map getKeyPairComponents kp) `shouldBe` [someETHPair]


  it "throws error when PublicKey provided does not match derived PublicKey" $ do
    let (_, priv, addr, scheme) = someETHPair
        fakePub = PubBS $ getByteString
                  "c640e94730fb7b7fce01b11086645741fcb5174d1c634888b9d146613730243a171833259cd7dab9b3435421dcb2816d3efa55033ff0899de6cc8b1e0b20e56c"
        apiKP   = ApiKeyPair priv (Just fakePub) addr (Just scheme) Nothing
    mkKeyPairs [apiKP] `shouldThrow` isUserError


testUserSig :: Spec
testUserSig = do
  it "successfully verifies user-provided ETH Signature" $ do
    let hsh = hash "(somePactFunction)"
        userSig = UserSig
                  "780c2a6d11baae240a2888c4cfa7243dabba26b6121e68d0ea3b3dff779024c0c847eff27c6499ea29e7aea5f5744b98e550f6e3f7514d08c6cc2230564a1339"
    [signer] <- toSigners [someETHPair]
    (verifyUserSig hsh userSig signer) `shouldBe` True



  it "fails UserSig validation when UserSig has unexpected Address" $ do
    let hsh = hash "(somePactFunction)"
        (_,_,wrongAddr,_) = someETHPair
    [signer] <- toSigners [someED25519Pair]
    [(kp,_)]     <- mkKeyPairs $ toApiKeyPairs [someED25519Pair]
    sig      <- sign kp (toUntypedHash hsh)
    let myUserSig   = UserSig (toB16Text sig)
        wrongSigner = Lens.set siAddress wrongAddr signer
    (verifyUserSig hsh myUserSig wrongSigner) `shouldBe` False



  it "fails UserSig validation when UserSig has unexpected Scheme" $ do
    let hsh = hash "(somePactFunction)"
    [signer] <- toSigners [someED25519Pair]
    [(kp,_)]     <- mkKeyPairs $ toApiKeyPairs [someED25519Pair]
    sig      <- sign kp (toUntypedHash hsh)
    let myUserSig   = UserSig (toB16Text sig)
        wrongScheme = ETH
        wrongSigner = Lens.set siScheme (Just wrongScheme) signer
    (verifyUserSig hsh myUserSig wrongSigner) `shouldBe` False



  it "provides default ppkscheme when one not provided" $ do
    let sigJSON = A.object ["addr" .= String "SomeAddr", "pubKey" .= String "SomePubKey",
                            "sig" .= String "SomeSig"]
        sig     = UserSig "SomeSig"
    (fromJSON' sigJSON) `shouldBe` (Right sig)



  it "makes address field the full public key when one not provided" $ do
    let sigJSON = A.object ["pubKey" .= String "SomePubKey", "sig" .= String "SomeSig"]
        sig     = UserSig "SomeSig"
    (fromJSON' sigJSON) `shouldBe` (Right sig)



testSigNonMalleability :: Spec
testSigNonMalleability = do
  it "fails when invalid signature provided for signer specified in the payload" $ do
    wrongSigners <- toSigners [someED25519Pair]
    kps          <- mkKeyPairs $ toApiKeyPairs [someETHPair]

    cmdWithWrongSig <- mkCommandTest kps wrongSigners "(somePactFunction)"
    shouldBeProcFail (verifyCommand cmdWithWrongSig)



  it "fails when number of signatures does not match number of payload signers" $ do
    [signer]  <- toSigners [someETHPair]
    [kp]      <- mkKeyPairs $ toApiKeyPairs [someETHPair]
    [wrongKp] <- mkKeyPairs $ toApiKeyPairs [someED25519Pair]

    cmdWithWrongNumSig <- mkCommandTest [kp, wrongKp] [signer] "(somePactFunction)"
    shouldBeProcFail (verifyCommand cmdWithWrongNumSig)

testSigsRoundtrip :: Spec
testSigsRoundtrip = runIO $ do
  uapiReq "tests/sign-scripts/unsigned-exec.yaml"
  uapiReq "tests/sign-scripts/unsigned-cont.yaml"

  signRes <- signCmd ["tests/sign-scripts/key.yaml"] "i1S2rUgEyfBl393oWEwts3DzuyCvraemXA9A1Bno6sg"
  signRes `shouldBe` "7d0c9ba189927df85c8c54f8b5c8acd76c1d27e923abbf25a957afdf25550804: c72ac57ac1f03cd264b4e0db1ef681894e42b02b5ddcb115ee2f776ba8048c2afd15a4c1e46b20248bb015ba395689a90ac93b5193173f3af6e495b4ce09ce03\n"

  addSigsRes <- addSigsReq ["tests/sign-scripts/key.yaml"] False =<< BS.readFile "tests/sign-scripts/add-sigs.yaml"
  addSigsExpected <- BS.readFile "tests/sign-scripts/addSigsExpected.yaml"
  T.strip (decodeUtf8 addSigsRes) `shouldBe` T.strip (decodeUtf8 addSigsExpected)

  combineSigsRes <- combineSigs ["tests/sign-scripts/add-sigs.yaml", "tests/sign-scripts/bare-sig.yaml"] False
  combineSigsExpected <- BS.readFile "tests/sign-scripts/combineSigsExpected.yaml"
  T.strip (decodeUtf8 combineSigsRes) `shouldBe` T.strip (decodeUtf8 combineSigsExpected)

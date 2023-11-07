{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SchemeSpec (spec) where

import Test.Hspec
import System.IO.Error
import qualified Data.ByteString.Base16 as Base16
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString          as BS
import Data.Aeson as A
import qualified Control.Lens             as Lens
import qualified Data.ByteString.Base16   as B16

import Pact.ApiReq
import Pact.Types.Crypto
import Pact.Types.Command
import Pact.Types.Util (toB16Text, fromJSON', fromText')
import Pact.Types.RPC
import Pact.Types.Hash
import Pact.JSON.Legacy.Value
import qualified Pact.JSON.Encode as J


---- HELPER DATA TYPES AND FUNCTIONS ----

getByteString :: ByteString -> ByteString
getByteString = fromRight (error "Expected valid base-16") . B16.decode


type Address = Maybe Text

getKeyPairComponents :: Ed25519KeyPairCaps -> (PublicKeyBS, PrivateKeyBS, Address, PPKScheme)
getKeyPairComponents (kp,_) = (PubBS $ getPublic kp,
                           PrivBS $ getPrivate kp,
                           Nothing,
                           ED25519)

someED25519Pair :: (PublicKeyBS, PrivateKeyBS, Address, PPKScheme)
someED25519Pair = (PubBS $ getByteString
                   "ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d",
                   PrivBS $ getByteString
                   "8693e641ae2bbe9ea802c736f42027b03f86afe63cae315e7169c9c496c17332",
                   Nothing,
                   ED25519)

anotherED25519Pair :: (PublicKeyBS, PrivateKeyBS, Address, PPKScheme)
anotherED25519Pair = (PubBS $ getByteString
                      "6866b33e7935752bb972f363fe0567902616075878392ff7159f5fd4a2672827",
                      PrivBS $ getByteString
                      "7693e641ae2bbe9ea802c736f42027b03f86afe63cae315e7169c9c496c17331",
                      Just "6866b33e7935752bb972f363fe0567902616075878392ff7159f5fd4a2672827",
                      ED25519)

someWebAuthnSignature :: (UserSig, PublicKeyBS)
someWebAuthnSignature = (sig, pubKey)
  where
    sig = UserSig "{\"authenticatorData\":\"+cNxurbmvuKrkAKBTgIRX89NPS7FT5KydvqIN951zwoBAAAADQ==\",\"clientDataJSON\":\"eyJ0eXBlIjoid2ViYXV0aG4uZ2V0IiwiY2hhbGxlbmdlIjoiTkFDbG5makJiT2o3R2ZuRTg2YzJOZVZHaTBZUkRKcllidUF0cmhFUzJiYyIsIm9yaWdpbiI6Imh0dHBzOi8vZ3JlZy10ZXN0aW5nLTIwMjMtMDItMDcuZ2l0aHViLmlvIiwiY3Jvc3NPcmlnaW4iOmZhbHNlfQ\",\"signature\":\"MEYCIQDwQF19+Wjxs0boANssWEKoUFKhwHgiaycIeU5kRlY+RwIhAIAfCOUDVHr5aCrVQ1pbvCEw1xkeF0s4yjD48sDe9uO7\"}"
    pubKey = case Base16.decode "a5010203262001215820025b213619e0cbeadf7a4c62784f865d61c4da9268c724fa133efcf90ca7e00222582062ab25b410da272d9f2505b509bf599ac04f34888fad7cbb107d368add79edf1" of
      Left _ -> error "Hex pubkey is valid."
      Right k -> PubBS k

toApiKeyPairs :: [(PublicKeyBS, PrivateKeyBS, Address, PPKScheme)] -> [ApiKeyPair]
toApiKeyPairs kps = map makeAKP kps
  where makeAKP (pub, priv, add, scheme) =
          ApiKeyPair priv (Just pub) add (Just scheme) Nothing


mkCommandTest :: [Ed25519KeyPairCaps] -> [Signer] -> Text -> IO (Command ByteString)
mkCommandTest kps signers code = mkCommand' kps $ toExecPayload signers code


toSigners :: [(PublicKeyBS, PrivateKeyBS, Address, PPKScheme)] -> IO [Signer]
toSigners kps = return $ map makeSigner kps
  where makeSigner (PubBS pub, _, add, scheme) =
          Signer (Just scheme) (toB16Text pub) add []


toExecPayload :: [Signer] -> Text -> ByteString
toExecPayload signers t = J.encodeStrict payload
  where
    payload = Payload (Exec (ExecMsg t $ toLegacyJson Null)) "nonce" (J.Aeson ()) signers [] Nothing


shouldBeProcFail ::  ProcessedCommand () ParsedCode -> Expectation
shouldBeProcFail pcmd = pcmd `shouldSatisfy` isProcFail
  where isProcFail result = case result of
          ProcFail _ -> True
          _ -> False



---- HSPEC TESTS ----

spec :: Spec
spec = describe "working with crypto schemes" $ do
  describe "test importing Key Pair for each Scheme" testKeyPairImport
  describe "test default scheme in ApiKeyPair" testDefSchemeApiKeyPair
  describe "test PublicKey import" testPublicKeyImport
  describe "test signature non-malleability" testSigNonMalleability
  describe "testSigsRoundtrip" testSigsRoundtrip
  describe "test webauthn signature verification" verifyWebAuthnSignature

testKeyPairImport :: Spec
testKeyPairImport = do
  it "imports ED25519 Key Pair" $ do
    kp <- mkKeyPairs (toApiKeyPairs [someED25519Pair])
    (map getKeyPairComponents kp) `shouldBe` [someED25519Pair]


testDefSchemeApiKeyPair :: Spec
testDefSchemeApiKeyPair =
  context "when scheme not provided in API" $
    it "makes the scheme the default PPKScheme" $ do
      let (pub, priv, addr, _) = someED25519Pair
          apiKP = ApiKeyPair priv (Just pub) addr Nothing Nothing
      kp <- mkKeyPairs [apiKP]
      (map getKeyPairComponents kp) `shouldBe` [someED25519Pair]



testPublicKeyImport :: Spec
testPublicKeyImport = do
  it "derives PublicKey from the PrivateKey when PublicKey not provided" $ do
    let (_, priv, addr, scheme) = someED25519Pair
        apiKP = ApiKeyPair priv Nothing addr (Just scheme) Nothing
    kp <- mkKeyPairs [apiKP]
    (map getKeyPairComponents kp) `shouldBe` [someED25519Pair]


  it "throws error when PublicKey provided does not match derived PublicKey" $ do
    let (_, priv, addr, scheme) = someED25519Pair
        fakePub = PubBS $ getByteString
                  "c640e94730fb7b7fce01b11086645741fcb5174d1c634888b9d146613730243a171833259cd7dab9b3435421dcb2816d3efa55033ff0899de6cc8b1e0b20e56c"
        apiKP   = ApiKeyPair priv (Just fakePub) addr (Just scheme) Nothing
    mkKeyPairs [apiKP] `shouldThrow` isUserError



  it "fails UserSig validation when UserSig has unexpected Address" $ do
    let hsh = hash "(somePactFunction)"
        (_,_,wrongAddr,_) = anotherED25519Pair
    [signer] <- toSigners [someED25519Pair]
    [((pubKey, privKey),_)]     <- mkKeyPairs $ toApiKeyPairs [someED25519Pair]
    sig      <- sign pubKey privKey (toUntypedHash hsh)
    let myUserSig   = UserSig (toB16Text sig)
        wrongSigner = Lens.set siAddress wrongAddr signer
    (verifyUserSig hsh myUserSig wrongSigner) `shouldBe` False



  it "fails UserSig validation when UserSig has unexpected Scheme" $ do
    let hsh = hash "(somePactFunction)"
    [signer] <- toSigners [someED25519Pair]
    [((pubKey, privKey),_)]     <- mkKeyPairs $ toApiKeyPairs [someED25519Pair]
    sig      <- sign pubKey privKey (toUntypedHash hsh)
    let myUserSig   = UserSig (toB16Text sig)
        wrongScheme = WebAuthn
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
    wrongSigners <- toSigners [anotherED25519Pair]
    kps          <- mkKeyPairs $ toApiKeyPairs [someED25519Pair]

    cmdWithWrongSig <- mkCommandTest kps wrongSigners "(somePactFunction)"
    shouldBeProcFail (verifyCommand cmdWithWrongSig)



  it "fails when number of signatures does not match number of payload signers" $ do
    [signer]  <- toSigners [anotherED25519Pair]
    [kp]      <- mkKeyPairs $ toApiKeyPairs [anotherED25519Pair]
    [wrongKp] <- mkKeyPairs $ toApiKeyPairs [someED25519Pair]

    cmdWithWrongNumSig <- mkCommandTest [kp, wrongKp] [signer] "(somePactFunction)"
    shouldBeProcFail (verifyCommand cmdWithWrongNumSig)

testSigsRoundtrip :: Spec
testSigsRoundtrip = it "SigsRoundtrip succeeds" $ do
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

-- This test uses example public keys, clientData and authenticatorData from a
-- real WebAuthn browser session from a test user.
verifyWebAuthnSignature :: Spec
verifyWebAuthnSignature = describe "WebAuthn signature" $ do
  it "should verify a webauthn signature" $ do
    let
      (webAuthnSig, PubBS pubKey) = someWebAuthnSignature
      pubKeyBase16 = T.decodeUtf8 $ Base16.encode pubKey
      cmdHash' = case fromText' $ T.pack "NAClnfjBbOj7GfnE86c2NeVGi0YRDJrYbuAtrhES2bc" of
        Right h -> h
        Left _ -> error "Hash is valid"
      signer = Signer
        { _siScheme = Just WebAuthn
        , _siPubKey = pubKeyBase16
        , _siAddress = Nothing
        , _siCapList = []
        }
    verifyUserSig cmdHash' webAuthnSig signer `shouldBe` True
  it "should require a matching pubkey" $ do
    let
      (webAuthnSig, _) = someWebAuthnSignature
      (PubBS otherPubKey, _, _, _) = someED25519Pair
      pubKeyBase16 = T.decodeUtf8 $ Base16.encode otherPubKey
      cmdHash' :: TypedHash Blake2b_256 = case fromText' $ T.pack "NAClnfjBbOj7GfnE86c2NeVGi0YRDJrYbuAtrhES2bc" of
        Right h -> h
        Left _ -> error "Hash is valid"
      signer = Signer
        { _siScheme = Just WebAuthn
        , _siPubKey = pubKeyBase16
        , _siAddress = Nothing
        , _siCapList = []
        }
    verifyUserSig cmdHash' webAuthnSig signer `shouldBe` False
  it "should require a matching cmdHash" $ do
    let
      (webAuthnSig, PubBS pubKey) = someWebAuthnSignature
      pubKeyBase16 = T.decodeUtf8 $ Base16.encode pubKey
      cmdHash' :: TypedHash Blake2b_256 = case fromText' $ T.pack "3fbc092db9350757e2ab4f7ee9792bfcd2f5220ada5a4bc684487f60c6034369" of
        Right h -> h
        Left _ -> error "Hash is valid"
      signer = Signer
        { _siScheme = Just WebAuthn
        , _siPubKey = pubKeyBase16
        , _siAddress = Nothing
        , _siCapList = []
        }
    verifyUserSig cmdHash' webAuthnSig signer `shouldBe` False
  it "should require webauthn scheme" $ do
    let
      (webAuthnSig, PubBS pubKey) = someWebAuthnSignature
      pubKeyBase16 = T.decodeUtf8 $ Base16.encode pubKey
      cmdHash' :: TypedHash Blake2b_256 = case fromText' $ T.pack "NAClnfjBbOj7GfnE86c2NeVGi0YRDJrYbuAtrhES2bc" of
        Right h -> h
        Left _ -> error "Hash is valid"
      signer = Signer
        { _siScheme = Nothing
        , _siPubKey = pubKeyBase16
        , _siAddress = Nothing
        , _siCapList = []
        }
    verifyUserSig cmdHash' webAuthnSig signer `shouldBe` False

{-# LANGUAGE OverloadedStrings #-}

module SchemeSpec (spec) where

import Test.Hspec
import System.IO.Error
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Aeson as A
import qualified Data.ByteString.Base16   as B16
import qualified Crypto.Hash              as H

import Pact.ApiReq
import Pact.Types.Crypto
import Pact.Types.Command hiding (Address)
import Pact.Types.Util (toB16Text, fromJSON')


---- HELPER DATA TYPES AND FUNCTIONS ----

getByteString :: ByteString -> ByteString
getByteString = fst . B16.decode


type Address = Text

getKeyPairComponents :: SomeKeyPair -> (PublicKeyBS, PrivateKeyBS, Address, PPKScheme)
getKeyPairComponents kp = (PubBS $ getPublic kp,
                           PrivBS $ getPrivate kp,
                           toB16Text $ formatPublicKey kp,
                           kpToPPKScheme kp)



someED25519Pair :: (PublicKeyBS, PrivateKeyBS, Address, PPKScheme)
someED25519Pair = (PubBS $ getByteString "ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d",
                   PrivBS $ getByteString "8693e641ae2bbe9ea802c736f42027b03f86afe63cae315e7169c9c496c17332",
                   "ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d",
                   ED25519)

someETHPair :: (PublicKeyBS, PrivateKeyBS, Address, PPKScheme)
someETHPair = (PubBS $ getByteString "836b35a026743e823a90a0ee3b91bf615c6a757e2b60b9e1dc1826fd0dd16106f7bc1e8179f665015f43c6c81f39062fc2086ed849625c06e04697698b21855e",
               PrivBS $ getByteString "208065a247edbe5df4d86fbdc0171303f23a76961be9f6013850dd2bdc759bbb",
               "0bed7abd61247635c1973eb38474a2516ed1d884",
               ETH)




---- HSPEC TESTS ----

spec :: Spec
spec = describe "working with crypto schemes" $ do
  describe "test importing Key Pair for each Scheme" testKeyPairImport
  describe "test default scheme in ApiKeyPair" testDefSchemeApiKeyPair
  describe "test for correct address in ApiKeyPair" testAddrApiKeyPair
  describe "test PublicKey import" testPublicKeyImport
  describe "test UserSig creation and verificaton" testUserSig


testKeyPairImport :: Spec
testKeyPairImport = do
  it "imports ED25519 Key Pair" $ do
    let (pub, priv, addr, scheme) = someED25519Pair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme)
    kp <- mkKeyPairs [apiKP]
    (map getKeyPairComponents kp) `shouldBe` [someED25519Pair]
    
  it "imports ETH Key Pair" $ do
    let (pub, priv, addr, scheme) = someETHPair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme)
    kp <- mkKeyPairs [apiKP]
    (map getKeyPairComponents kp) `shouldBe` [someETHPair]


testDefSchemeApiKeyPair :: Spec
testDefSchemeApiKeyPair =
  context "when scheme not provided in API" $
    it "makes the scheme the default PPKScheme" $ do
      let (pub, priv, addr, _) = someED25519Pair
          apiKP = ApiKeyPair priv (Just pub) (Just addr) Nothing
      kp <- mkKeyPairs [apiKP]
      (map getKeyPairComponents kp) `shouldBe` [someED25519Pair]


testAddrApiKeyPair :: Spec
testAddrApiKeyPair =
  it "throws error when address provided in API doesn't match derived address" $ do
     let (pub, priv, _, scheme) = someETHPair
         apiKP = ApiKeyPair priv (Just pub) (Just "9f491e44a3f87df60d6cb0eefd5a9083ae6c3f32") (Just scheme)
     mkKeyPairs [apiKP] `shouldThrow` isUserError


testPublicKeyImport :: Spec
testPublicKeyImport = do
  it "derives PublicKey from the PrivateKey when PublicKey not provided" $ do
    let (_, priv, addr, scheme) = someETHPair
        apiKP = ApiKeyPair priv Nothing (Just addr) (Just scheme)
    kp <- mkKeyPairs [apiKP]
    (map getKeyPairComponents kp) `shouldBe` [someETHPair]

  it "throws error when PublicKey provided does not match derived PublicKey" $ do
    let (_, priv, addr, scheme) = someETHPair
        fakePub = PubBS $ getByteString "c640e94730fb7b7fce01b11086645741fcb5174d1c634888b9d146613730243a171833259cd7dab9b3435421dcb2816d3efa55033ff0899de6cc8b1e0b20e56c"
        apiKP = ApiKeyPair priv (Just fakePub) (Just addr) (Just scheme)
    mkKeyPairs [apiKP] `shouldThrow` isUserError


testUserSig :: Spec
testUserSig = do
  it "successfully verifies ETH UserSig when using Command's mkUserSig" $ do
    let (pub, priv, addr, scheme) = someETHPair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme)
    kp <- mkKeyPairs [apiKP]
    cmd <- mkCommand' kp "(somePactFunction)"
    (map (verifyUserSig $ _cmdHash cmd) (_cmdSigs cmd)) `shouldBe` [True]




  it "successfully verifies ETH UserSig when provided by user" $ do
    let (pub, priv, addr, scheme) = someETHPair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme)
        (PubBS pubBS) = pub
        -- UserSig verification will pass but Command verification might fail
        -- if hash algorithm provided not supported for hashing commands.
        hsh = hashTx "(somePactFunction)" H.SHA3_256
    [kp] <- mkKeyPairs [apiKP]
    sig <- sign kp hsh
    let myUserSig = UserSig scheme (toB16Text pubBS) addr (toB16Text sig)
    (verifyUserSig hsh myUserSig) `shouldBe` True




  it "fails UserSig validation when UserSig has unexpected Address" $ do
    let (pub, priv, addr, scheme) = someETHPair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme)
        (PubBS pubBS) = pub
        hsh = hashTx "(somePactFunction)" H.Blake2b_512
        wrongAddr = (toB16Text pubBS)
    [kp] <- mkKeyPairs [apiKP]
    sig <- sign kp hsh
    let myUserSig = UserSig scheme (toB16Text pubBS) wrongAddr (toB16Text sig)
    (verifyUserSig hsh myUserSig) `shouldBe` False




  it "fails UserSig validation when UserSig has unexpected Scheme" $ do
    let (pub, priv, addr, scheme) = someETHPair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme)
        (PubBS pubBS) = pub
        hsh = hashTx "(somePactFunction)" H.Blake2b_512
        wrongScheme = ED25519
    [kp] <- mkKeyPairs [apiKP]
    sig <- sign kp hsh
    let myUserSig = UserSig wrongScheme (toB16Text pubBS) addr (toB16Text sig)
    (verifyUserSig hsh myUserSig) `shouldBe` False




  it "provides default ppkscheme when one not provided" $ do
    let sigJSON = A.object ["addr" .= String "SomeAddr", "pubKey" .= String "SomePubKey",
                            "sig" .= String "SomeSig"]
        sig = UserSig defPPKScheme "SomePubKey" "SomeAddr" "SomeSig"
    (fromJSON' sigJSON) `shouldBe` (Right sig)




  it "makes address field the full public key when one not provided" $ do
    let sigJSON = A.object ["pubKey" .= String "SomePubKey", "sig" .= String "SomeSig"]
        sig = UserSig defPPKScheme "SomePubKey" "SomePubKey" "SomeSig"
    (fromJSON' sigJSON) `shouldBe` (Right sig)

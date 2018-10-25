{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Types.ECC where

import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (Point(..),getCurveByName,CurveName(..))
import Crypto.PubKey.ECC.ECDSA (PublicKey(..),PrivateKey(..),Signature(..),signWith,sign,verify)
import Crypto.Hash (hash,Digest,Keccak_256(..),SHA3_256(..))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString (ByteString)
import Pact.Types.Util (toB16Text)
import Data.Text (Text)
import Data.Monoid ((<>))


gen :: IO (PublicKey,PrivateKey)
gen = generate (getCurveByName SEC_p256k1)

formatEthPublicKey :: PublicKey -> ByteString
formatEthPublicKey pub = BS.drop 12 $ keccak256Hash xy
  where (x,y) = case public_q pub of (Point a b) -> (a,b)
                                     PointO -> (0,0) -- ???
        xy = integralToHexBS x <> integralToHexBS y

integralToHexBS :: Integral a => a -> ByteString
integralToHexBS a = BS.pack $ reverse $ go (quotRem a base) where
  go (n,d) | n == 0 = [fromIntegral d]
           | otherwise = (fromIntegral d):go (quotRem n base)
  base = 256

iToHexText :: Integral a => a -> Text
iToHexText = toB16Text . integralToHexBS

keccak256Hash :: ByteString -> ByteString
keccak256Hash = BS.pack . BA.unpack . (hash :: BA.Bytes -> Digest Keccak_256) . BA.pack . BS.unpack

-- Decodes ByteString from base 16 to ByteString of base 256
hexBS16ToBS :: ByteString -> ByteString
hexBS16ToBS = fst . B16.decode

hexBStoEthAddress :: ByteString -> ByteString
hexBStoEthAddress = BS.drop 12 . keccak256Hash . hexBS16ToBS

-- TODO
-- Generates an ECDSA nonce (`k`) deterministically according to RFC 6979.
-- https://tools.ietf.org/html/rfc6979#section-3.2
-- Based in this example:
--   https://github.com/btcsuite/btcd/blob/master/btcec/signature.go#L455
-- If k-signature returns zero should re-determine k
deterministicNonce :: PrivateKey -> ByteString -> Int
deterministicNonce priv msg = undefined

-- TODO why SHA3 and not keccak
signSHA3_256' :: Integer -> PrivateKey -> ByteString -> Maybe Signature
signSHA3_256' i priv = signWith i priv SHA3_256

verifySHA3_256 :: PublicKey -> Signature -> ByteString -> Bool
verifySHA3_256 pub = verify SHA3_256 pub


-- example from https://kobl.one/blog/create-full-ethereum-keypair-and-address/
-- "Which gives us the Ethereum address 0x0bed7abd61247635c1973eb38474a2516ed1d884"
_testKeccak1 :: Text
_testKeccak1 = toB16Text $ hexBStoEthAddress
  "836b35a026743e823a90a0ee3b91bf615c6a757e2b60b9e1dc1826fd0dd16106f7bc1e8179f665015f43c6c81f39062fc2086ed849625c06e04697698b21855e"

_testKeccak2 :: Text
_testKeccak2 = toB16Text $ hexBStoEthAddress
  --"4643bb6b393ac20a6175c713175734a72517c63d6f73a3ca90a15356f2e967da03d16431441c61ac69aeabb7937d333829d9da50431ff6af38536aa262497b27"
  "af80b90d25145da28c583359beb47b21796b2fe1a23c1511e443e7a64dfdb27d7434c380f0aa4c500e220aa1a9d068514b1ff4d5019e624e7ba1efe82b340a59"

_testKeccak :: ByteString -> Text
_testKeccak bs = toB16Text $ hexBStoEthAddress bs

_test :: ByteString
_test =  hexBS16ToBS "0bed7abd61247635c1973eb38474a2516ed1d884"

_testSign :: PrivateKey -> Maybe Signature
_testSign priv = signSHA3_256' 5 priv msg
  where msg = "hello"

_testVerify :: PublicKey -> Maybe Signature -> Bool
_testVerify _ Nothing = False
_testVerify pub (Just sig) = verifySHA3_256 pub sig "hello"

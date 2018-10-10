{-# LANGUAGE PackageImports #-}

module Pact.Types.ECC where

import Crypto.PubKey.ECC.Generate
import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.ECDSA
import Numeric (showHex, showIntAtBase)
import Crypto.Hash
import qualified Data.ByteArray as BA
import Data.Word (Word8)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Pact.Types.Util
import Data.Text (Text)
import Data.Monoid ((<>))


gen :: IO (PublicKey,PrivateKey)
gen = generate (getCurveByName SEC_p256k1)

formatEthPublicKey :: PublicKey -> ByteString
formatEthPublicKey pub = keccak256Hash xy
  where (Point x y) = public_q pub
        xy = toHexBS x <> toHexBS y

toHexBS :: Integral a => a -> ByteString
toHexBS a = BS.pack $ reverse $ go (quotRem a base) where
  go (n,d) | n == 0 = [fromIntegral d]
           | otherwise = (fromIntegral d):go (quotRem n base)
  base = 256

keccak256Hash :: ByteString -> ByteString
keccak256Hash = BS.pack . BA.unpack . (hash :: BA.Bytes -> Digest Keccak_256) . BA.pack . BS.unpack

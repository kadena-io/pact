module Blake2Spec (spec) where

import Data.Foldable (foldlM)
import Crypto.Hash.Blake2Native
import Data.Bits
import Data.ByteString (ByteString,pack)
import Data.Word
import Test.Hspec

spec :: Spec
spec = do
  describe "blake2b_selftest" blake2b_selftest
  describe "blake2s_selftest" blake2s_selftest

-- test from RFC7693

-- | grand hash of hash results
blake2b_res :: ByteString
blake2b_res = pack [
        0xC2, 0x3A, 0x78, 0x00, 0xD9, 0x81, 0x23, 0xBD,
        0x10, 0xF5, 0x06, 0xC6, 0x1E, 0x29, 0xDA, 0x56,
        0x03, 0xD7, 0x63, 0xB8, 0xBB, 0xAD, 0x2E, 0x73,
        0x7F, 0x5E, 0x76, 0x5A, 0x7B, 0xCC, 0xD4, 0x75
    ]

-- parameter sets
b2b_md_len :: [Int]; b2b_md_len = [ 20, 32, 48, 64 ];
b2b_in_len :: [Int]; b2b_in_len = [ 0, 3, 128, 129, 255, 1024 ];


selftest_seq :: Int -> Word32 -> ByteString
selftest_seq len seed =
  let a' = 0xDEAD4BAD * seed
      b' = 1
  in pack $ reverse $ fst $ ffoldl ([],(a',b')) [0..(pred len)] $ \(bs,(a,b)) _ ->
    let t = a + b
        a'' = b
        b'' = t
    in (fromIntegral ((t `shiftR` 24) .&. 0xFF):bs,(a'',b''))




blake2b_selftest :: Spec
blake2b_selftest = either fail p hashing
  where hashing = blake2b_init 32 mempty >>= \cxinit -> foldlM f cxinit b2b_md_len
        p cxr   = it "final hash correct" $ blake2b_final cxr `shouldBe` blake2b_res
        f cx outlen = foldlM g cx b2b_in_len
          where g cx0 inlen = do
                  let inB = selftest_seq inlen (fromIntegral inlen)
                  md0 <- blake2b outlen mempty inB
                  let cx1 = blake2b_update md0 cx0
                      key = selftest_seq outlen (fromIntegral outlen)
                  md1 <- blake2b outlen key inB
                  Right $ blake2b_update md1 cx1

-- | Grand hash of hash results.
blake2s_res :: ByteString
blake2s_res = pack [
        0x6A, 0x41, 0x1F, 0x08, 0xCE, 0x25, 0xAD, 0xCD,
        0xFB, 0x02, 0xAB, 0xA6, 0x41, 0x45, 0x1C, 0xEC,
        0x53, 0xC5, 0x98, 0xB2, 0x4F, 0x4F, 0xC7, 0x87,
        0xFB, 0xDC, 0x88, 0x79, 0x7F, 0x4C, 0x1D, 0xFE
    ];

-- Parameter sets.
b2s_md_len :: [Int]; b2s_md_len = [ 16, 20, 28, 32 ];
b2s_in_len :: [Int]; b2s_in_len = [ 0,  3,  64, 65, 255, 1024 ];

blake2s_selftest :: Spec
blake2s_selftest = either fail p hashing
  where hashing = blake2s_init 32 mempty >>= \cxinit -> foldlM f cxinit b2s_md_len
        p cxr   = it "final hash correct" $ blake2s_final cxr `shouldBe` blake2s_res
        f cx outlen = foldlM g cx b2s_in_len
          where g cx0 inlen = do
                  let inB = selftest_seq inlen (fromIntegral inlen)
                  md0 <- blake2s outlen mempty inB
                  let cx1 = blake2s_update md0 cx0
                      key = selftest_seq outlen (fromIntegral outlen)
                  md1 <- blake2s outlen key inB
                  Right $ blake2s_update md1 cx1

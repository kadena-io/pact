module Blake2Spec (spec) where

import Test.Hspec
import Crypto.Hash.Blake2Native
import Data.Word
import Data.ByteString (ByteString,pack)
import Data.Bits
import Data.Either

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


selftestSeq :: Int -> Word32 -> ByteString
selftestSeq len seed =
  let a' = 0xDEAD4BAD * seed
      b' = 1
  in pack $ reverse $ fst $ ffoldl ([],(a',b')) [0..(pred len)] $ \(bs,(a,b)) _ ->
    let t = a + b
        a'' = b
        b'' = t
    in (fromIntegral ((t `shiftR` 24) .&. 0xFF):bs,(a'',b''))




blake2b_selftest :: Spec
blake2b_selftest = do
  let ctxm = blake2b_init 32 mempty
  it "init succeeds" $ isRight ctxm
  let (Right cxinit) = ctxm

      cxr = ffoldl cxinit b2b_md_len $
        \c outlen -> ffoldl c b2b_in_len $ \cx0 inlen ->
          let inB = selftestSeq inlen (fromIntegral inlen)
              (Right md0) = blake2b outlen mempty inB
              cx1 = blake2b_update md0 cx0
              key = selftestSeq outlen (fromIntegral outlen)
              (Right md1) = blake2b outlen key inB
          in blake2b_update md1 cx1

      res = blake2b_final cxr

  it "final hash correct" $ res `shouldBe` blake2b_res


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
blake2s_selftest = do
  let ctxm = blake2s_init 32 mempty
  it "init succeeds" $ isRight ctxm
  let (Right cxinit) = ctxm

      cxr = ffoldl cxinit b2s_md_len $
        \c outlen -> ffoldl c b2s_in_len $ \cx0 inlen ->
          let inB = selftestSeq inlen (fromIntegral inlen)
              (Right md0) = blake2s outlen mempty inB
              cx1 = blake2s_update md0 cx0
              key = selftestSeq outlen (fromIntegral outlen)
              (Right md1) = blake2s outlen key inB
          in blake2s_update md1 cx1

      res = blake2s_final cxr

  it "final hash correct" $ res `shouldBe` blake2s_res

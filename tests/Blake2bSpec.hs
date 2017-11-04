module Blake2bSpec where

import Test.Hspec
import Pact.Types.Blake2b
import Data.Word
import Data.ByteString (ByteString,pack)
import Data.Bits
import Data.Either

spec :: Spec
spec = describe "blake2b_selftest" blake2b_selftest

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

ffoldl i l f = foldl f i l

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
blake2b_selftest = do
  let ctxm = blake2b_init 32 mempty
  it "init succeeds" $ isRight ctxm
  let (Right cxinit) = ctxm

      cxr = ffoldl cxinit b2b_md_len $
        \c outlen -> ffoldl c b2b_in_len $ \cx0 inlen ->
          let inB = selftest_seq inlen (fromIntegral inlen)
              (Right md0) = blake2b outlen mempty inB
              cx1 = blake2b_update md0 cx0
              key = selftest_seq outlen (fromIntegral outlen)
              (Right md1) = blake2b outlen key inB
          in blake2b_update md1 cx1

      res = blake2b_final cxr

  it "final hash correct" $ res `shouldBe` blake2b_res

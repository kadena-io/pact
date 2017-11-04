{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Types.PureBlake2
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Pure implementation of BLAKE2b for use in GHCJS.
-- Adapted from C code in https://github.com/mjosaarinen/blake2_mjosref
--

module Pact.Types.Blake2b where

import Data.Word
import Data.Bits
import Data.ByteString as B (ByteString,index,unpack,pack,length,take,splitAt,null,replicate)
import Data.Vector as V (Vector,(//),(!),fromList,(++))
import Control.Lens ((&),(<&>))
import Prelude as P hiding (last)
import Data.Monoid


type V = Vector

-- | state context
data Blake2BCtx = Blake2BCtx
  { _b :: ByteString -- 128 input buffer
  , _h :: V Word64        --   8 chained state
  , _t0 :: Word64         --   total number of bytes 0
  , _t1 :: Word64         --                         1
  , _c :: Int        -- pointer for b[]
  , _outlen :: Int   -- digest size
  } deriving (Eq)
instance Show Blake2BCtx where
  show (Blake2BCtx b h t0 t1 c outlen) =
    "Blake2BCtx {_b=pack" P.++ show (unpack b) P.++ ", _h=mk" P.++ show h P.++
    ", _t0=" P.++ show t0 P.++ ", _t1=" P.++ show t1 P.++ ", _c=" P.++ show c P.++
    ", _outlen=" P.++ show outlen P.++ "}"


at :: V a -> Int -> a
at v i = v ! i


upd :: Int -> (V a -> a -> a) -> V a -> V a
upd i f v = v // [(i,f v (v ! i))]


mk :: [a] -> V a
mk = fromList

cat :: V Word64 -> V Word64 -> V Word64
cat = (V.++)

-- | flipped foldl
ffoldl :: Foldable t => b -> t a -> (b -> a -> b) -> b
ffoldl i l f = foldl f i l


-- | Cyclic right rotation.
rotr64 :: Word64 -> Int -> Word64
rotr64 x y = (x `shiftR` y) `xor` (x `shiftL` (64 - y))

-- | Little-endian byte access.

b2b_get64 :: Int -> ByteString -> Word64
b2b_get64 o p =
  let ix64 i = fromIntegral $ index p (o+i)
  in
        ix64 0               `xor`
       (ix64 1  `shiftL` 8 ) `xor`
       (ix64 2  `shiftL` 16) `xor`
       (ix64 3  `shiftL` 24) `xor`
       (ix64 4  `shiftL` 32) `xor`
       (ix64 5  `shiftL` 40) `xor`
       (ix64 6  `shiftL` 48) `xor`
       (ix64 7  `shiftL` 56)


-- | G Mixing function.
b2b_g :: Int -> Int -> Int -> Int -> Word64 -> Word64 -> V Word64 -> V Word64
b2b_g a b c d x' y' v' =
  v' &
  upd a (\v x -> x + (v `at` b) + x') &
  upd d (\v x -> rotr64 (x `xor` (v `at` a)) 32) &
  upd c (\v x -> x + (v `at` d)) &
  upd b (\v x -> rotr64 (x `xor` (v `at` c)) 24) &
  upd a (\v x -> x + (v `at` b) + y') &
  upd d (\v x -> rotr64 (x `xor` (v `at` a)) 16) &
  upd c (\v x -> x + (v `at` d)) &
  upd b (\v x -> rotr64 (x `xor` (v `at` c)) 63)

-- | Initialization Vector.
blake2b_iv :: V Word64
blake2b_iv = mk [
    0x6A09E667F3BCC908, 0xBB67AE8584CAA73B,
    0x3C6EF372FE94F82B, 0xA54FF53A5F1D36F1,
    0x510E527FADE682D1, 0x9B05688C2B3E6C1F,
    0x1F83D9ABFB41BD6B, 0x5BE0CD19137E2179
    ]

sigma :: V (V Int)
sigma = mk [
        mk [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ],
        mk [ 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 ],
        mk [ 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4 ],
        mk [ 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8 ],
        mk [ 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13 ],
        mk [ 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9 ],
        mk [ 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11 ],
        mk [ 13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10 ],
        mk [ 6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5 ],
        mk [ 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0 ],
        mk [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ],
        mk [ 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 ]
        ]


-- | Compression function. "last" flag indicates last block.
blake2b_compress :: Blake2BCtx -> Bool -> Blake2BCtx
blake2b_compress ctx last =
  let
    v1 = (_h ctx `cat` blake2b_iv) &
      upd 12 (\_ x -> (x `xor` _t0 ctx)) &                    -- low 64 bits of offset
      upd 13 (\_ x -> (x `xor` _t1 ctx)) &                    -- high 64 bits
      (if last then upd 14 (\_ x -> complement x) else id)     -- last block flag set ?
    m = mk $ (`map` [0..15]) $ \i -> b2b_get64 (8 * i) (_b ctx)         -- get little-endian words
    v2 = ffoldl v1 sigma $ runRound m                        -- 12 rounds
    h = mk $ (`map` [0..7]) $ \i ->
          (_h ctx `at` i) `xor` (v2 `at` i) `xor` (v2 `at` (i + 8))
  in ctx { _h = h }

runRound :: V Word64 -> V Word64 -> V Int -> V Word64
runRound m v s = v &
  b2b_g 0 4  8 12 (m `at` (s `at`  0)) (m `at` (s `at`  1)) &
  b2b_g 1 5  9 13 (m `at` (s `at`  2)) (m `at` (s `at`  3)) &
  b2b_g 2 6 10 14 (m `at` (s `at`  4)) (m `at` (s `at`  5)) &
  b2b_g 3 7 11 15 (m `at` (s `at`  6)) (m `at` (s `at`  7)) &
  b2b_g 0 5 10 15 (m `at` (s `at`  8)) (m `at` (s `at`  9)) &
  b2b_g 1 6 11 12 (m `at` (s `at` 10)) (m `at` (s `at` 11)) &
  b2b_g 2 7  8 13 (m `at` (s `at` 12)) (m `at` (s `at` 13)) &
  b2b_g 3 4  9 14 (m `at` (s `at` 14)) (m `at` (s `at` 15))

-- | Initialize the hashing context "ctx" with optional key "key".
--      1 <= outlen <= 64 gives the digest size in bytes.
--      Secret key (also <= 64 bytes) is optional (keylen = 0).
blake2b_init :: Word64 -> ByteString -> Either String Blake2BCtx
blake2b_init outlen key
  | outlen < 1 || outlen > 64 = Left $ "outlen must be > 0 and <= 64 bytes" P.++ show outlen
  | B.length key > 64 = Left "Key must be <= 64 bytes"
  | otherwise = Right $
    let keylen :: Word64
        keylen = fromIntegral $ B.length key
        iv = blake2b_iv
    in Blake2BCtx
          {
            -- state, "param block"
            _h = upd 0 (\_ x -> x `xor` 0x01010000 `xor` (keylen `shiftL` 8) `xor` outlen) iv
          , _t0 = 0 -- input count low word
          , _t1 = 0 -- input count high word
          , _c = 0  -- pointer within buffer
          , _outlen = fromIntegral outlen
          , _b = B.replicate 128 0 -- zero input block.
          } &
          (\ctx -> if keylen > 0 then blake2b_update key ctx & (\c -> c { _c = 128}) else ctx)

-- | Slice a bytestring into segments: 'slice c l bs' means the first
-- segment will be length c but no longer than l, and the following will
-- be length l, with the last being whatever remains.
slice :: Int -> Int -> ByteString -> [ByteString]
slice c l bs = go (B.splitAt (if c >= l then l else l - c) bs) where
  go (x,y) | B.null y = [x]
           | otherwise = x:go (B.splitAt l y)

-- | Add "inlen" bytes from "in" into the hash.
blake2b_update :: ByteString -> Blake2BCtx -> Blake2BCtx
blake2b_update bs ctx
  | B.null bs = ctx
  | otherwise = ffoldl ctx (slice (_c ctx) 128 bs) $ \cx s ->
      resetMaybe cx & cpyBuf s
  where resetMaybe cx
          | _c cx < 128 = cx
          | otherwise =
              let t0 = _t0 cx + 128
                  t1 = _t1 cx + (if t0 < 128 then 1 else 0)
              in (blake2b_compress (cx { _t0 = t0, _t1 = t1 }) False) { _c = 0 }
        cpyBuf s cx = cx {
          _b = B.take (_c cx) (_b cx) <> s <> B.replicate (128 - _c cx - B.length s) 0,
          _c = B.length s + _c cx
          }

-- | Generate the message digest (size given in init).
blake2b_final :: Blake2BCtx -> ByteString
blake2b_final ctx = finalCompress ctx & conv where
  finalCompress cx =
    let c = fromIntegral $ _c cx
        t0 = _t0 cx + c
        t1 = _t1 cx + (if t0 < c then 1 else 0)
        b = B.take (_c cx) (_b cx) <> B.replicate (128 - _c cx) 0
    in blake2b_compress (cx { _t0 = t0, _t1 = t1, _b = b }) True
  conv cx = B.pack $ (`map` [0 .. pred (_outlen cx)]) $ \i ->
    fromIntegral $ ((_h cx `at` (i `shiftR` 3)) `shiftR` (8 * (i .&. 7))) .&. 0xFF

-- | All together now! 'blake2b outlen key inB' hashes inB, with
-- optional key (empty means no key), to length outlen.
blake2b :: Int -> ByteString -> ByteString -> Either String ByteString
blake2b outlen key inB =
  blake2b_init (fromIntegral outlen) key <&>
  (blake2b_final . blake2b_update inB)

-- Test value
_ctx32 :: Blake2BCtx
_ctx32 = let Right c = blake2b_init 32 mempty in c

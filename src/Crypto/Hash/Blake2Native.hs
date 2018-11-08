{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Crypto.Hash.Blake2Native
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Native haskell implementation of BLAKE2b.
-- Adapted from C code in https://github.com/mjosaarinen/blake2_mjosref
--

module Crypto.Hash.Blake2Native where

import Data.Word
import Data.Bits
import Data.ByteString as B (ByteString,index,unpack,pack,length,take,splitAt,null,replicate)
import Data.Vector as V (Vector,(//),(!),fromList,(++))
import Control.Lens ((&),(<&>))
import Prelude as P hiding (last)


type V = Vector

-- | state context
data Blake2Ctx w = Blake2Ctx
  { _b :: ByteString -- 128 input buffer
  , _h :: V w        --   8 chained state
  , _t0 :: w         --   total number of bytes 0
  , _t1 :: w         --                         1
  , _c :: Int        -- pointer for b[]
  , _outlen :: Int   -- digest size
  } deriving (Eq)
instance (Show w) => Show (Blake2Ctx w) where
  show (Blake2Ctx b h t0 t1 c outlen) =
    "Blake2Ctx {_b=pack" P.++ show (unpack b) P.++ ", _h=mk" P.++ show h P.++
    ", _t0=" P.++ show t0 P.++ ", _t1=" P.++ show t1 P.++ ", _c=" P.++ show c P.++
    ", _outlen=" P.++ show outlen P.++ "}"


at :: V a -> Int -> a
at v i = v ! i


upd :: Int -> (V a -> a -> a) -> V a -> V a
upd i f v = v // [(i,f v (v ! i))]


mk :: [a] -> V a
mk = fromList

cat :: V a -> V a -> V a
cat = (V.++)

-- | flipped foldl
ffoldl :: Foldable t => b -> t a -> (b -> a -> b) -> b
ffoldl i l f = foldl f i l


-- | Cyclic right rotation, 64 bit
rotr64 :: Word64 -> Int -> Word64
rotr64 x y = (x `shiftR` y) `xor` (x `shiftL` (64 - y))

-- | Cyclic right rotation, 32 bit
rotr32 :: Word32 -> Int -> Word32
rotr32 x y = (x `shiftR` y) `xor` (x `shiftL` (32 - y))

-- | Little-endian byte access.

b2b_get64 :: Int -> ByteString -> Word64
b2b_get64 o p =
  let ix i = fromIntegral $ index p (o+i)
  in ix 0               `xor`
     (ix 1  `shiftL` 8 ) `xor`
     (ix 2  `shiftL` 16) `xor`
     (ix 3  `shiftL` 24) `xor`
     (ix 4  `shiftL` 32) `xor`
     (ix 5  `shiftL` 40) `xor`
     (ix 6  `shiftL` 48) `xor`
     (ix 7  `shiftL` 56)

-- | Little-endian byte access.

b2s_get32 :: Int -> ByteString -> Word32
b2s_get32 o p =
  let ix i = fromIntegral $ index p (o+i)
  in ix 0               `xor`
     (ix 1  `shiftL` 8 ) `xor`
     (ix 2  `shiftL` 16) `xor`
     (ix 3  `shiftL` 24)

type G w = Int -> Int -> Int -> Int -> w -> w -> V w -> V w

-- | G Mixing function, 64bit
b2b_g :: G Word64
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

-- | G Mixing function, 32bit
b2s_g :: G Word32
b2s_g a b c d x' y' v' =
  v' &
  upd a (\v x -> x + (v `at` b) + x') &
  upd d (\v x -> rotr32 (x `xor` (v `at` a)) 16) &
  upd c (\v x -> x + (v `at` d)) &
  upd b (\v x -> rotr32 (x `xor` (v `at` c)) 12) &
  upd a (\v x -> x + (v `at` b) + y') &
  upd d (\v x -> rotr32 (x `xor` (v `at` a)) 8) &
  upd c (\v x -> x + (v `at` d)) &
  upd b (\v x -> rotr32 (x `xor` (v `at` c)) 7)

-- | Initialization Vector, 64bit
blake2b_iv :: V Word64
blake2b_iv = mk [
  0x6A09E667F3BCC908, 0xBB67AE8584CAA73B,
  0x3C6EF372FE94F82B, 0xA54FF53A5F1D36F1,
  0x510E527FADE682D1, 0x9B05688C2B3E6C1F,
  0x1F83D9ABFB41BD6B, 0x5BE0CD19137E2179
  ]

-- | Initialization Vector, 32bit
blake2s_iv :: V Word32
blake2s_iv = mk [
  0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
  0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19
  ]

sigma64 :: V (V Int)
sigma64 = mk [
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

sigma32 :: V (V Int)
sigma32 = mk [
  mk [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ],
  mk [ 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 ],
  mk [ 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4 ],
  mk [ 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8 ],
  mk [ 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13 ],
  mk [ 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9 ],
  mk [ 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11 ],
  mk [ 13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10 ],
  mk [ 6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5 ],
  mk [ 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0 ]
  ]


-- | Compression function, 64bit. "last" flag indicates last block.
blake2b_compress :: Blake2Ctx Word64 -> Bool -> Blake2Ctx Word64
blake2b_compress ctx last =
  let
    v1 = (_h ctx `cat` blake2b_iv) &
      upd 12 (\_ x -> (x `xor` _t0 ctx)) &                    -- low 64 bits of offset
      upd 13 (\_ x -> (x `xor` _t1 ctx)) &                    -- high 64 bits
      (if last then upd 14 (\_ x -> complement x) else id)     -- last block flag set ?
    m = mk $ (`map` [0..15]) $ \i -> b2b_get64 (8 * i) (_b ctx)         -- get little-endian words
    v2 = ffoldl v1 sigma64 $ runRound b2b_g m                        -- 12 rounds
    h = mk $ (`map` [0..7]) $ \i ->
          (_h ctx `at` i) `xor` (v2 `at` i) `xor` (v2 `at` (i + 8))
  in ctx { _h = h }

-- | Compression function, 32bit. "last" flag indicates last block.
blake2s_compress :: Blake2Ctx Word32 -> Bool -> Blake2Ctx Word32
blake2s_compress ctx last =
  let
    v1 = (_h ctx `cat` blake2s_iv) &
      upd 12 (\_ x -> (x `xor` _t0 ctx)) &                    -- low 64 bits of offset
      upd 13 (\_ x -> (x `xor` _t1 ctx)) &                    -- high 64 bits
      (if last then upd 14 (\_ x -> complement x) else id)     -- last block flag set ?
    m = mk $ (`map` [0..15]) $ \i -> b2s_get32 (4 * i) (_b ctx)         -- get little-endian words
    v2 = ffoldl v1 sigma32 $ runRound b2s_g m                        -- 12 rounds
    h = mk $ (`map` [0..7]) $ \i ->
          (_h ctx `at` i) `xor` (v2 `at` i) `xor` (v2 `at` (i + 8))
  in ctx { _h = h }

runRound :: G w -> V w -> V w -> V Int -> V w
runRound g m v s = v &
  g 0 4  8 12 (m `at` (s `at`  0)) (m `at` (s `at`  1)) &
  g 1 5  9 13 (m `at` (s `at`  2)) (m `at` (s `at`  3)) &
  g 2 6 10 14 (m `at` (s `at`  4)) (m `at` (s `at`  5)) &
  g 3 7 11 15 (m `at` (s `at`  6)) (m `at` (s `at`  7)) &
  g 0 5 10 15 (m `at` (s `at`  8)) (m `at` (s `at`  9)) &
  g 1 6 11 12 (m `at` (s `at` 10)) (m `at` (s `at` 11)) &
  g 2 7  8 13 (m `at` (s `at` 12)) (m `at` (s `at` 13)) &
  g 3 4  9 14 (m `at` (s `at` 14)) (m `at` (s `at` 15))

-- | Initialize the hashing context "ctx" with optional key "key".
--      1 <= outlen <= 64 gives the digest size in bytes.
--      Secret key (also <= 64 bytes) is optional (keylen = 0).
blake2b_init :: Word64 -> ByteString -> Either String (Blake2Ctx Word64)
blake2b_init outlen key
  | outlen < 1 || outlen > 64 = Left $ "outlen must be > 0 and <= 64 bytes" P.++ show outlen
  | B.length key > 64 = Left "Key must be <= 64 bytes"
  | otherwise = Right $
    let keylen :: Word64
        keylen = fromIntegral $ B.length key
        iv = blake2b_iv
    in Blake2Ctx
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


-- | Initialize the hashing context "ctx" with optional key "key".
--      1 <= outlen <= 32 gives the digest size in bytes.
--      Secret key (also <= 32 bytes) is optional (keylen = 0).
blake2s_init :: Word32 -> ByteString -> Either String (Blake2Ctx Word32)
blake2s_init outlen key
  | outlen < 1 || outlen > 32 = Left $ "outlen must be > 0 and <= 32 bytes" P.++ show outlen
  | B.length key > 32 = Left "Key must be <= 32 bytes"
  | otherwise = Right $
    let keylen :: Word32
        keylen = fromIntegral $ B.length key
        iv = blake2s_iv
    in Blake2Ctx
          {
            -- state, "param block"
            _h = upd 0 (\_ x -> x `xor` 0x01010000 `xor` (keylen `shiftL` 8) `xor` outlen) iv
          , _t0 = 0 -- input count low word
          , _t1 = 0 -- input count high word
          , _c = 0  -- pointer within buffer
          , _outlen = fromIntegral outlen
          , _b = B.replicate 64 0 -- zero input block.
          } &
          (\ctx -> if keylen > 0 then blake2s_update key ctx & (\c -> c { _c = 64}) else ctx)

-- | Slice a bytestring into segments: 'slice c l bs' means the first
-- segment will be length c but no longer than l, and the following will
-- be length l, with the last being whatever remains.
slice :: Int -> Int -> ByteString -> [ByteString]
slice c l bs = go (B.splitAt (if c >= l then l else l - c) bs) where
  go (x,y) | B.null y = [x]
           | otherwise = x:go (B.splitAt l y)

-- | Add "inlen" bytes from "in" into the hash, 64bit version.
blake2b_update :: ByteString -> Blake2Ctx Word64 -> Blake2Ctx Word64
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

-- | Add "inlen" bytes from "in" into the hash, 32bit version.
blake2s_update :: ByteString -> Blake2Ctx Word32 -> Blake2Ctx Word32
blake2s_update bs ctx
  | B.null bs = ctx
  | otherwise = ffoldl ctx (slice (_c ctx) 64 bs) $ \cx s ->
      resetMaybe cx & cpyBuf s
  where resetMaybe cx
          | _c cx < 64 = cx
          | otherwise =
              let t0 = _t0 cx + 64
                  t1 = _t1 cx + (if t0 < 64 then 1 else 0)
              in (blake2s_compress (cx { _t0 = t0, _t1 = t1 }) False) { _c = 0 }
        cpyBuf s cx = cx {
          _b = B.take (_c cx) (_b cx) <> s <> B.replicate (64 - _c cx - B.length s) 0,
          _c = B.length s + _c cx
          }

-- | Generate the message digest (size given in init), 64bit version.
blake2b_final :: Blake2Ctx Word64 -> ByteString
blake2b_final ctx = finalCompress ctx & conv where
  finalCompress cx =
    let c = fromIntegral $ _c cx
        t0 = _t0 cx + c
        t1 = _t1 cx + (if t0 < c then 1 else 0)
        b = B.take (_c cx) (_b cx) <> B.replicate (128 - _c cx) 0
    in blake2b_compress (cx { _t0 = t0, _t1 = t1, _b = b }) True
  conv cx = B.pack $ (`map` [0 .. pred (_outlen cx)]) $ \i ->
    fromIntegral $ ((_h cx `at` (i `shiftR` 3)) `shiftR` (8 * (i .&. 7))) .&. 0xFF


-- | Generate the message digest (size given in init), 32bit version.
blake2s_final :: Blake2Ctx Word32 -> ByteString
blake2s_final ctx = finalCompress ctx & conv where
  finalCompress cx =
    let c = fromIntegral $ _c cx
        t0 = _t0 cx + c
        t1 = _t1 cx + (if t0 < c then 1 else 0)
        b = B.take (_c cx) (_b cx) <> B.replicate (64 - _c cx) 0
    in blake2s_compress (cx { _t0 = t0, _t1 = t1, _b = b }) True
  conv cx = B.pack $ (`map` [0 .. pred (_outlen cx)]) $ \i ->
    fromIntegral $ ((_h cx `at` (i `shiftR` 2)) `shiftR` (8 * (i .&. 3))) .&. 0xFF

-- | All together now, 64bit! 'blake2b outlen key inB' hashes inB, with
-- optional key (empty means no key), to length outlen.
blake2b :: Int -> ByteString -> ByteString -> Either String ByteString
blake2b outlen key inB =
  blake2b_init (fromIntegral outlen) key <&>
  (blake2b_final . blake2b_update inB)

-- | All together now, 32bit! 'blake2s outlen key inB' hashes inB, with
-- optional key (empty means no key), to length outlen.
blake2s :: Int -> ByteString -> ByteString -> Either String ByteString
blake2s outlen key inB =
  blake2s_init (fromIntegral outlen) key <&>
  (blake2s_final . blake2s_update inB)

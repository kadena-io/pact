{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      :  Pact.Native.Trans.Types
-- Copyright   :  (C) 2022 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Operators and math built-ins.
--

module Pact.Native.Trans.Types
  ( TransResult(..)
  , mpfr_arity1
  , mpfr_arity2
  , c'mpz_init
  , c'mpz_clear
  , c'mpz_set_str
  , c'mpz_get_str
  , c'mpq_init
  , c'mpq_clear
  , c'mpq_set_str
  , c'mpq_get_str
  , c'mpfr_init
  , c'mpfr_set_default_prec
  , c'mpfr_clear
  , c'mpfr_set_str
  , c'mpfr_set_q
  , c'mpfr_get_q
  , c'mpfr_div
  , c'mpfr_pow
  , c'mpfr_log
  , c'mpfr_log2
  , c'mpfr_log10
  , c'mpfr_exp
  , c'mpfr_exp2
  , c'mpfr_exp10
  , c'mpfr_sqrt
  , c'mpfr_snprintf
  , withTemp
  ) where

import Control.Exception
import Data.Decimal (Decimal)
import Data.Int
import Data.Word
import Data.Ratio
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

data TransResult a
  = TransNumber !a
  | TransNaN
  | TransInf
  | TransNegInf
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data MPZ = MPZ {
  _mpzAlloc :: {-# UNPACK #-} !Int32,
  _mpzSize :: {-# UNPACK #-} !Int32,
  _mpzD :: {-# UNPACK #-} !(Ptr Limb)
}

instance Storable MPZ where
  sizeOf _ = (16)
  alignment _ = alignment (undefined :: Int32)
  peek = error "MPZ.peek: Not needed and not applicable"
  poke p (MPZ alloc size fp) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) p alloc
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) p size
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) p fp

data MPQ = MPQ {
  _mpqNum :: {-# UNPACK #-} !MPZ,
  _mpzDen :: {-# UNPACK #-} !MPZ
}

instance Storable MPQ where
  sizeOf _ = (32)
  alignment _ = alignment (undefined :: Int32)
  peek = error "MPQ.peek: Not needed and not applicable"
  poke p (MPQ n d) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) p n
    (\hsc_ptr -> pokeByteOff hsc_ptr 16) p d

type CPrecision = Int64
type Sign = Int32
type Exp = Int64
type Limb = Word64

data MPFR = MP {
  _precision :: {-# UNPACK #-} !CPrecision,
  _sign :: {-# UNPACK #-} !Sign,
  _exponent :: {-# UNPACK #-} !Exp,
  _limbs :: {-# UNPACK #-} !(Ptr Limb)
}

instance Storable MPFR where
  sizeOf _ = (32)
  alignment _ = alignment (undefined :: Int64)
  peek = error "MPFR.peek: Not needed and not applicable"
  poke p (MP prec s e fp) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) p prec
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) p s
    (\hsc_ptr -> pokeByteOff hsc_ptr 16) p e
    (\hsc_ptr -> pokeByteOff hsc_ptr 24) p fp

c'MPFR_RNDN :: CInt
c'MPFR_RNDN = 0    -- round to nearest, with ties to even
{-
c'MPFR_RNDZ :: CInt
c'MPFR_RNDZ = 1    -- round toward zero
c'MPFR_RNDU :: CInt
c'MPFR_RNDU = 2    -- round toward +Inf
c'MPFR_RNDD :: CInt
c'MPFR_RNDD = 3    -- round toward -Inf
c'MPFR_RNDA :: CInt
c'MPFR_RNDA = 4    -- round away from zero
c'MPFR_RNDF :: CInt
c'MPFR_RNDF = 5    -- faithful rounding
c'MPFR_RNDNA :: CInt
c'MPFR_RNDNA = -1  -- round to nearest, with ties away from zero (mpfr_round)
-}

rounding :: CInt
rounding = c'MPFR_RNDN

{-
readResultNumber :: String -> TransResult Decimal
readResultNumber (' ':s) = readResultNumber s
readResultNumber "nan" = TransNaN
readResultNumber "inf" = TransInf
readResultNumber "-inf" = TransNegInf
readResultNumber n = TransNumber (read (trimZeroes n))
-}

type Mpz_t = Ptr MPZ
type Mpq_t = Ptr MPQ
type Mpfr_t = Ptr MPFR

foreign import ccall "__gmpz_init"
  c'mpz_init :: Mpz_t -> IO ()

foreign import ccall "__gmpz_clear"
  c'mpz_clear :: Mpz_t -> IO ()

foreign import ccall "__gmpz_set_str"
  c'mpz_set_str :: Mpz_t -> Ptr CChar -> CInt -> IO ()

foreign import ccall "__gmpz_get_str"
  c'mpz_get_str :: Ptr CChar -> CInt -> Mpz_t -> IO (Ptr CChar)

foreign import ccall "__gmpq_init"
  c'mpq_init :: Mpq_t -> IO ()

foreign import ccall "__gmpq_clear"
  c'mpq_clear :: Mpq_t -> IO ()

foreign import ccall "__gmpq_set_str"
  c'mpq_set_str :: Mpq_t -> Ptr CChar -> CInt -> IO ()

foreign import ccall "__gmpq_get_str"
  c'mpq_get_str :: Ptr CChar -> CInt -> Mpq_t -> IO (Ptr CChar)

foreign import ccall "mpfr_init"
  c'mpfr_init :: Mpfr_t -> IO ()

foreign import ccall "mpfr_set_default_prec"
  c'mpfr_set_default_prec :: CInt -> IO ()

foreign import ccall "mpfr_clear"
  c'mpfr_clear :: Mpfr_t -> IO ()

foreign import ccall "mpfr_set_str"
  c'mpfr_set_str :: Mpfr_t -> Ptr CChar -> CInt -> CInt -> IO ()

foreign import ccall "mpfr_set_q"
  c'mpfr_set_q :: Mpfr_t -> Mpq_t -> CInt -> IO ()

foreign import ccall "mpfr_get_q"
  c'mpfr_get_q :: Mpq_t -> Mpfr_t -> IO ()

foreign import ccall "mpfr_div"
  c'mpfr_div :: Mpfr_t -> Mpfr_t -> Mpfr_t -> CInt -> IO ()

foreign import ccall "mpfr_pow"
  c'mpfr_pow :: Mpfr_t -> Mpfr_t -> Mpfr_t -> CInt -> IO ()

foreign import ccall "mpfr_log"
  c'mpfr_log :: Mpfr_t -> Mpfr_t -> CInt -> IO ()

foreign import ccall "mpfr_log2"
  c'mpfr_log2 :: Mpfr_t -> Mpfr_t -> CInt -> IO ()

foreign import ccall "mpfr_log10"
  c'mpfr_log10 :: Mpfr_t -> Mpfr_t -> CInt -> IO ()

foreign import ccall "mpfr_exp"
  c'mpfr_exp :: Mpfr_t -> Mpfr_t -> CInt -> IO ()

foreign import ccall "mpfr_exp2"
  c'mpfr_exp2 :: Mpfr_t -> Mpfr_t -> CInt -> IO ()

foreign import ccall "mpfr_exp10"
  c'mpfr_exp10 :: Mpfr_t -> Mpfr_t -> CInt -> IO ()

foreign import ccall "mpfr_sqrt"
  c'mpfr_sqrt :: Mpfr_t -> Mpfr_t -> CInt -> IO ()

foreign import ccall "mpfr_snprintf"
  c'mpfr_snprintf :: Ptr CChar -> CInt -> Ptr CChar -> CInt -> Mpfr_t -> IO ()

{-
withTempz :: (Mpz_t -> IO a) -> IO a
withTempz k = alloca $ \x ->
  bracket_ (c'mpz_init x) (c'mpz_clear x) (k x)
-}

withTempq :: (Mpq_t -> IO a) -> IO a
withTempq k = alloca $ \x ->
  bracket_ (c'mpq_init x) (c'mpq_clear x) (k x)

withTemp :: (Mpfr_t -> IO a) -> IO a
withTemp k = alloca $ \x ->
  bracket_ (c'mpfr_init x) (c'mpfr_clear x) (k x)

dec2Mpfr :: Decimal -> (Mpfr_t -> IO a) -> IO a
dec2Mpfr d k =
  withCString (show (numerator r) ++ "/" ++ show (denominator r)) $ \r' ->
  withTempq $ \q ->
  withTemp $ \x -> do
    c'mpq_set_str q r' 10
    c'mpfr_set_q x q rounding
    k x
  where
  r = toRational d

mpfr2Dec :: Mpfr_t -> IO (TransResult Decimal)
mpfr2Dec m =
  withTempq $ \q -> do
    c'mpfr_get_q q m
    out <- c'mpq_get_str nullPtr 10 q
    buf <- peekCString out
    free out
    let val = case break (== '/') buf of
                (before, []) -> read before % 1
                (before, _:after) -> read before % read after
    pure $ TransNumber $ fromRational val

mpfr_arity1
  :: (Mpfr_t -> Mpfr_t -> CInt -> IO ()) -> Decimal -> TransResult Decimal
mpfr_arity1 f x = unsafePerformIO $
  dec2Mpfr x $ \x' ->
  withTemp $ \y' -> do
    f y' x' rounding
    mpfr2Dec y'

mpfr_arity2
  :: (Mpfr_t -> Mpfr_t -> Mpfr_t -> CInt -> IO ())
  -> Decimal -> Decimal -> TransResult Decimal
mpfr_arity2 f x y = unsafePerformIO $
  dec2Mpfr x $ \x' ->
  dec2Mpfr y $ \y' ->
  withTemp $ \z' -> do
    f z' x' y' rounding
    mpfr2Dec z'

{-
trimZeroes :: String -> String
trimZeroes = reverse . go . reverse
  where
  go ('0':s) = go s
  go ('.':s) = "0." ++ s
  go s = s
-}

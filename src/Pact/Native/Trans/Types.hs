{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      :  Pact.Native.Trans.Types
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Operators and math built-ins.
--

module Pact.Native.Trans.Types where

import Data.Decimal (Decimal, normalizeDecimal)
import Data.Int
import Data.Word
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

type CPrecision = Int64
type Sign = Int32
type Exp = Int64
type Limb = Word64

data TransResult a
  = TransNumber !a
  | TransNaN
  | TransInf
  | TransNegInf
  deriving (Functor, Foldable, Traversable)

data MPFR = MP {
  _precision :: {-# UNPACK #-} !CPrecision,
  _sign      :: {-# UNPACK #-} !Sign,
  _exponent  :: {-# UNPACK #-} !Exp,
  _limbs     :: {-# UNPACK #-} !(ForeignPtr Limb)
}

instance Storable MPFR where
    sizeOf _ = (32)
    alignment _ = alignment (undefined :: Int64)
    peek = error "MPFR.peek: Not needed and not applicable"
    poke p (MP prec s e fp) = do
      (\hsc_ptr -> pokeByteOff hsc_ptr 0) p prec
      (\hsc_ptr -> pokeByteOff hsc_ptr 8) p s
      (\hsc_ptr -> pokeByteOff hsc_ptr 16) p e
      withForeignPtr fp $ \p1 -> (\hsc_ptr -> pokeByteOff hsc_ptr 24) p p1

c'MPFR_RNDN :: CInt
c'MPFR_RNDN = 0

withFormattedNumber :: (Ptr CChar -> Ptr CChar -> IO a) -> IO a
withFormattedNumber f =
  allocaBytes 1024 $ \out ->
    withCString "%512.256R*f" $ \fmt -> f out fmt

readResultNumber :: String -> TransResult Decimal
readResultNumber (' ':s) = readResultNumber s
readResultNumber "nan" = TransNaN
readResultNumber "inf" = TransInf
readResultNumber "-inf" = TransNegInf
readResultNumber n = TransNumber (read (trimZeroes n))

type Mpfr_t = Ptr MPFR

foreign import ccall "mpfr_init"
  c'mpfr_init :: Mpfr_t -> IO ()

foreign import ccall "mpfr_set_str"
  c'mpfr_set_str :: Mpfr_t -> Ptr CChar -> CInt -> CInt -> IO ()

foreign import ccall "mpfr_div"
  c'mpfr_div :: Mpfr_t -> Mpfr_t -> Mpfr_t -> CInt -> IO ()

foreign import ccall "mpfr_pow"
  c'mpfr_pow :: Mpfr_t -> Mpfr_t -> Mpfr_t -> IO ()

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

trans_arity1
  :: (Mpfr_t -> Mpfr_t -> CInt -> IO ()) -> Decimal -> TransResult Decimal
trans_arity1 f x = unsafePerformIO $ withFormattedNumber $ \out fmt ->
  withCString (show (normalizeDecimal x)) $ \xstr ->
  alloca $ \x' ->
  alloca $ \y' -> do
    c'mpfr_init x'
    c'mpfr_set_str x' xstr 10 c'MPFR_RNDN
    c'mpfr_init y'
    f y' x' c'MPFR_RNDN
    c'mpfr_snprintf out 1024 fmt c'MPFR_RNDN y'
    readResultNumber <$> peekCString out

trimZeroes :: String -> String
trimZeroes = reverse . go . reverse
  where
  go ('0':s) = go s
  go ('.':s) = "0." ++ s
  go s = s

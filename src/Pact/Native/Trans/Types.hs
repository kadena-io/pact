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

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

type CPrecision = Int64
type Sign = Int32
type Exp = Int64
type Limb = Word64

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

type Mpfr_t = Ptr MPFR

foreign import ccall "mpfr_init"
  c'mpfr_init :: Mpfr_t -> IO ()

foreign import ccall "mpfr_set_str"
  c'mpfr_set_str :: Mpfr_t -> Ptr CChar -> CInt -> CInt -> IO ()

foreign import ccall "mpfr_pow"
  c'mpfr_pow :: Mpfr_t -> Mpfr_t -> Mpfr_t -> IO ()

foreign import ccall "mpfr_sprintf"
  c'mpfr_sprintf :: Ptr CChar -> Ptr CChar -> CInt -> Mpfr_t -> IO ()

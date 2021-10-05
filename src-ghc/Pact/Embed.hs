{-# LANGUAGE TemplateHaskell #-}
module Pact.Embed where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Unsafe (unsafePackAddressLen)
import System.IO.Unsafe (unsafePerformIO)

embedFile :: FilePath -> Q Exp
embedFile fp = do
  qAddDependentFile fp
  bs@(B.PS ptr off sz) <- runIO $ B.readFile fp
  return
    (AppE
     (VarE 'unsafePerformIO)
     (AppE
      (AppE
       (VarE 'unsafePackAddressLen)
       (LitE (IntegerL $ fromIntegral $ B8.length bs)))
      (LitE (bytesPrimL (mkBytes ptr (fromIntegral off) (fromIntegral sz))))))

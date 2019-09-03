{-# LANGUAGE CPP #-}

module Main where

#if !defined(ghcjs_HOST_OS)
import qualified Pact.GasModel as GasModel
#endif


main :: IO ()
main =
#if defined(ghcjs_HOST_OS)
    error "Error: gas model benchmarking does not exist in GHCJS mode"
#else
    GasModel.main
#endif

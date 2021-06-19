{-# LANGUAGE CPP #-}

module Main where

#if !defined(ghcjs_HOST_OS) && defined(BUILD_TOOL)
import qualified Pact.GasModel.GasModel as GasModel
#endif


main :: IO ()
main =
#if !defined(ghcjs_HOST_OS) && defined(BUILD_TOOL)
    GasModel.main
#else
    error "Error: gas model benchmarking does not exist in GHCJS mode"
#endif

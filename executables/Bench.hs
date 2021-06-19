{-# LANGUAGE CPP #-}

module Main where

#if !defined(ghcjs_HOST_OS) && defined(BUILD_TOOL)
import qualified Pact.Bench as Bench
#endif


main :: IO ()
main =
#if !defined(ghcjs_HOST_OS) && defined(BUILD_TOOL)
    Bench.main
#else
    error "Error: command line REPL does not exist in GHCJS mode"
#endif

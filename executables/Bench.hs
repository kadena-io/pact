{-# LANGUAGE CPP #-}

module Main where

#if !defined(ghcjs_HOST_OS)
import qualified Pact.Bench as Bench
#endif


main :: IO ()
main =
#if defined(ghcjs_HOST_OS)
    error "Error: command line REPL does not exist in GHCJS mode"
#else
    Bench.main
#endif

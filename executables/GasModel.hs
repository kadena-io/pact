{-# LANGUAGE CPP #-}

module Main where

main :: IO ()
main =
#if defined(ghcjs_HOST_OS)
    error "Error: command line REPL does not exist in GHCJS mode"
#else
    gasModelBench
#endif




gasModelBench :: IO ()
gasModelBench = undefined

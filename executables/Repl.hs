{-# LANGUAGE CPP #-}

-- |
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
--

module Main where

#if !defined(ghcjs_HOST_OS)
import qualified Pact.Main as Repl
#endif

main :: IO ()
main =
#if defined(ghcjs_HOST_OS)
    error "Error: command line REPL does not exist in GHCJS mode"
#else
    Repl.main
#endif

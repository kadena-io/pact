{-# LANGUAGE CPP #-}

-- |
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
--

module Main where

#if !defined(ghcjs_HOST_OS) && defined(BUILD_TOOL)
import qualified Pact.Main as Repl
#endif

main :: IO ()
main =
#if !defined(ghcjs_HOST_OS) && defined(BUILD_TOOL)
    Repl.main
#else
    error "Error: command line REPL does not exist in GHCJS mode"
#endif

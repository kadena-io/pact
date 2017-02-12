{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Pact.Main
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.Main
    (
     main
    ) where

import Control.Applicative
import Control.Lens hiding (op)
import Control.Monad.State.Strict
import Data.List
import qualified Data.HashMap.Strict as HM
import Prelude hiding (exp,print,putStrLn,putStr,interact)
import Text.Trifecta as TF hiding (line,err,try,newline)
import System.IO hiding (interact)
import System.Exit
import qualified Options.Applicative as O
import Data.Monoid
import System.Directory
import System.FilePath
import Data.Word (Word16)

import Pact.Repl
import Pact.Compile
import Pact.Types.Runtime hiding ((<>))
import Pact.Server.Server


data Option =
  OVersion |
  OBuiltins |
  OLoad Bool String |
  ORepl |
  OServer FilePath
  deriving (Eq,Show)

replOpts :: O.Parser Option
replOpts =
    O.flag' OVersion (O.short 'v' <> O.long "version" <> O.help "Display version") <|>
    O.flag' OBuiltins (O.short 'b' <> O.long "builtins" <> O.help "List builtins") <|>
    (OServer <$> O.strOption (O.short 's' <> O.long "serve" <> O.metavar "config" <>
                              O.help "Launch REST API server with config file")) <|>
    (OLoad
     <$> O.flag False True
         (O.short 'r' <> O.long "findscript" <>
          O.help "For .pact files, attempts to locate a .repl file to execute.")
     <*> O.argument O.str
        (O.metavar "FILE" <> O.help "File path to compile (if .pact extension) or execute.")) <|>
    pure ORepl -- would be nice to bail on unrecognized args here

argParser :: O.ParserInfo Option
argParser = O.info (O.helper <*> replOpts)
            (O.fullDesc <> O.header "The Pact Smart Contract Language Interpreter")

_testArgs :: String -> O.ParserResult Option
_testArgs = O.execParserPure O.defaultPrefs argParser . words


main :: IO ()
main = do
  as <- O.execParser argParser
  let exitEither _ Left {} = hPutStrLn stderr "Load failed" >> hFlush stderr >> exitFailure
      exitEither m (Right t) = m t >> exitSuccess
      exitLoad = exitEither (\_ -> hPutStrLn stderr "Load successful" >> hFlush stderr)
  case as of
    OServer conf -> serve conf
    OVersion -> putStrLn $ "pact version " ++ unpack pactVersion
    OBuiltins -> echoBuiltins
    OLoad findScript fp
        | isPactFile fp -> do
            script <- if findScript then locatePactReplScript fp else return Nothing
            case script of
              Just s -> execScript s >>= exitLoad
              Nothing -> compileOnly fp >>= exitLoad
        | otherwise -> execScript fp >>= exitLoad
    ORepl -> repl >>= exitEither (const (return ()))

-- | Run heuristics to find a repl script. First is the file name with ".repl" extension;
-- if not, it will see if there is a single ".repl" file in the directory, and if so
-- use that.
locatePactReplScript :: FilePath -> IO (Maybe FilePath)
locatePactReplScript fp = do
  let r = dropExtension fp ++ ".repl"
  b <- doesFileExist r
  if b then return $ Just r
    else do
      let dir = takeDirectory fp
      rs <- filter ((== ".repl") . takeExtension) <$> getDirectoryContents dir
      case rs of
        [a] -> return $ Just $ combine dir a
        _ -> return Nothing


compileOnly :: String -> IO (Either String [Term Name])
compileOnly fp = do
  !pr <- TF.parseFromFileEx exprsOnly fp
  src <- readFile fp
  s <- initReplState (Script fp)
  (`evalStateT` s) $ handleParse pr $ \es -> (sequence <$> forM es (\e -> handleCompile src e (return . Right)))



echoBuiltins :: IO ()
echoBuiltins = do
  defs <- view (eeRefStore.rsNatives) <$> initPureEvalEnv
  forM_ (sort $ HM.keys defs) print

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

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
import Control.Lens
import Control.Monad.State.Strict
import Data.List
import qualified Data.HashMap.Strict as HM
import Prelude hiding (print,putStrLn)
import Text.Trifecta as TF hiding (err)
import System.IO
import System.Exit hiding (die)
import qualified Options.Applicative as O
import System.Directory
import System.FilePath

import "crypto-api" Crypto.Random
import Crypto.Ed25519.Pure

import Pact.Repl
import Pact.Parse
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Server.Server
#ifndef mingw32_HOST_OS
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdInput)
#endif
import Pact.ReplTools
import Pact.Repl.Types
import Pact.Types.Version
import Pact.ApiReq


data Option =
  OVersion
  | OBuiltins
  | OGenKey
  | OLoad { _oFindScript :: Bool, _oDebug :: Bool, _oFile :: String }
  | ORepl
  | OApiReq { _oReqYaml :: FilePath, _oReqLocal :: Bool }
  | OServer { _oConfigYaml :: FilePath }
  deriving (Eq,Show)

replOpts :: O.Parser Option
replOpts =
    O.flag' OVersion (O.short 'v' <> O.long "version" <> O.help "Display version") <|>
    O.flag' OBuiltins (O.short 'b' <> O.long "builtins" <> O.help "List builtins") <|>
    O.flag' OGenKey (O.short 'g' <> O.long "genkey" <> O.help "Generate ED25519 keypair") <|>
    (OServer <$> O.strOption (O.short 's' <> O.long "serve" <> O.metavar "config" <>
                              O.help "Launch REST API server with config file")) <|>
    (OLoad
     <$> O.flag False True
         (O.short 'r' <> O.long "findscript" <>
          O.help "For .pact files, attempts to locate a .repl file to execute.")
     <*> O.flag False True
         (O.short 't' <> O.long "trace" <> O.help "Show trace output")
     <*> O.argument O.str
        (O.metavar "FILE" <> O.help "File path to compile (if .pact extension) or execute.")) <|>
    (OApiReq <$> O.strOption (O.short 'a' <> O.long "apireq" <> O.metavar "REQ_YAML" <>
                             O.help "Format API request JSON using REQ_YAML file")
      <*> O.flag False True (O.short 'l' <> O.long "local" <> O.help "Format for /local endpoint")) <|>
    pure ORepl -- would be nice to bail on unrecognized args here

argParser :: O.ParserInfo Option
argParser = O.info (O.helper <*> replOpts)
            (O.fullDesc <> O.header "The Pact Smart Contract Language Interpreter")

_testArgs :: String -> O.ParserResult Option
_testArgs = O.execParserPure O.defaultPrefs argParser . words


main :: IO ()
main = do
  as <- O.execParser argParser
  let exitEither _ Left {} = die "Load failed"
      exitEither m (Right t) = m t >> exitSuccess
      exitLoad = exitEither (\_ -> hPutStrLn stderr "Load successful" >> hFlush stderr)
  case as of
    OServer conf -> serve conf
    OVersion -> putStrLn $ "pact version " ++ unpack pactVersion
    OBuiltins -> echoBuiltins
    OLoad findScript dolog fp
        | isPactFile fp -> do
            script <- if findScript then locatePactReplScript fp else return Nothing
            case script of
              Just s -> execScript dolog s >>= exitLoad
              Nothing -> compileOnly fp >>= exitLoad
        | otherwise -> execScript dolog fp >>= exitLoad
    ORepl -> getMode >>= generalRepl >>= exitEither (const (return ()))
    OGenKey -> genKeys
    OApiReq cf l -> apiReq cf l

getMode :: IO ReplMode
#ifdef mingw32_HOST_OS
getMode = return Interactive
#else
getMode = queryTerminal stdInput >>= \isatty ->
  return $ if isatty then Interactive else StdinPipe
#endif

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
  s <- initReplState (Script False fp)
  (`evalStateT` s) $ handleParse pr $ \es -> (sequence <$> forM es (\e -> handleCompile src e (return . Right)))

die :: String -> IO b
die msg = hPutStrLn stderr msg >> hFlush stderr >> exitFailure


echoBuiltins :: IO ()
echoBuiltins = do
  defs <- view (eeRefStore.rsNatives) <$> initPureEvalEnv
  forM_ (sort $ HM.keys defs) print


genKeys :: IO ()
genKeys = do
  g :: SystemRandom <- newGenIO
  case generateKeyPair g of
    Left err -> die $ show err
    Right (s,p,_) -> do
      putStrLn $ "public: " ++ unpack (toB16Text $ exportPublic p)
      putStrLn $ "secret: " ++ unpack (toB16Text $ exportPrivate s)

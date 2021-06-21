{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      :  Pact.Main
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.Main ( main ) where

import Prelude hiding (print,putStrLn)

import System.IO
import System.Exit hiding (die)
import System.Directory
import System.FilePath
#ifndef mingw32_HOST_OS
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdInput)
#endif

import qualified Options.Applicative as O

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (unpack)
import Data.Text.Encoding

import Text.Trifecta as TF hiding (err)

import Pact.Coverage
import Pact.Repl
import Pact.Parse
import Pact.Types.Runtime hiding (PublicKey)
import Pact.Server.Server
import Pact.ReplTools
import Pact.Repl.Types
import Pact.Types.Version
import Pact.Types.Crypto
import Pact.Types.SPV
import Pact.ApiReq


data Option =
  OVersion
  | OBuiltins
  | OGenKey
  | OLoad { _oFindScript :: Bool, _oDebug :: Bool, _oCoverage :: Bool, _oFile :: String }
  | ORepl
  | OApiReq { _oReqYaml :: FilePath, _oReqLocal :: Bool }
  | OUnsignedReq { _oReqYaml :: FilePath }
  | OServer { _oConfigYaml :: FilePath }
  | OAddSigsReq { _oKeyFiles :: [FilePath], _oReqLocal :: Bool }
  | OCombineSigs { _oSigFiles :: [FilePath], _oReqLocal :: Bool }
  | OSignCmd { _oSigFile :: [FilePath] }
  deriving (Eq,Show)

replOpts :: O.Parser Option
replOpts =
    O.subparser (addSigParser <> combineSigsParser <> signParser)
    <|>
    O.flag' OVersion (O.short 'v' <> O.long "version" <> O.help "Display version")
    <|>
    O.flag' OBuiltins (O.short 'b' <> O.long "builtins" <> O.help "List builtins")
    <|>
    O.flag' OGenKey (O.short 'g' <> O.long "genkey" <> O.help "Generate ED25519 keypair")
    <|>
    (OServer <$> O.strOption (O.short 's' <> O.long "serve" <> O.metavar "config" <>
                              O.help "Launch REST API server with config file")
    ) <|>
    (OLoad
     <$> O.flag False True
         (O.short 'r' <> O.long "findscript" <>
          O.help "For .pact files, attempts to locate a .repl file to execute.")
     <*> O.flag False True
         (O.short 't' <> O.long "trace" <> O.help "Show trace output")
     <*> O.flag False True
         (O.short 'c' <> O.long "coverage" <> O.help "Generate coverage report coverage/lcov.info")
     <*> O.argument O.str
        (O.metavar "FILE" <> O.help "File path to compile (if .pact extension) or execute.")
    ) <|>
    (OApiReq <$> O.strOption (O.short 'a' <> O.long "apireq" <> O.metavar "REQ_YAML" <>
                             O.help "Format API request JSON using REQ_YAML file")
      <*> localFlag
    ) <|>
    (OUnsignedReq
     <$> O.strOption (O.short 'u' <> O.long "unsigned" <> O.metavar "REQ_YAML" <>
                      O.help "Format unsigned API request JSON using REQ_YAML file")
    ) <|>
    pure ORepl -- would be nice to bail on unrecognized args here

localFlag :: O.Parser Bool
localFlag = O.flag False True (O.short 'l' <> O.long "local" <> O.help "Format for /local endpoint")

combineSigsParser :: O.Mod O.CommandFields Option
combineSigsParser = O.command "combine-sigs" $ O.info (OCombineSigs <$> parser <*> localFlag <**> O.helper) i
  where
    parser = many $ O.strArgument
             ( O.metavar "SIG_FILE"
            <> O.help "A signature file generated by \"pact -u\" or \"pact add-sig\"")
    i = O.fullDesc
        <> O.header synopsis
        <> O.progDesc synopsis
    synopsis = "Combine multiple signature files"

addSigParser :: O.Mod O.CommandFields Option
addSigParser = O.command "add-sig" $ O.info (OAddSigsReq <$> parser <*> localFlag <**> O.helper) i
  where
    parser = many $ O.strArgument
             ( O.metavar "KEY_FILE"
            <> O.help "File with a public / private key pair in the format generated by \"pact -g\"")
    i = O.fullDesc
        <> O.header synopsis
        <> O.progDesc synopsis
    synopsis = "Add a signature to a signature data from stdin"

signParser :: O.Mod O.CommandFields Option
signParser = O.command "sign" $ O.info (OSignCmd <$> parser <**> O.helper) i
  where
    parser = many $ O.strArgument
             ( O.metavar "KEY_FILE"
            <> O.help "File with a public / private key pair in the format generated by \"pact -g\"")
    i = O.fullDesc
        <> O.header synopsis
        <> O.progDesc synopsis
    synopsis = "Sign arbitrary base64url-encoded data from stdin"


argParser :: O.ParserInfo Option
argParser = O.info (O.helper <*> replOpts)
            (O.fullDesc <> O.header "The Pact Smart Contract Language Interpreter")

_testArgs :: String -> O.ParserResult Option
_testArgs = O.execParserPure O.defaultPrefs argParser . words


main :: IO ()
main = O.execParser argParser >>= \as -> case as of

    OServer conf -> serve conf noSPVSupport

    OVersion -> putStrLn $ "pact version " ++ unpack pactVersion

    OBuiltins -> echoBuiltins

    OLoad findScript dolog coverage fp
        | isPactFile fp -> do
            script <- if findScript then locatePactReplScript fp else return Nothing
            case script of
              Just s -> runScript dolog s coverage
              Nothing -> compileOnly fp >>= exitLoad
        | otherwise -> runScript dolog fp coverage

    ORepl -> getMode >>= generalRepl >>= exitEither (const (return ()))

    OGenKey -> genKeys

    OApiReq cf l -> apiReq cf l

    OUnsignedReq cf -> uapiReq cf

    OAddSigsReq kf l -> BS8.putStrLn =<< addSigsReq kf l =<< BS.getContents

    OCombineSigs sigs l -> BS8.putStrLn =<< combineSigs sigs l

    OSignCmd kfs -> BS8.putStrLn =<< signCmd kfs =<< fmap (encodeUtf8 . T.strip) T.getContents

  where

    exitEither _ Left {} = die "Load failed"
    exitEither m (Right t) = m t >> exitSuccess

    exitLoad = exitEither (\_ -> hPutStrLn stderr "Load successful" >> hFlush stderr)

    runScript dolog fp coverage = do
      (ref,coverF) <- handleCov coverage
      r <- execScriptF dolog fp coverF
      withCurrentDirectory (takeDirectory fp) $ void $ traverse writeCovReport ref
      exitLoad r
        where
          handleCov False = return (Nothing,id)
          handleCov True = do
            (ref,adv) <- mkCoverageAdvice
            return (Just ref, set (rEnv . eeAdvice) adv)


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
  srcBS <- BS.readFile fp
  let src = unpack $ decodeUtf8 srcBS
  s <- initReplState (Script False fp) Nothing
  (`evalStateT` s) $ handleParse pr $ \es -> (sequence <$> forM es (\e -> handleCompile src e (return . Right)))

die :: String -> IO b
die msg = hPutStrLn stderr msg >> hFlush stderr >> exitFailure


echoBuiltins :: IO ()
echoBuiltins = do
  defs <- view (eeRefStore.rsNatives) <$> (initPureEvalEnv Nothing)
  forM_ (sort $ HM.keys defs) print


genKeys :: IO ()
genKeys = do
  kp <- genKeyPair defaultScheme
  putStrLn $ "public: " ++ unpack (toB16Text $ getPublic kp)
  putStrLn $ "secret: " ++ unpack (toB16Text $ getPrivate kp)

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Pact.Repl
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- REPL/interactive interpreter for Pact. Includes
-- "Repl lib" automatically for non-blockchain interactive
-- functionality.
--

module Pact.Repl
    (
     interactiveRepl,generalRepl
    ,evalRepl,evalRepl',ReplMode(..)
    ,execScript,execScript'
    ,initEvalEnv,initPureEvalEnv,initReplState
    ,handleParse,handleCompile
    ,isPactFile
    ,ReplState(..),rEnv,rEvalState,rMode,rOut
    ) where

import Control.Applicative
import Control.Lens hiding (op)
import Control.Monad.Catch
import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Word (Word8)
import Prelude hiding (exp,print,putStrLn)
import Text.Trifecta as TF hiding (line,err,try,newline)
import qualified Text.Trifecta.Delta as TF
import System.IO
import Text.Trifecta.Delta
import Control.Concurrent
import System.Console.Haskeline
  (runInputT, withInterrupt, InputT, getInputLine, handleInterrupt,
   CompletionFunc, completeQuotedWord, completeWord, listFiles,
   filenameWordBreakChars, Settings(Settings), simpleCompletion)

import Pact.Parse
import Pact.Types.Runtime
import Pact.Native
import Pact.Repl.Types
import Pact.ReplCommon


interactiveRepl :: IO (Either () (Term Name))
interactiveRepl = generalRepl Interactive

completeFn :: (MonadIO m, MonadState ReplState m) => CompletionFunc m
completeFn = completeQuotedWord (Just '\\') "\"" listFiles $
  completeWord (Just '\\') ("\"\'" ++ filenameWordBreakChars) $ \str -> do
    modules <- use (rEnv . eeRefStore . rsModules)
    let namesInModules = toListOf (traverse . _2 . to HM.keys . each) modules
        allNames = concat
          [ namesInModules
          , nameOfModule <$> HM.keys modules
          , unName <$> HM.keys nativeDefs
          ]
        matchingNames = filter (str `isPrefixOf`) (unpack <$> allNames)
    pure $ simpleCompletion <$> matchingNames

  where
    unName :: Name -> Text
    unName (QName _ name _) = name
    unName (Name    name _) = name

    nameOfModule :: ModuleName -> Text
    nameOfModule (ModuleName name) = name

replSettings :: (MonadIO m, MonadState ReplState m) => Settings m
replSettings = Settings
  completeFn
  Nothing -- don't write history
  True -- automatically add each line to history

generalRepl :: ReplMode -> IO (Either () (Term Name))
generalRepl m = initReplState m >>= \s -> case m of
  Interactive -> evalStateT
    (runInputT replSettings (withInterrupt (haskelineLoop [] Nothing)))
    (setReplLib s)
  _StdInPipe -> runPipedRepl s stdin

runPipedRepl :: ReplState -> Handle -> IO (Either () (Term Name))
runPipedRepl s@ReplState{..} h =
    evalStateT (useReplLib >> pipeLoop h Nothing) s

errToUnit :: Functor f => f (Either e a) -> f (Either () a)
errToUnit a = either (const (Left ())) Right <$> a

type HaskelineRepl = InputT (StateT ReplState IO)

-- | Main loop for interactive input.
--
-- Swallows ctrl-c, requiring ctrl-d to exit. Includes autocomplete and
-- readline.
haskelineLoop :: [String] -> Maybe (Term Name) -> HaskelineRepl (Either () (Term Name))
haskelineLoop prevLines lastResult =
  let
    getNonEmptyInput = do
      let lineHeader = if null prevLines then "pact> " else "....> "
      line <- getInputLine lineHeader

      case line of
        Nothing -> maybe rSuccess (return.Right) lastResult
        Just "" -> haskelineLoop prevLines lastResult
        Just input -> handleMultilineInput input prevLines lastResult

    interruptHandler = do
      liftIO $ putStrLn "Type ctrl-d to exit pact"
      haskelineLoop [] lastResult

  in handleInterrupt interruptHandler getNonEmptyInput

-- | Interactive multiline input loop.
handleMultilineInput
  :: String   -- ^ latest input line
  -> [String] -- ^ previous input lines
  -> Maybe (Term Name) -- ^ previous result
  -> HaskelineRepl (Either () (Term Name))
handleMultilineInput input prevLines lastResult =
  let multilineInput = prevLines <> [input]
      joinedInput = unlines multilineInput
  in case TF.parseString exprsOnly mempty joinedInput of

       -- Check where our parser crashed to see if it's at the end of
       -- input. If so, we can assume it unexpectedly hit EOF,
       -- indicating open parens / continuing input.
       Failure (ErrInfo _ [TF.Lines x y z w])
         -- check we've consumed:
         -- n + 1 newlines (unlines concats a newline at the end)
         | x == fromIntegral (length prevLines + 1) &&
         -- and 0 chars on the last line
           y == 0 &&
         -- all the bytes
           z == fromIntegral (utf8BytesLength joinedInput) &&
         -- but none since the trailing newline
           w == 0

           -- If so, continue accepting input
           -> haskelineLoop multilineInput lastResult

       Failure e -> do
         liftIO $ print e
         haskelineLoop [] Nothing

       parsed -> do
         ret <- lift $ errToUnit $ parsedCompileEval joinedInput parsed
         case ret of
           Left _  -> haskelineLoop [] Nothing
           Right t -> haskelineLoop [] (Just t)

toUTF8Bytes :: String -> [Word8]
toUTF8Bytes = BS.unpack . encodeUtf8 . Text.pack

utf8BytesLength :: String -> Int
utf8BytesLength = length . toUTF8Bytes

-- | Main loop for non-interactive (piped) input
pipeLoop :: Handle -> Maybe (Term Name) -> Repl (Either () (Term Name))
pipeLoop h lastResult = do
  isEof <- liftIO (hIsEOF h)
  let retVal = maybe rSuccess (return.Right) lastResult
  if isEof then retVal else do
    line <- trim <$> liftIO (hGetLine h)
    r <- if null line then rSuccess
         else do
           d <- getDelta
           errToUnit $ parsedCompileEval line $ TF.parseString exprsOnly d line
    case r of
      Left _ -> do
        outStrLn HErr "Aborting execution"
        return r
      Right t -> pipeLoop h (Just t)

getDelta :: Repl Delta
getDelta = do
  m <- use rMode
  case m of
    (Script _ file) -> return $ Directed (BS.fromString file) 0 0 0 0
    _ -> return mempty

rSuccess :: Monad m => m (Either a (Term Name))
rSuccess = return $ Right $ toTerm True

-- | Workhorse to load script; also checks/reports test failures
execScript :: Bool -> FilePath -> IO (Either () (Term Name))
execScript dolog f = do
  (r,ReplState{..}) <- execScript' (Script dolog f) f
  case r of
    Left _ -> return $ Left ()
    Right t -> do
      LibState{..} <- readMVar $ _eePactDbVar _rEnv
      fs <- fmap sequence $ forM _rlsTests $ \TestResult{..} -> case trFailure of
        Nothing -> return (Just ())
        Just (i,e) -> do
          hPutStrLn stderr $ renderInfo (_faInfo i) ++ ": " ++ unpack e
          return Nothing
      maybe (return $ Left ()) (const (return (Right t))) fs


execScript' :: ReplMode -> FilePath -> IO (Either String (Term Name),ReplState)
execScript' m fp = do
  s <- initReplState m
  runStateT (useReplLib >> loadFile fp) s


evalRepl :: ReplMode -> String -> IO (Either String (Term Name))
evalRepl m cmd = initReplState m >>= evalStateT (evalRepl' cmd)

_eval :: String -> IO (Term Name)
_eval cmd = evalRepl (Script True "_eval") cmd >>= \r ->
            case r of Left e -> throwM (userError $ " Failure: " ++ show e); Right v -> return v

_run :: String -> IO ()
_run cmd = void $ evalRepl Interactive cmd

_testAccounts :: IO ()
_testAccounts = void $ execScript False "examples/accounts/accounts.repl"

_testBench :: IO ()
_testBench = void $ execScript False "tests/bench/bench"

_testCP :: IO ()
_testCP = void $ execScript False "examples/cp/cp.repl"

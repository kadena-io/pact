{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Pact.ReplTools where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.List
import qualified Data.HashMap.Strict as HM
import Prelude hiding (print,putStrLn)
import Text.Trifecta as TF hiding (line)
import qualified Text.Trifecta.Delta as TF
import System.IO
import System.Console.Haskeline
  (runInputT, withInterrupt, InputT, getInputLine, handleInterrupt,
   CompletionFunc, completeQuotedWord, completeWord, listFiles,
   filenameWordBreakChars, Settings(Settings), simpleCompletion)

import Pact.Parse
import Pact.Types.Runtime
import Pact.Native
import Pact.Repl
import Pact.Repl.Types

------------------------------------------------------------------------------
-- Moved from Pact.Repl
------------------------------------------------------------------------------

interactiveRepl :: IO (Either () (Term Name))
interactiveRepl = generalRepl Interactive

-- Note(emily): revisit whether we want all _module_ names, or all interface names as well.
completeFn :: (MonadIO m, MonadState ReplState m) => CompletionFunc m
completeFn = completeQuotedWord (Just '\\') "\"" listFiles $
  completeWord (Just '\\') ("\"\'" ++ filenameWordBreakChars) $ \str -> do
    modules <- use (rEnv . eeRefStore . rsModules)
    let namesInModules = toListOf (traverse . mdRefMap . to HM.keys . each) modules
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
    nameOfModule (ModuleName name _) = name

replSettings :: (MonadIO m, MonadState ReplState m) => Settings m
replSettings = Settings
  completeFn
  (Just ".pact-history") -- write history to ".pact-history"
  True -- automatically add each line to history

generalRepl :: ReplMode -> IO (Either () (Term Name))
generalRepl m = initReplState m Nothing >>= \s -> case m of
  Interactive -> evalStateT
    (runInputT replSettings (withInterrupt (haskelineLoop [] Nothing)))
    (setReplLib s)
  _StdInPipe -> runPipedRepl s stdin

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

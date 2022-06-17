{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Pact core minimal repl
--


module Main where

import Control.Monad.IO.Class(MonadIO(..))
import Control.Monad.Catch
import System.Console.Haskeline
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.IORef
import Data.Foldable(traverse_)

import Pact.Core.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Persistence
import Pact.Core.Pretty


main :: IO ()
main = do
  pactDb <- mockPactDb
  ref <- newIORef (ReplState mempty emptyLoaded pactDb)
  runInputT defaultSettings (loop ref)
  where
  displayOutput = \case
    InterpretValue v -> outputStrLn (show (pretty v))
    InterpretLog t -> outputStrLn (T.unpack t)
  catch' ref ma = catchAll ma (\e -> outputStrLn (show e) *> loop ref)
  loop ref = do
    minput <- getInputLine "pact>"
    case minput of
      Nothing -> outputStrLn "goodbye"
      Just input -> case T.strip (T.pack input) of
        "" -> loop ref
        ":quit" -> outputStrLn "goodbye"
        i | T.isPrefixOf ":load" i -> let
          file = T.unpack (T.drop 6 i)
          in catch' ref $ do
            vs <- liftIO $ runReplT ref (interpretProgramFile file)
            traverse_ displayOutput vs
            loop ref
        i -> catch' ref $ do
          out <- liftIO $ runReplT ref (interpretExpr (T.encodeUtf8 i))
          displayOutput (InterpretValue  out)
          loop ref

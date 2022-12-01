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

import Control.Lens
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Catch
import Control.Monad.Except
import System.Console.Haskeline
import Data.IORef
import Data.Foldable(traverse_)
import Data.Text(Text)
import Control.Monad.Trans(lift)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as Set

import Pact.Core.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Persistence
import Pact.Core.Pretty
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Info

main :: IO ()
main = do
  pactDb <- mockPactDb
  g <- newIORef mempty
  evalLog <- newIORef Nothing
  ref <- newIORef (ReplState mempty emptyLoaded pactDb g evalLog)
  runReplT ref (runInputT replSettings (loop lispInterpretBundle ref)) >>= \case
    Left err -> do
      putStrLn "Exited repl session with error:"
      putStrLn $ T.unpack $ replError (ReplSource "(interactive)" "") err
    _ -> pure ()
  where
  replSettings = Settings (replCompletion rawBuiltinNames) (Just ".pc-history") True
  displayOutput = \case
    InterpretValue v -> outputStrLn (show (pretty v))
    InterpretLog t -> outputStrLn (T.unpack t)
  catch' bundle ref ma = catchAll ma (\e -> outputStrLn (show e) *> loop bundle ref)
  loop bundle ref = do
    minput <- fmap T.pack <$> getInputLine "pact>"
    case minput of
      Nothing -> outputStrLn "goodbye"
      Just input | T.null input -> loop bundle ref
      Just input -> case parseReplAction (T.strip input) of
        Nothing -> do
          outputStrLn "Error: Expected command [:load, :type, :syntax, :debug] or expression"
          loop bundle ref
        Just ra -> case ra of
          RALoad txt -> let
            file = T.unpack txt
            in catch' bundle ref $ do
              source <- liftIO (B.readFile file)
              vs <- lift (program bundle source)
              traverse_ displayOutput vs
              loop bundle ref
          RASetLispSyntax -> loop lispInterpretBundle ref
          RASetNewSyntax -> loop lispInterpretBundle ref
          RATypecheck inp -> catch' bundle ref $ do
            let inp' = T.strip inp
            out <- lift (exprType bundle (T.encodeUtf8 inp'))
            outputStrLn (show (pretty out))
            loop bundle ref
          RASetFlag flag -> do
            liftIO (modifyIORef' ref (over replFlags (Set.insert flag)))
            outputStrLn $ unwords ["set debug flag for", prettyReplFlag flag]
            loop bundle ref
          RADebugAll -> do
            liftIO (modifyIORef' ref (set replFlags (Set.fromList [minBound .. maxBound])))
            outputStrLn $ unwords ["set all debug flags"]
            loop bundle ref
          RADebugNone -> do
            liftIO (modifyIORef' ref (set replFlags mempty))
            outputStrLn $ unwords ["Remove all debug flags"]
            loop bundle ref
          RAExecuteExpr src -> catch' bundle ref $ do
            eout <- lift (tryError (expr bundle (T.encodeUtf8 src)))
            case eout of
              Right out -> displayOutput (InterpretValue out)
              Left err -> let
                rs = ReplSource "(interactive)" input
                in outputStrLn (T.unpack (replError rs err))
            loop bundle ref

tryError :: MonadError a m => m b -> m (Either a b)
tryError ma =
  catchError (Right <$> ma) (pure . Left)

replError
  :: ReplSource
  -> PactErrorI
  -> Text
replError (ReplSource file src) pe =
  let srcLines = T.lines src
      pei = view peInfo pe
      slice = withLine $ take (max 1 (_liSpan pei)) $ drop (_liLine pei) srcLines
      colMarker = "  | " <> T.replicate (_liColumn pei - 1) " " <> "^"
      errRender = renderPactError pe
      fileErr = file <> ":" <> T.pack (show (_liLine pei)) <> ":" <> T.pack (show (_liColumn pei)) <> ": "
  in T.unlines ([fileErr <> errRender] ++ slice ++ [colMarker])
  where
  withLine lns = zipWith (\i e -> T.pack (show i) <> " | " <> e) [0..] lns

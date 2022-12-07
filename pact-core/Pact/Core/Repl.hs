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
import Control.Monad.Catch
import Control.Monad.Except
import System.Console.Haskeline
import Data.IORef
import Data.Foldable(traverse_)
import Data.Text(Text)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as Set

import Pact.Core.Persistence
import Pact.Core.Pretty
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Info

import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils

main :: IO ()
main = do
  pactDb <- mockPactDb
  g <- newIORef mempty
  evalLog <- newIORef Nothing
  ref <- newIORef (ReplState mempty emptyLoaded pactDb g evalLog)
  runReplT ref (runInputT replSettings (loop lispInterpretBundle)) >>= \case
    Left err -> do
      putStrLn "Exited repl session with error:"
      putStrLn $ T.unpack $ replError (ReplSource "(interactive)" "") err
    _ -> pure ()
  where
  replSettings = Settings (replCompletion rawBuiltinNames) (Just ".pc-history") True
  displayOutput = \case
    InterpretValue v -> outputStrLn (show (pretty v))
    InterpretLog t -> outputStrLn (T.unpack t)
  catch' bundle ma = catchAll ma (\e -> outputStrLn (show e) *> loop bundle)
  loop bundle = do
    minput <- fmap T.pack <$> getInputLine "pact>"
    case minput of
      Nothing -> outputStrLn "goodbye"
      Just input | T.null input -> loop bundle
      Just input -> case parseReplAction (T.strip input) of
        Nothing -> do
          outputStrLn "Error: Expected command [:load, :type, :syntax, :debug] or expression"
          loop bundle
        Just ra -> case ra of
          RALoad txt -> let
            file = T.unpack txt
            in catch' bundle $ do
              source <- liftIO (B.readFile file)
              eout <- lift $ tryError $ program bundle source
              case eout of
                Right vs -> traverse_ displayOutput vs
                Left err -> let
                  rs = ReplSource (T.pack file) (T.decodeUtf8 source)
                  in outputStrLn (T.unpack (replError rs err))
              loop bundle
          RASetLispSyntax -> loop lispInterpretBundle
          RASetNewSyntax -> loop lispInterpretBundle
          RATypecheck inp -> catch' bundle  $ do
            let inp' = T.strip inp
            out <- lift (exprType bundle (T.encodeUtf8 inp'))
            outputStrLn (show (pretty out))
            loop bundle
          RASetFlag flag -> do
            lift (replFlags %= Set.insert flag)
            outputStrLn $ unwords ["set debug flag for", prettyReplFlag flag]
            loop bundle
          RADebugAll -> do
            lift (replFlags .= Set.fromList [minBound .. maxBound])
            outputStrLn $ unwords ["set all debug flags"]
            loop bundle
          RADebugNone -> do
            lift (replFlags .= Set.empty)
            outputStrLn $ unwords ["Remove all debug flags"]
            loop bundle
          RAExecuteExpr src -> catch' bundle $ do
            eout <- lift (tryError (expr bundle (T.encodeUtf8 src)))
            case eout of
              Right out -> displayOutput (InterpretValue out)
              Left err -> let
                rs = ReplSource "(interactive)" input
                in outputStrLn (T.unpack (replError rs err))
            loop bundle

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
      slice = withLine (_liLine pei) $ take (max 1 (_liSpan pei)) $ drop (_liLine pei) srcLines
      colMarker = "  | " <> T.replicate (_liColumn pei - 1) " " <> "^"
      errRender = renderPactError pe
      fileErr = file <> ":" <> T.pack (show (_liLine pei)) <> ":" <> T.pack (show (_liColumn pei)) <> ": "
  in T.unlines ([fileErr <> errRender] ++ slice ++ [colMarker])
  where
  withLine st lns = zipWith (\i e -> T.pack (show i) <> " | " <> e) [st ..] lns

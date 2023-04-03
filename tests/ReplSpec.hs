-- |
{-# LANGUAGE OverloadedStrings #-}

module ReplSpec where

import Test.Hspec

import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import System.Process
import qualified Data.ByteString as BS
import Control.Concurrent (threadDelay)
import GHC.IO.Handle (hClose)

spec :: Spec
spec = describe "ReplSpec" $ do
  it "should not print literal via `Show` (regression #1101)" $ do
    let src = "(print 1)"
    out <- liftIO (runInteractive src)
    out `shouldSatisfy` outputSatisfy containsNoTermInfo

  it "should not print `ErrInfo` via `Show` (regression #1164)" $ do
    let src = "(module m g (defcap g () true) (defcap OFFERED (pid:string) true) \
              \ (defpact sale () (step (= (create-capability-guard (OFFERED (pact-id)))\
              \ (create-capability-guard (OFFERED pact-id)))))))"
    out <- liftIO (runInteractive src)
    out `shouldSatisfy` outputSatisfy containsNoErrInfo


runInteractive :: ByteString -> IO (ByteString, ByteString)
runInteractive src = do
  let cp = (shell "pact")
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  (Just inH, Just outH, Just errH, ph) <- createProcess cp
  BS.hPut inH src
  hClose inH
  threadDelay 20000
  (out, err) <- readTillPrompt outH errH mempty mempty

  terminateProcess ph

  pure (out, err)
  where
    readTillPrompt outH errH o e = do
      out <- BS.hGetNonBlocking outH 4046
      err <- BS.hGetNonBlocking errH 4096

      pure (o <> out, e <> err)

      -- if not ("pact>" `BS.isInfixOf` err)
      --   then threadDelay 20000 >>  readTillPrompt outH errH o e
      --   else pure (o <> out, e <> err)


outputSatisfy :: (ByteString -> Bool) -> (ByteString, ByteString) ->Bool
outputSatisfy f (o, e) = f o && f e

containsNoErrInfo :: ByteString -> Bool
containsNoErrInfo s = not ("_errDeltas" `BS.isInfixOf` s)

containsNoTermInfo :: ByteString -> Bool
containsNoTermInfo s = not ("_tLiteral" `BS.isInfixOf` s)

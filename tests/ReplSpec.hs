-- |
{-# LANGUAGE OverloadedStrings #-}

module ReplSpec where

import Test.Hspec

import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Control.Concurrent (threadDelay)
import System.Posix.Pty
import Control.Monad (void)

spec :: Spec
spec = describe "ReplSpec" $ do
  it "should not print literal via `Show` (regression #1101)" $ do
    let src = "(print 1)"
    out <- liftIO (runInteractive src)
    out `shouldSatisfy` containsNoTermInfo

  it "should not print `ErrInfo` via `Show` (regression #1164)" $ do
    let src = "(module m g (defcap g () true) (defcap OFFERED (pid:string) true) \
              \ (defpact sale () (step (= (create-capability-guard (OFFERED (pact-id)))\
              \ (create-capability-guard (OFFERED pact-id)))))))"
    out <- liftIO (runInteractive src)
    out `shouldSatisfy`containsNoErrInfo


runInteractive :: ByteString -> IO ByteString
runInteractive src = do
  (pty, _) <- spawnWithPty Nothing True "pact" [] (100,100)
  threadDelay 2000
  writePty pty (src <> "\n")
  threadDelay 2000
  void $ go pty mempty -- the first pact prompt
  go pty mempty
  where
    go pty o = do
      content <- readPty pty
      if "pact>" `BS.isInfixOf` content
      then pure o
      else go pty (o <> content)

containsNoErrInfo :: ByteString -> Bool
containsNoErrInfo s = not ("_errDeltas" `BS.isInfixOf` s)

containsNoTermInfo :: ByteString -> Bool
containsNoTermInfo s = not ("_tLiteral" `BS.isInfixOf` s)

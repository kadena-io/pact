-- |
{-# LANGUAGE OverloadedStrings #-}

module ReplSpec where

import Test.Hspec

import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import System.Posix.Pty (spawnWithPty, writePty, readPty, closePty)
import System.Process (terminateProcess)
import Control.Monad (void)
import System.FilePath ((</>))
import Paths_pact (getBinDir)

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

-- | Execute 'src' inside a pseudo-terminal running the pact repl and returns the repl output.
runInteractive :: ByteString -> IO ByteString
runInteractive src = do
  binPath <- getBinDir
  let pactPath = binPath </> "pact"
  (pty, ph) <- spawnWithPty Nothing True pactPath [] (100,100)
  -- Read until we reach the first pact prompt to ensure
  -- the repl is ready.
  void $ seekPactPrompt pty mempty
  writePty pty (src <> "\n")
  out <- seekPactPrompt pty mempty
  terminateProcess ph
  closePty pty
  pure out
  where
    -- We recursively collect output using 'readPty' (line-based reading),
    -- until we encounter the pact prompt.
    seekPactPrompt pty o = do
      content <- readPty pty
      if "pact>" `BS.isInfixOf` content
      then pure o
      else seekPactPrompt pty (o <> content)

-- | Check if 's' did not use the show instance of 'ErrInfo'
containsNoErrInfo :: ByteString -> Bool
containsNoErrInfo s = not ("_errDeltas" `BS.isInfixOf` s)

-- | Check if 's' did not use the show instance of 'TLiteral'
containsNoTermInfo :: ByteString -> Bool
containsNoTermInfo s = not ("_tLiteral" `BS.isInfixOf` s)

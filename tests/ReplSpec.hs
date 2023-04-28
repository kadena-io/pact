-- |
{-# LANGUAGE OverloadedStrings #-}

module ReplSpec where

import Test.Hspec

import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Posix.Pty (spawnWithPty, writePty, readPty, closePty)
import System.Process (terminateProcess)
import Control.Monad (void)

import Pact.Types.Runtime

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
    traverse (uncurry disabledNativeTest) $
      [ ("enumerate", FlagDisablePact40)
        ()
      ]


disabledNativeTest :: ByteString -> ExecutionFlag -> Spec
disabledNativeTest ntv flag = do
  let flagName = BS8.pack (drop 4 (show flag))
  it ("Should disable " <> BS8.unpack ntv <> " on flag " <> BS8.unpack flagName) $ do
    let cmd = "(env-exec-config [' "<> flagName <> "]) " <> ntv
    out <- liftIO (runInteractive cmd)
    out `shouldSatisfy` BS.isInfixOf ("Cannot resolve " <> ntv)

-- | Execute 'src' inside a pseudo-terminal running the pact repl and returns the repl output.
runInteractive :: ByteString -> IO ByteString
runInteractive src = do
  (pty, ph) <- spawnWithPty Nothing True "cabal" ["run", "pact"] (100,100)
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

{--module TestRunner
  () where
--}
import Pact.Server.Server
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (when)
import System.Directory  

_testDir, _testLogDir, _testConfigFilePath, testPort :: String
_testDir = "tests/resume/"
_testLogDir = "tests/resume/test-log/"
_testConfigFilePath = "tests/resume/test-config.yaml"
testPort = "8080"

_logFiles :: [String]
_logFiles = ["access.log","commands.sqlite","error.log","pact.sqlite"]

main :: IO ()
main = do
  withAsync (serve _testConfigFilePath) $ \a -> do
      _ <- threadDelay (20000000)
      print (asyncThreadId a)
      isCancelled <- poll a
      print isCancelled

  print "server should be down. check"
  _ <- threadDelay (10000000)
  print "time to check ended. Cleaning up now"
  flushDb

flushDb :: IO ()
flushDb = mapM_ deleteIfExists _logFiles
  where deleteIfExists filename = do
          let fp = _testLogDir ++ filename
          isFile <- doesFileExist fp
          when isFile $ removeFile fp

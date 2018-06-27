{--module TestRunner
  () where
--}
import Pact.Server.Server
import Control.Concurrent
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
  threadId <- runServer
  _ <- threadDelay (20000000)
  killThread threadId
  print threadId
  flushDb

runServer :: IO ThreadId
runServer = forkIO (serve _testConfigFilePath)

flushDb :: IO ()
flushDb = mapM_ deleteIfExists _logFiles
  where deleteIfExists filename = do
          let fp = _testLogDir ++ filename
          isFile <- doesFileExist fp
          when isFile $ removeFile fp


{--runPactServer :: String -> IO ProcessHandle
runPactServer args = do
  let p = proc "pactserver" $ words args
  (_,_,_, procHandle) <- createProcess p
  sleep 1
  return procHadle--}

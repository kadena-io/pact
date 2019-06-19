{-# LANGUAGE OverloadedStrings #-}

module HistoryServiceSpec (spec) where


import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Strict
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import System.Directory
import Test.Hspec
import Data.Default

import Pact.Server.History.Persistence as DB
import Pact.Server.History.Service
import Pact.Server.History.Types
import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Server
import Pact.Types.Term
import Pact.Types.Runtime (PactError(..),PactErrorType(..))
import Pact.Types.PactValue (PactValue)
import Pact.Types.Pretty (viaShow)


histFile :: FilePath
histFile = fp ++ "/commands.sqlite"

spec :: Spec
spec = describe "roundtrip" $ after_ deleteTempFile $ testHistoryDB

deleteTempFile :: IO ()
deleteTempFile = do
  isFile <- doesFileExist histFile
  when isFile $ removeFile histFile


dbg :: String -> IO ()
-- dbg = putStrLn   -- <- USE THIS TO DEBUG HISTORY STUFF
dbg = const $ return ()

fp :: FilePath
fp = "tests/test-log"

cmd :: Command ByteString
cmd = Command "" [] initialHash

rq :: RequestKey
rq = RequestKey pactInitialHash

res :: Either PactError PactValue
res = Left $ PactError TxFailure def def . viaShow $ ("some error message" :: String)

cr :: CommandResult Hash
cr = CommandResult rq Nothing (PactResult res) (Gas 0) Nothing Nothing Nothing Nothing

results :: HashMap.HashMap RequestKey (CommandResult Hash)
results = HashMap.fromList [(rq, cr)]

initHistory :: IO (HistoryEnv,HistoryState)
initHistory = do
  (inC,histC) <- initChans
  replayFromDisk' <- ReplayFromDisk <$> newEmptyMVar
  let env = initHistoryEnv histC inC (Just fp) dbg replayFromDisk'
  pers <- setupPersistence dbg (Just fp) replayFromDisk'
  let hstate = HistoryState { _registeredListeners = HashMap.empty, _persistence = pers }
  return (env,hstate)

testHistoryDB :: Spec
testHistoryDB = do
  runIO $ deleteTempFile
  pirs <- runIO $ do
    (env,hstate) <- initHistory
    (pirs,_,_) <- runRWST startup env hstate
    DB.closeDB $ dbConn (_persistence hstate)
    return pirs

  it "should have results" $ pirs `shouldBe` (PossiblyIncompleteResults results)

  (env',hstate') <- runIO $ initHistory
  replay <- runIO $ takeMVar $ case _replayFromDisk env' of ReplayFromDisk d -> d

  it "should replay command" $ replay `shouldBe` [cmd]

  pirs' <- runIO $ do
    (pirs',_,_) <- runRWST replay' env' hstate'
    DB.closeDB $ dbConn (_persistence hstate')
    return pirs'

  it "should have replay results" $ pirs' `shouldBe` (PossiblyIncompleteResults results)



startup :: HistoryService PossiblyIncompleteResults
startup = do
  addNewKeys [cmd]
  updateExistingKeys results
  mv <- liftIO $ newEmptyMVar
  queryForResults (HashSet.singleton rq, mv)
  v <- liftIO $ takeMVar mv
  return v


replay' :: HistoryService PossiblyIncompleteResults
replay' = do
  mv <- liftIO $ newEmptyMVar
  queryForResults (HashSet.singleton rq, mv)
  liftIO $ takeMVar mv

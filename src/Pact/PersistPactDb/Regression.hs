{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pact.PersistPactDb.Regression
  (DbEnv(..),
   initDbEnv,
   runRegression,
   regressPure) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Lens hiding ((.=))
import Control.DeepSeq
import Data.Text(pack)
import Data.Foldable(for_)

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM

import Pact.PersistPactDb
import Pact.Persist
import Pact.Types.Runtime
import Pact.Persist.Pure (initPureDb,persister,PureDb)
import Data.Aeson
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Repl
import Pact.Repl.Types
import Pact.Native (nativeDefs)
import Pact.Types.RowData

import qualified Pact.JSON.Encode as J
import Pact.JSON.Legacy.Value

loadModule :: IO (ModuleName, ModuleData Ref, PersistModuleData)
loadModule = do
  let fn = "tests/pact/simple.repl"
  (r,s) <- execScript' (Script False fn) fn
  let mn = ModuleName "simple" Nothing
  case r of
    Left a -> throwFail $ "module load failed: " ++ show a
    Right _ -> case preview (rEvalState . evalRefs . rsLoadedModules . ix mn) s of
      Just (md,_) -> case traverse (traverse toPersistDirect) md of
        Right md' -> return (mn,md,md')
        Left e -> throwFail $ "toPersistDirect failed: " ++ show e
      Nothing -> throwFail $ "Failed to find module 'simple': " ++
        show (view (rEvalState . evalRefs . rsLoadedModules) s)

nativeLookup :: NativeDefName -> Maybe (Term Name)
nativeLookup (NativeDefName n) = case HM.lookup n nativeDefs of
  Just (Direct t) -> Just t
  _ -> Nothing

runRegression :: PactDb p -> MVar p -> IO ()
runRegression pdb v = do
  (Just t1) <- _beginTx pdb Transactional v
  let user1 = "user1"
      usert = UserTables user1
      toPV :: ToTerm a => a -> PactValue
      toPV = toPactValueLenient . toTerm'
  _createUserTable pdb user1 "free.some-module" v
  assertEquals' "output of commit 2"
    [ TxLogJson $ J.encodeJsonText $ TxLog "SYS_usertables" "user1" $
      LegacyValue $ object
       [ "utModule" .= object
         [ "name" .= String "some-module"
         , "namespace" .= String "free"
         ]
       ]
    ]
    (_commitTx pdb v)
  void $ _beginTx pdb Transactional v
  assertEquals' "user table info correct" (Just "free.some-module") $ _getUserTableInfo pdb user1 v
  assertEquals' "user table info missing" Nothing $ _getUserTableInfo pdb "user2" v
  let row = RowData RDV0 $ ObjectMap $ M.fromList [("gah",pactValueToRowData $ PLiteral (LDecimal 123.454345))]
  _writeRow pdb Insert usert "key1" row v
  assertEquals' "user insert" (Just row) (_readRow pdb usert "key1" v)
  let row' = RowData RDV1 $ ObjectMap $ fmap pactValueToRowData $ M.fromList [("gah",toPV False),("fh",toPV (1 :: Int))]
  _writeRow pdb Update usert "key1" row' v
  assertEquals' "user update" (Just row') (_readRow pdb usert "key1" v)
  let ks = mkKeySet [PublicKeyText "skdjhfskj"] "predfun"
  _writeRow pdb Write KeySets "ks1" ks v
  assertEquals' "keyset write" (Just ks) $ _readRow pdb KeySets "ks1" v
  (modName,modRef,mod') <- loadModule
  _writeRow pdb Write Modules modName mod' v
  assertEquals' "module write" (Just mod') $ _readRow pdb Modules modName v
  assertEquals "module native repopulation" (Right modRef) $
    traverse (traverse (fromPersistDirect nativeLookup)) mod'
  assertEquals' "result of commit 3"
    ( TxLogJson . J.encodeJsonText <$>

      [ TxLog { _txDomain = "SYS_keysets"
              , _txKey = "ks1"
              , _txValue = toLegacyJsonViaEncode ks
              }
      , TxLog { _txDomain = "SYS_modules"
              , _txKey = asString modName
              , _txValue = toLegacyJsonViaEncode mod'
              }
      , TxLog { _txDomain = "USER_user1"
              , _txKey = "key1"
              , _txValue = toLegacyJsonViaEncode row
              }
      , TxLog { _txDomain = "USER_user1"
              , _txKey = "key1"
              , _txValue = toLegacyJsonViaEncode row'
              }
      ]
    )
    (_commitTx pdb v)
  void $ _beginTx pdb Transactional v
  tids <- _txids pdb user1 t1 v
  assertEquals "user txids" [1] tids
  assertEquals' "user txlogs"
    [TxLog "USER_user1" "key1" row,
     TxLog "USER_user1" "key1" row'] $
    _getTxLog pdb usert (head tids) v
  _writeRow pdb Insert usert "key2" row v
  assertEquals' "user insert key2 pre-rollback" (Just row) (_readRow pdb usert "key2" v)
  assertEquals' "keys pre-rollback" ["key1","key2"] $ _keys pdb (UserTables user1) v
  _rollbackTx pdb v
  assertEquals' "rollback erases key2" Nothing $ _readRow pdb usert "key2" v
  assertEquals' "keys" ["key1"] $ _keys pdb (UserTables user1) v
  -- Reversed just to ensure inserts are not in order.
  for_ (reverse [2::Int .. 9]) $ \k ->
    _writeRow pdb Insert usert (RowKey $ "key" <> (pack $ show k)) row' v
  assertEquals' "keys" [RowKey ("key" <> (pack $ show k)) | k <- [1 :: Int .. 9]] $ _keys pdb (UserTables user1) v

toTerm' :: ToTerm a => a -> Term Name
toTerm' = toTerm

throwFail :: String -> IO a
throwFail = throwIO . userError

assertEquals :: (Eq a,Show a,NFData a) => String -> a -> a -> IO ()
assertEquals msg a b | [a,b] `deepseq` a == b = return ()
                     | otherwise =
                         throwFail $ "FAILURE: " ++ msg ++ ": expected \n  " ++ show a ++ "\n got \n  " ++ show b

assertEquals' :: (Eq a, Show a, NFData a) => String -> a -> IO a -> IO ()
assertEquals' msg a b = assertEquals msg a =<< b

regressPure :: Loggers -> IO (MVar (DbEnv PureDb))
regressPure l = do
  v <- newMVar $ initDbEnv l persister initPureDb
  createSchema v
  runRegression pactdb v
  return v


_regress :: IO ()
_regress = void $ regressPure alwaysLog

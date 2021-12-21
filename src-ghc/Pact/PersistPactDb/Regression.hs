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
import Data.Default (def)
import Pact.Types.PactValue
import Pact.Repl
import Pact.Repl.Types
import Pact.Native (nativeDefs)
import Pact.Types.RowData


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
nativeLookup (NativeDefName n) = case HM.lookup (Name $ BareName n def) nativeDefs of
  Just (Direct t) -> Just t
  _ -> Nothing

runRegression :: DbEnv p -> IO (MVar (DbEnv p))
runRegression p = do
  v <- newMVar p
  createSchema v
  (Just t1) <- begin v
  let user1 = "user1"
      usert = UserTables user1
      toPV :: ToTerm a => a -> PactValue
      toPV = toPactValueLenient . toTerm'
  createUserTable' v user1 "free.some-Module"
  assertEquals' "output of commit 2"
    [TxLog "SYS_usertables" "user1" $
     object [ ("utModule" .= object [ ("name" .= String "some-Module"), ("namespace" .= String "free")])
            ]
     ]
    (commit v)
  void $ begin v
  assertEquals' "user table info correct" "free.some-Module" $ _getUserTableInfo pactdb user1 v
  let row = RowData RDV0 $ ObjectMap $ M.fromList [("gah",pactValueToRowData $ PLiteral (LDecimal 123.454345))]
  _writeRow pactdb Insert usert "key1" row v
  assertEquals' "user insert" (Just row) (_readRow pactdb usert "key1" v)
  let row' = RowData RDV1 $ ObjectMap $ fmap pactValueToRowData $ M.fromList [("gah",toPV False),("fh",toPV (1 :: Int))]
  _writeRow pactdb Update usert "key1" row' v
  assertEquals' "user update" (Just row') (_readRow pactdb usert "key1" v)
  let ks = mkKeySet [PublicKey "skdjhfskj"] "predfun"
  _writeRow pactdb Write KeySets "ks1" ks v
  assertEquals' "keyset write" (Just ks) $ _readRow pactdb KeySets "ks1" v
  (modName,modRef,mod') <- loadModule
  _writeRow pactdb Write Modules modName mod' v
  assertEquals' "module write" (Just mod') $ _readRow pactdb Modules modName v
  assertEquals "module native repopulation" (Right modRef) $
    traverse (traverse (fromPersistDirect nativeLookup)) mod'
  assertEquals' "result of commit 3"

    [ TxLog { _txDomain = "SYS_keysets"
            , _txKey = "ks1"
            , _txValue = toJSON ks
            }
    , TxLog { _txDomain = "SYS_modules"
            , _txKey = asString modName
            , _txValue = toJSON mod'
            }
    , TxLog { _txDomain = "USER_user1"
            , _txKey = "key1"
            , _txValue = toJSON row
            }
    , TxLog { _txDomain = "USER_user1"
            , _txKey = "key1"
            , _txValue = toJSON row'
            }
    ]
    (commit v)
  void $ begin v
  tids <- _txids pactdb user1 t1 v
  assertEquals "user txids" [1] tids
  assertEquals' "user txlogs"
    [TxLog "USER_user1" "key1" row,
     TxLog "USER_user1" "key1" row'] $
    _getTxLog pactdb usert (head tids) v
  _writeRow pactdb Insert usert "key2" row v
  assertEquals' "user insert key2 pre-rollback" (Just row) (_readRow pactdb usert "key2" v)
  assertEquals' "keys pre-rollback" ["key1","key2"] $ _keys pactdb (UserTables user1) v
  _rollbackTx pactdb v
  assertEquals' "rollback erases key2" Nothing $ _readRow pactdb usert "key2" v
  assertEquals' "keys" ["key1"] $ _keys pactdb (UserTables user1) v
  -- Reversed just to ensure inserts are not in order.
  for_ (reverse [2::Int .. 9]) $ \k ->
    _writeRow pactdb Insert usert (RowKey $ "key" <> (pack $ show k)) row' v
  assertEquals' "keys" [RowKey ("key" <> (pack $ show k)) | k <- [1 :: Int .. 9]] $ _keys pactdb (UserTables user1) v
  return v

toTerm' :: ToTerm a => a -> Term Name
toTerm' = toTerm

begin :: MVar (DbEnv p) -> IO (Maybe TxId)
begin v = _beginTx pactdb Transactional v

commit :: MVar (DbEnv p) -> IO [TxLog Value]
commit v = _commitTx pactdb v

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
  let e = initDbEnv l persister initPureDb
  runRegression e


_regress :: IO ()
_regress = void $ regressPure alwaysLog

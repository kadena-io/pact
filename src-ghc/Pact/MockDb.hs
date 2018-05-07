{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.MockDb where

import Pact.Types.Runtime
import Data.Aeson
import Data.String
import Data.Default
import Pact.Interpreter

rc :: a -> Method e a
rc = const . return

newtype MockRead =
  MockRead (forall k v . (IsString k,FromJSON v) =>
             Domain k v -> k -> Method () (Maybe v))
instance Default MockRead where def = MockRead (\_t _k -> rc Nothing)

newtype MockKeys =
  MockKeys (TableName -> Method () [RowKey])
instance Default MockKeys where def = MockKeys (\_t -> rc [])

newtype MockTxIds =
  MockTxIds (TableName -> TxId -> Method () [TxId])
instance Default MockTxIds where def = MockTxIds (\_t _i -> rc [])

newtype MockGetUserTableInfo =
  MockGetUserTableInfo (TableName -> Method () (ModuleName,KeySetName))
instance Default MockGetUserTableInfo where def = MockGetUserTableInfo (\_t -> rc ("",""))

newtype MockCommitTx =
  MockCommitTx (Method () [TxLog Value])
instance Default MockCommitTx where def = MockCommitTx (rc [])

newtype MockGetTxLog =
  MockGetTxLog (forall k v . (IsString k,FromJSON v) =>
                 Domain k v -> TxId -> Method () [TxLog v])
instance Default MockGetTxLog where def = MockGetTxLog (\_t _i -> rc [])

data MockDb = MockDb {
  mockRead :: MockRead,
  mockKeys :: MockKeys,
  mockTxIds :: MockTxIds,
  mockGetUserTableInfo :: MockGetUserTableInfo,
  mockCommitTx :: MockCommitTx,
  mockGetTxLog :: MockGetTxLog
  }
instance Default MockDb where def = MockDb def def def def def def

pactdb :: MockDb -> PactDb ()
pactdb (MockDb (MockRead r) (MockKeys ks) (MockTxIds tids) (MockGetUserTableInfo uti)
        (MockCommitTx c) (MockGetTxLog gt)) = PactDb {
  _readRow = r
  ,
  _writeRow = \_wt _t _k _v -> rc ()
  ,
  _keys = ks
  ,
  _txids = tids
  ,
  _createUserTable = \_t _m _k -> rc ()
  ,
  _getUserTableInfo = uti
  ,
  _beginTx = \_t -> rc ()
  ,
  _commitTx = c
  ,
  _rollbackTx = rc ()
  ,
  _getTxLog = gt

    }


mkMockEnv :: MockDb -> IO (PactDbEnv ())
mkMockEnv m = mkPactDbEnv (pactdb m) ()

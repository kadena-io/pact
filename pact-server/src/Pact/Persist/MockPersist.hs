{-# LANGUAGE RankNTypes #-}
module Pact.Persist.MockPersist where

import Pact.Persist
import Data.Default
import Pact.PersistPactDb
import Pact.Interpreter
import Pact.Types.Logger

rcp :: a -> Persist e a
rcp a s = return (s,a)

newtype MockQueryKeys =
  MockQueryKeys (forall k . PactKey k => Table k -> Maybe (KeyQuery k) -> Persist () [k])
instance Default MockQueryKeys where def = MockQueryKeys (\_t _q -> rcp [])

newtype MockQuery =
  MockQuery (forall k v . (PactKey k, PactValue v) => Table k -> Maybe (KeyQuery k) -> Persist () [(k,v)])
instance Default MockQuery where def = MockQuery (\_t _q -> rcp [])

newtype MockReadValue =
  MockReadValue (forall k v . (PactKey k, PactValue v) => Table k -> k -> Persist () (Maybe v))
instance Default MockReadValue where def = MockReadValue (\_t _k -> rcp Nothing)

data MockPersist = MockPersist {
   mockQueryKeys :: MockQueryKeys
  ,mockQuery :: MockQuery
  ,mockReadValue :: MockReadValue
  }
instance Default MockPersist where def = MockPersist def def def

persister :: MockPersist -> Persister ()
persister (MockPersist (MockQueryKeys qk) (MockQuery q) (MockReadValue rv)) = Persister {
  createTable = \_t -> rcp ()
  ,
  beginTx = \_t -> rcp ()
  ,
  commitTx = rcp ()
  ,
  rollbackTx = rcp ()
  ,
  queryKeys = qk
  ,
  query = q
  ,
  readValue = rv
  ,
  writeValue = \_t _wt _k _v -> rcp ()
  ,
  refreshConn = rcp ()
  }


mkMockPersistEnv :: Loggers -> MockPersist -> IO (PactDbEnv (DbEnv ()))
mkMockPersistEnv l mp = mkPactDbEnv pactdb (initDbEnv l (persister mp) ())

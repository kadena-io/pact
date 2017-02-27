{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Pact.Persist
  (Persist,
   Table(..),
   KeyCmp(..),
   cmpToOp,
   KeyQuery(..),
   Persister(..),
   WriteType(..),throwDbError,
   Text
   ) where

import Data.Aeson
import Data.Text (Text)
import Data.String

import Pact.Types.Runtime (WriteType(..),throwDbError)

type Persist s a = s -> IO (s,a)

data Table k where
  DataTable :: Text -> Table Text
  TxTable :: Text -> Table Int

deriving instance Show (Table k)
deriving instance Eq (Table k)
deriving instance Ord (Table k)

data KeyCmp = KGT|KGTE|KEQ|KLT|KLTE deriving (Eq,Show,Ord,Enum)

-- | SQL equivalents for 'KeyCmp'
cmpToOp :: IsString s => KeyCmp -> s
cmpToOp kc = fromString $ case kc of
  KGT -> ">"
  KGTE -> ">="
  KEQ -> "="
  KLT -> "<"
  KLTE -> "<="

data KeyQuery k =
  KQAll |
  KQKey { _kqKey :: k, _kqCmp :: KeyCmp }

data Persister s = Persister {
  createTable :: forall k . Table k -> Persist s ()
  ,
  beginTx :: Persist s ()
  ,
  commitTx :: Persist s ()
  ,
  rollbackTx :: Persist s ()
  ,
  getKeys :: forall k . Table k -> KeyQuery k -> Persist s [k]
  ,
  readData :: forall k v . FromJSON v => Table k -> k -> Persist s (Maybe v)
  ,
  writeData :: forall k v . ToJSON v => Table k -> WriteType -> k -> v -> Persist s ()
  }

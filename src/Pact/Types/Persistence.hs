{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Pact.Types.Persistence
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Persistence and back-end DB types.
--
module Pact.Types.Persistence
  (
   ColumnId(..),
   RowKey(..),
   Columns(..),columns,
   Domain(..),
   TxLog(..),txDomain,txKey,txValue,
   WriteType(..),
   Method,
   PactDb(..),
   TxId(..)
   ) where

import Control.Concurrent.MVar (MVar)
import Control.DeepSeq (NFData)
import Control.Lens (makeLenses)
import Control.Monad (forM)
import Data.Aeson hiding (Object)
import Data.Default (Default)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Pact.Types.Lang
import Pact.Types.Pretty
import Pact.Types.Util (AsString(..))




-- | Row key type for user tables.
newtype RowKey = RowKey Text
    deriving (Eq,Ord,IsString,ToTerm,AsString,Show,Pretty)

-- | Column key type.
newtype ColumnId = ColumnId Text
    deriving (Eq,Ord,IsString,ToTerm,AsString,ToJSON,FromJSON,Default,Show,Pretty)

-- | User table row-value type, mapping column ids to values.
newtype Columns v = Columns { _columns :: M.Map ColumnId v }
    deriving (Eq,Show,Generic,Functor,Foldable,Traversable)
instance (ToJSON v) => ToJSON (Columns v) where
    toJSON (Columns m) = object . map (\(k,v) -> asString k .= toJSON v) . M.toList $ m
    {-# INLINE toJSON #-}
instance (FromJSON v) => FromJSON (Columns v) where
    parseJSON = withObject "Columns" $ \o ->
                Columns . M.fromList <$>
                 forM (HM.toList o)
                  (\(k,v) -> ((,) <$> pure (ColumnId k) <*> parseJSON v))
    {-# INLINE parseJSON #-}

instance Pretty v => Pretty (Columns v) where
  pretty (Columns cols) = commaBraces
    $ fmap (\(k, v) -> pretty k <> ": " <> pretty v)
    $ M.toList cols

makeLenses ''Columns

-- | Specify key and value types for database domains.
data Domain k v where
  UserTables :: !TableName -> Domain RowKey (Columns (Term Name))
  KeySets :: Domain KeySetName KeySet
  Modules :: Domain ModuleName (ModuleDef Name)
  Namespaces :: Domain NamespaceName Namespace
deriving instance Eq (Domain k v)
deriving instance Show (Domain k v)
instance AsString (Domain k v) where
    asString (UserTables t) = asString t
    asString KeySets    = "SYS:KeySets"
    asString Modules    = "SYS:Modules"
    asString Namespaces = "SYS:Namespaces"

-- | Transaction record.
-- Backends are expected to return "user-visible" values
-- for '_txValue', namely that internal JSON formats for 'Persistable'
-- need to be converted to Term JSON formats.
data TxLog v =
    TxLog {
      _txDomain :: !Text
    , _txKey :: !Text
    , _txValue :: !v
    } deriving (Eq,Show,Typeable,Generic,Foldable,Functor,Traversable)
makeLenses ''TxLog
instance Hashable v => Hashable (TxLog v)

instance ToJSON v => ToJSON (TxLog v) where
    toJSON (TxLog d k v) =
        object ["table" .= d, "key" .= k, "value" .= v]
instance FromJSON v => FromJSON (TxLog v) where
    parseJSON = withObject "TxLog" $ \o ->
                TxLog <$> o .: "table" <*> o .: "key" <*> o .: "value"

instance Pretty v => Pretty (TxLog v) where
  pretty (TxLog domain key value) = commaBrackets
    [ "table: " <> pretty domain
    , "key: "   <> pretty key
    , "value: " <> pretty value
    ]

-- | Instruction for '_writeRow'.
data WriteType =
  -- | Insert a new row, fail if key already found.
  --   Requires complete row value, enforced by pact runtime.
  Insert |
  -- | Update an existing row, fail if key not found.
  --   Allows incomplete row values.
  Update |
  -- | Update an existing row, or insert a new row if not found.
  --   Requires complete row value, enforced by pact runtime.
  Write
  deriving (Eq,Ord,Show,Enum,Bounded)



-- | Transaction ids are non-negative 64-bit integers and
--   are expected to be monotonically increasing.
newtype TxId = TxId Word64
    deriving (Eq,Ord,Enum,Num,Real,Integral,Bounded,Default,FromJSON,ToJSON,Generic)

instance NFData TxId
instance Show TxId where
  show (TxId s) = show s
instance Pretty TxId where
  pretty (TxId s) = viaShow s
instance ToTerm TxId where toTerm = tLit . LInteger . fromIntegral
instance AsString TxId where asString = pack . show




-- | Shape of back-end methods: use MVar for state, run in IO.
type Method e a = MVar e -> IO a

-- | Fun-record type for Pact back-ends.
data PactDb e = PactDb {
    -- | Read a domain value at key, throwing an exception if not found.
    _readRow :: forall k v . (IsString k,FromJSON v) =>
                Domain k v -> k -> Method e (Maybe v)
    -- | Write a domain value at key. WriteType argument governs key behavior.
  , _writeRow :: forall k v . (AsString k,ToJSON v) =>
                 WriteType -> Domain k v -> k -> v -> Method e ()
    -- | Retrieve all keys for user table.
  , _keys ::  TableName -> Method e [RowKey]
    -- | Retrieve all transaction ids greater than supplied txid for table.
  , _txids ::  TableName -> TxId -> Method e [TxId]
    -- | Create a user table.
  , _createUserTable ::  TableName -> ModuleName -> Method e ()
    -- | Get module, keyset for user table.
  , _getUserTableInfo ::  TableName -> Method e ModuleName
    -- | Initiate transaction. If TxId not provided, commit fails/rolls back.
  , _beginTx :: Maybe TxId -> Method e ()
    -- | Commit transaction, if in tx. If not in tx, rollback and throw error.
    -- Return raw TxLogs, for use in checkpointing only (not for transmission to user).
  , _commitTx ::  Method e [TxLog Value]
    -- | Rollback database transaction.
  , _rollbackTx :: Method e ()
    -- | Get transaction log for table. TxLogs are expected to be user-visible format.
  , _getTxLog :: forall k v . (IsString k,FromJSON v) =>
                 Domain k v -> TxId -> Method e [TxLog v]
}

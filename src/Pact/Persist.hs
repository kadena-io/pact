{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pact.Persist
  (Persist,
   Table(..),DataTable,TxTable,
   TableId(..),tableId,
   PactDbKey,PactDbValue(..),
   DataKey(..),TxKey(..),
   KeyCmp(..),cmpToOp,
   KeyConj(..),conjToOp,
   KeyQuery(..),kAnd,kOr,compileQuery,
   Persister(..),
   WriteType(..),throwDbError,
   Text
   ) where

import Data.Aeson
import Data.String
import Data.Hashable
import Data.Text (Text)
import Data.Typeable

import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.RowData
import Pact.Types.Runtime

type Persist s a = s -> IO (s,a)

newtype DataKey = DataKey Text
  deriving (Eq,Ord,IsString,AsString,Hashable,Pretty)
instance Show DataKey where show (DataKey k) = show k
newtype TxKey = TxKey Integer
  deriving (Eq,Ord,Num,Enum,Real,Integral,Hashable,Pretty)
instance Show TxKey where show (TxKey k) = show k

type DataTable = Table DataKey
type TxTable = Table TxKey

newtype TableId = TableId Text
  deriving (Eq,Show,Ord,IsString,AsString,Hashable, Pretty)

data Table k where
  DataTable :: !TableId -> DataTable
  TxTable :: !TableId -> TxTable

tableId :: Table k -> TableId
tableId (DataTable t) = t
tableId (TxTable t) = t
{-# INLINE tableId #-}

deriving instance Show (Table k)
deriving instance Eq (Table k)
deriving instance Ord (Table k)
instance Hashable k => Hashable (Table k) where
  hashWithSalt s (DataTable t) = s `hashWithSalt` (0::Int) `hashWithSalt` t
  hashWithSalt s (TxTable t) = s `hashWithSalt` (1::Int) `hashWithSalt` t

instance Pretty (Table k) where
  pretty (DataTable tid) = pretty tid
  pretty (TxTable tid) = pretty tid

data KeyCmp = KGT|KGTE|KEQ|KNEQ|KLT|KLTE deriving (Eq,Show,Ord,Enum)
data KeyConj = AND|OR deriving (Eq,Show,Ord,Enum)

-- key query stuff
-- a key value pairs with an operator: >= k, < k, etc.
-- connectors join operators: (>= k1 -AND- < k2) -OR- /= k3

-- | SQL equivalents for 'KeyCmp'
cmpToOp :: IsString s => KeyCmp -> s
cmpToOp kc = fromString $ case kc of
  KGT -> ">"
  KGTE -> ">="
  KEQ -> "="
  KNEQ -> "!="
  KLT -> "<"
  KLTE -> "<="
{-# INLINE cmpToOp #-}

-- | SQL equivalents for 'KeyConj'
conjToOp :: IsString s => KeyConj -> s
conjToOp c = fromString $ case c of
  AND -> "AND"
  OR -> "OR"
{-# INLINE conjToOp #-}

data KeyQuery k =
  KQKey { kqCmp :: KeyCmp, kqKey :: k } |
  KQConj { kqL :: KeyQuery k, kqConj :: KeyConj, kqR :: KeyQuery k }
  deriving (Eq,Show)

-- | Convenience for infix usage.
kAnd :: KeyQuery k -> KeyQuery k -> KeyQuery k
kAnd l r = KQConj l AND r
-- | Convenience for infix usage.
kOr :: KeyQuery k -> KeyQuery k -> KeyQuery k
kOr l r = KQConj l OR r

-- | Compile an optional 'KeyQuery' to standard SQL statement syntax with param values list.
compileQuery :: (IsString s, Monoid s) => s -> Maybe (KeyQuery k) -> (s,[k])
compileQuery _ Nothing = ("",[])
compileQuery keyfield (Just kq) = ("WHERE " <> qs,pms)
  where (qs,pms) = compile True kq
        compile _ (KQKey q k) = (keyfield <> " " <> cmpToOp q <> " ?",[k])
        compile top (KQConj l o r) = (op <> lq <> " " <> conjToOp o <> " " <> rq <> cp,lps ++ rps)
          where (op,cp) | top = ("","")
                        | otherwise = ("(",")")
                (lq,lps) = compile False l
                (rq,rps) = compile False r
{-# INLINE compileQuery #-}


class (Ord k,Show k,Eq k,Hashable k,Pretty k) => PactDbKey k
instance PactDbKey TxKey
instance PactDbKey DataKey

class (Eq v,Show v,ToJSON v,FromJSON v,Typeable v) => PactDbValue v where
  prettyPactDbValue :: v -> Doc

instance PactDbValue v => PactDbValue (TxLog v) where
  prettyPactDbValue = pretty . fmap (SomeDoc . prettyPactDbValue)
instance PactDbValue RowData where
  prettyPactDbValue = pretty
instance PactDbValue a => PactDbValue [a] where
  prettyPactDbValue = prettyList . fmap (SomeDoc . prettyPactDbValue)
instance PactDbValue PersistModuleData where
  prettyPactDbValue = (pretty . _mdModule)
instance PactDbValue KeySet where
  prettyPactDbValue = pretty
instance PactDbValue Value where
  prettyPactDbValue = pretty
instance PactDbValue (Namespace PactValue) where
  prettyPactDbValue = pretty
instance PactDbValue (Maybe PactExec) where
  prettyPactDbValue = pretty

data Persister s = Persister {
  createTable :: forall k . PactDbKey k => Table k -> Persist s ()
  ,
  beginTx :: ExecutionMode -> Persist s ()
  ,
  commitTx :: Persist s ()
  ,
  rollbackTx :: Persist s ()
  ,
  queryKeys :: forall k . PactDbKey k => Table k -> Maybe (KeyQuery k) -> Persist s [k]
  ,
  query :: forall k v . (PactDbKey k, PactDbValue v) => Table k -> Maybe (KeyQuery k) -> Persist s [(k,v)]
  ,
  readValue :: forall k v . (PactDbKey k, PactDbValue v) => Table k -> k -> Persist s (Maybe v)
  ,
  writeValue :: forall k v . (PactDbKey k, PactDbValue v) => Table k -> WriteType -> k -> v -> Persist s ()
  ,
  refreshConn :: Persist s ()
  }

_compileQry1 :: (String,[Int])
_compileQry1 = compileQuery "key" (Just (KQKey KGT 2 `kAnd` KQKey KLT 4 `kOr` KQKey KNEQ 10))

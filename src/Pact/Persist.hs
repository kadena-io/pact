{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Persist
  (Persist,
   Table(..),
   KeyCmp(..),
   cmpToOp,conjToOp,
   KeyQuery(..),kAnd,kOr,compileQuery,
   Persister(..),
   WriteType(..),throwDbError,
   Text
   ) where

import Data.Aeson
import Data.Text (Text)
import Data.String
import Data.Monoid

import Pact.Types.Runtime (WriteType(..),throwDbError)

type Persist s a = s -> IO (s,a)

data Table k where
  DataTable :: Text -> Table Text
  TxTable :: Text -> Table Int

deriving instance Show (Table k)
deriving instance Eq (Table k)
deriving instance Ord (Table k)

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

-- | SQL equivalents for 'KeyConj'
conjToOp :: IsString s => KeyConj -> s
conjToOp c = fromString $ case c of
  AND -> "AND"
  OR -> "OR"

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

data Persister s = Persister {
  createTable :: forall k . Table k -> Persist s ()
  ,
  beginTx :: Persist s ()
  ,
  commitTx :: Persist s ()
  ,
  rollbackTx :: Persist s ()
  ,
  queryKeys :: forall k . Table k -> Maybe (KeyQuery k) -> Persist s [k]
  ,
  query :: forall k v . FromJSON v => Table k -> Maybe (KeyQuery k) -> Persist s [(k,v)]
  ,
  readValue :: forall k v . FromJSON v => Table k -> k -> Persist s (Maybe v)
  ,
  writeValue :: forall k v . ToJSON v => Table k -> WriteType -> k -> v -> Persist s ()
  }

_compileQry1 :: (String,[Int])
_compileQry1 = compileQuery "key" (Just (KQKey KGT 2 `kAnd` KQKey KLT 4 `kOr` KQKey KNEQ 10))

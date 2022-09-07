{-# LANGUAGE DeriveTraversable #-}


module Pact.Core.Guards
( PublicKey(..)
, KeySetName(..)
, Governance(..)
, KeySet(..)
, Guard(..)
, UserGuard(..)
, KSPredicate(..)
)
where

import Data.Text(Text)
import Data.ByteString(ByteString)
import qualified Data.Set as S

import Pact.Core.Names

newtype PublicKey = PublicKey { _pubKey :: ByteString }
  deriving (Eq,Ord,Show)

newtype KeySetName = KeySetName Text
    deriving (Eq,Ord,Show)

newtype Governance a =
  Governance { _governance :: Either KeySetName a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data KSPredicate name
  = KeysAll
  | Keys2
  | KeysAny
  -- | CustomPredicate name
  deriving (Eq, Show, Ord)

data KeySet name
  = KeySet
  { _ksKeys :: !(S.Set PublicKey)
  , _ksPredFun :: KSPredicate name
  } deriving (Eq, Show, Ord)

data UserGuard name term
  = UserGuard
  { _ugFunction :: name
  , _ugArgs :: [term] }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Guard name term
  = GKeyset (KeySet name)
  | GKeySetRef KeySetName
  | GUserGuard term
  -- | GUserGuard (UserGuard name term)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Namespace name term
  = Namespace
  { _nsName :: !NamespaceName
  , _nsUser :: !(Guard name term)
  , _nsAdmin :: !(Guard name term)
  } deriving (Eq, Show)

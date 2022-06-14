{-# LANGUAGE DeriveTraversable #-}


module Pact.Core.Guards
( PublicKey(..)
, KeySetName(..)
, Guard(..)
, Governance(..)
)
where

import Data.Text(Text)
import Data.ByteString(ByteString)
import qualified Data.Set as S

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
  | CustomPredicate name
  deriving (Eq, Show, Ord)

data KeySet name
  = KeySet
  { _ksKeys :: !(S.Set PublicKey)
  , _ksPredFun :: KSPredicate name
  } deriving (Eq, Show, Ord)

data UserGuard name v
  = UserGuard
  { _ugFun :: name
  , _ugArgs :: ![v]
  } deriving (Eq, Show)

data ModuleGuard name
  = ModuleGuard
  { _mgModuleName :: name
  , _mgName :: !Text
  } deriving (Eq, Show)


data Guard name v
  = GKeyset (KeySet name)
  | GKeySetRef KeySetName
  | GUserGuard (UserGuard name v)
  | GModuleGuard (ModuleGuard name)
  deriving (Eq, Show)

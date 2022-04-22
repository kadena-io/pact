module Pact.Core.Guards
( PublicKey(..)
, KeySetName(..)
, Guard(..)
)
where

import Data.Text(Text)
import Pact.Types.KeySet(PublicKey(..), KeySetName(..))
import qualified Data.Set as S

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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      :  Pact.Core.IR.Term
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, Jose Cardona <jose@kadena.io>
--
-- Our Core IR, which supports let-bound terms for type inference
--

module Pact.Core.IR.Term where

import Control.Lens
import Data.Void(Void)
import Data.Map(Map)
import Data.Text(Text)
import Data.Vector (Vector)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set

import Pact.Core.Hash
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Builtin
import Pact.Core.Imports
import Pact.Core.Guards

data Defun name builtin info
  = Defun
  { _dfunName :: Text
  , _dfunType :: Type Void
  , _dfunTerm :: Term name builtin info
  , _dfunInfo :: info
  } deriving Show

data DefConst name builtin info
  = DefConst
  { _dcName :: Text
  , _dcType :: Maybe (Type Void)
  , _dcTerm :: Term name builtin info
  , _dcInfo :: info
  } deriving Show

data CapType
  = ManagedCap Int
  | AutomanagedCap
  | Unmanaged
  deriving Show

data DefCap name builtin info
  = DefCap
  { _dcapName :: Text
  , _dcapArgs :: [Text]
  , _dcapTerm :: Term name builtin info
  , _dcapCapType :: CapType
  , _dcapType :: Type Void
  , _dcapInfo :: info
  } deriving Show

data Def name builtin info
  = Dfun (Defun name builtin info)
  | DConst (DefConst name builtin info)
  deriving Show

defName :: Def name b i -> Text
defName (Dfun d) = _dfunName d
defName (DConst d) = _dcName d

-- Todo:
-- Support module guard
data Module name builtin info
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance name
  , _mDefs :: [Def name builtin info]
  , _mBlessed :: !(Set.Set ModuleHash)
  , _mImports :: [Import]
  , _mImplements :: [ModuleName]
  , _mHash :: ModuleHash
  } deriving Show

data Interface name builtin info
  = Interface
  { _ifName :: name
  , _ifDefns :: [IfDef name builtin info]
  , _ifHash :: Hash
  } deriving Show

data IfDefun name info
  = IfDefun
  { _ifdName :: name
  , _ifdType :: Type Void
  , _ifdInfo :: info
  } deriving Show

data IfDef name builtin info
  = IfDfun (IfDefun name info)
  | IfDConst (DefConst name builtin info)
  deriving Show

data TopLevel name builtin info
  = TLModule (Module name builtin info)
  | TLInterface (Interface name builtin info)
  | TLTerm (Term name builtin info)
  deriving Show

data ReplTopLevel name builtin info
  = RTLModule (Module name builtin info)
  | RTLInterface (Interface name builtin info)
  | RTLDefConst (DefConst name builtin info)
  | RTLDefun (Defun name builtin info)
  | RTLTerm (Term name builtin info)
  deriving Show


-- | Core IR
data Term name builtin info
  = Var name info
  -- ^ single variables e.g x
  | Lam (NonEmpty (name, Maybe (Type Void))) (Term name builtin info) info
  -- ^ $f = \x.e
  -- Lambdas are named for the sake of the callstack.
  | Let name (Maybe (Type Void)) (Term name builtin info) (Term name builtin info) info
  -- ^ let x = e1 in e2
  | App (Term name builtin info) (NonEmpty (Term name builtin info)) info
  -- ^ (e1 e2)
  | Block (NonEmpty (Term name builtin info)) info
  -- ^ error term , error "blah"
  | Builtin builtin info
  -- ^ Built-in ops, e.g (+)
  | DynAccess name name info
  -- ^ For some module m, m::f
  | Constant Literal info
  -- ^ Literals
  | ObjectLit (Map Field (Term name builtin info)) info
  -- ^ Object literals
  | ListLit (Vector (Term name builtin info)) info
  -- List Literals ^
  | ObjectOp (ObjectOp (Term name builtin info)) info
  deriving (Show, Functor)


----------------------------
-- Aliases for convenience
----------------------------

termInfo :: Lens' (Term name builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Let n mty t1 t2 i ->
    Let n mty t1 t2 <$> f i
  Lam ns term i -> Lam ns term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i
  DynAccess n1 n2 i -> DynAccess n1 n2 <$> f i
  ObjectLit m i -> ObjectLit m <$> f i
  ObjectOp o i -> ObjectOp o <$> f i
  Block terms i -> Block terms <$> f i
  ListLit l i  -> ListLit l <$> f i

instance Plated (Term name builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam ns term i -> Lam ns <$> f term <*> pure i
    Let n mty t1 t2 i -> Let n mty <$> f t1 <*> f t2 <*> pure i
    App t1 t2 i -> App <$> f t1 <*> traverse f t2 <*> pure i
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)
    DynAccess n1 n2 i -> pure (DynAccess n1 n2 i)
    ObjectLit m i -> ObjectLit <$> traverse f m <*> pure i
    ObjectOp o i -> ObjectOp <$> traverse f o <*> pure i
    Block terms i -> Block <$> traverse f terms <*> pure i
    ListLit m i -> ListLit <$> traverse f m <*> pure i


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
import Data.Map(Map)
import Data.Text(Text)
import Data.Vector (Vector)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set

import Pact.Types.Hash (Hash)
import Pact.Types.Term (ModuleHash(..), Governance(..))
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Builtin
import Pact.Core.Imports
import qualified Pact.Types.Names as PNames


-- Todo: Not sure if this type is useful at all,
-- since we're trying to share the `DefConst`
-- type in interfaces.
-- might just be useful to provide a lens into
-- `Def` bodies.
data DefType
  = DTDefCap
  | DTDefun
  | DTDefPact
  | DTDefConst
  deriving Show

data Defun name tyname builtin info
  = Defun
  { _dfunName :: name
  , _dfunTerm :: Term name tyname builtin info
  , _dfunTermType :: Maybe (Type tyname)
  } deriving Show

data DefConst name tyname builtin info
  = DefConst
  { _dcName :: name
  , _dcTerm :: Term name tyname builtin info
  , _dcTermType :: Maybe (Type tyname)
  } deriving Show

data DefCap name tyname builtin info
  = DefCap
  { _dcapName :: name
  , _dcapTerm :: Term name tyname builtin info
  , _dcapTermType :: Maybe (Type tyname)
  } deriving Show

-- Todo :: probably need a special form
-- either structurally, or these typecheck different.
data DefPact name tyname builtin info
  = DefPact
  { _dpName :: name
  , _dpTerm :: Term name tyname builtin info
  , _dpTermType :: Maybe (Type tyname)
  } deriving Show

data Def name tyname builtin info
  = Dfun (Defun name tyname builtin info)
  | DConst (DefConst name tyname builtin info)
  | DPact (DefPact name tyname builtin info)
  deriving Show

-- Todo:
-- Support module guard
data Module name tyname builtin info
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance (DefCap name tyname builtin info)
  , _mDefs :: [Def name tyname builtin info]
  , _mBlessed :: !(Set.Set ModuleHash)
  , _mImports :: [Import]
  , _mHash :: ModuleHash
  } deriving Show

data Interface name tyname builtin info
  = Interface
  { _ifName :: name
  , _ifDefns :: [IfDef name tyname builtin info]
  , _ifHash :: Hash
  } deriving Show

data IfDefun name tyname info
  = IfDefun
  { _ifdName :: name
  , _ifdType :: Type tyname
  , _ifdInfo :: info
  } deriving Show

data IfDef name tyname builtin info
  = IfDfun (IfDefun name tyname info)
  | IfDConst (DefConst name tyname builtin info)
  deriving Show

data TopLevel name tyname builtin info
  = TLModule (Module name tyname builtin info)
  | TLInterface (Interface name tyname builtin info)
  | TLTerm (Term name tyname builtin info)
  deriving Show

type TermP = Term PNames.Name Text RawBuiltin
type DefP = Def PNames.Name Text RawBuiltin
type ModuleP = Module PNames.Name Text RawBuiltin
type TopLevelP = TopLevel PNames.Name Text RawBuiltin
type CoreIRProgramP info = [TopLevelP info]

-- | Core IR
data Term name tyname builtin info
  = Var name info
  -- ^ single variables e.g x
  | Lam name (NonEmpty (name, Maybe (Type tyname))) (Term name tyname builtin info) info
  -- ^ $f = \x.e
  -- Lambdas are named for the sake of the callstack.
  | Let name (Maybe (Type tyname)) (Term name tyname builtin info) (Term name tyname builtin info) info
  -- ^ let x = e1 in e2
  | App (Term name tyname builtin info) (NonEmpty (Term name tyname builtin info)) info
  -- ^ (e1 e2)
  | Block (NonEmpty (Term name tyname builtin info)) info
  -- ^ (e1) (e2)
  | Error Text info
  -- ^ error term , error "blah"
  | Builtin builtin info
  -- ^ Built-in ops, e.g (+)
  | DynAccess name name info
  -- ^ For some module m, m::f
  | Constant Literal info
  -- ^ Literals
  | ObjectLit (Map Field (Term name tyname builtin info)) info
  -- ^ Object literals
  | ListLit (Vector (Term name tyname builtin info)) info
  -- List Literals ^
  | ObjectOp (ObjectOp (Term name tyname builtin info)) info
  deriving (Show, Functor)


termInfo :: Lens' (Term name tyname builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Let n mty t1 t2 i ->
    Let n mty t1 t2 <$> f i
  Lam n ns term i -> Lam n ns term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  Error s i -> Error s <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i
  DynAccess n1 n2 i -> DynAccess n1 n2 <$> f i
  ObjectLit m i -> ObjectLit m <$> f i
  ObjectOp o i -> ObjectOp o <$> f i
  Block terms i -> Block terms <$> f i
  ListLit l i  -> ListLit l <$> f i

instance Plated (Term name tyname builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam n ns term i -> Lam n ns <$> f term <*> pure i
    Let n mty t1 t2 i -> Let n mty <$> f t1 <*> f t2 <*> pure i
    App t1 t2 i -> App <$> f t1 <*> traverse f t2 <*> pure i
    Error s i -> pure (Error s i)
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)
    DynAccess n1 n2 i -> pure (DynAccess n1 n2 i)
    ObjectLit m i -> ObjectLit <$> traverse f m <*> pure i
    ObjectOp o i -> ObjectOp <$> traverse f o <*> pure i
    Block terms i -> Block <$> traverse f terms <*> pure i
    ListLit m i -> ListLit <$> traverse f m <*> pure i


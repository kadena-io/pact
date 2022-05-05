{-# LANGUAGE LambdaCase #-}

module Pact.Core.Typed.Term
( Defun(..)
, DefConst(..)
, DefCap(..)
, DefPact(..)
, Def(..)
, Term(..)
, Module(..)
, Interface(..)
, IfDefun(..)
, IfDef(..)
, Literal(..)
, termInfo
)
 where

import Control.Lens
import Data.Map(Map)
import Data.List.NonEmpty(NonEmpty)
import Data.Vector (Vector)
import qualified Data.Set as Set


import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Imports
import Pact.Types.Hash (Hash)
import Pact.Types.Term (ModuleHash(..), Governance(..))
import Pact.Types.Exp (Literal(..))

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

data DefSchema name tyname info
  = DefSchema
  { _dsName :: name
  , _dsType :: RowObject tyname
  } deriving Show

data DefTable name tyname info
  = DefTable
  { _dtName :: name
  , _dtType :: Type tyname
  } deriving Show

data Def name tyname builtin info
  = Dfun (Defun name tyname builtin info)
  | DConst (DefConst name tyname builtin info)
  | DPact (DefPact name tyname builtin info)
  | DSchema (DefSchema name tyname info)
  | DTable (DefTable name tyname info)
  deriving Show

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
  | IFDConst (DefConst name tyname builtin info)
  deriving Show

data TopLevel name tyname builtin info
  = TLModule (Module name tyname builtin info)
  | TLInterface (Interface name tyname builtin info)
  | TLTerm (Term name tyname builtin info)
  deriving Show


-- | Typed pact core terms
data Term name tyname builtin info
  = Var name info
  -- ^ single variables, e.g the term `x`
  | Lam name (NonEmpty name) (NonEmpty (Type tyname)) (Term name tyname builtin info) info
  -- ^ f = \a b c -> e
  -- All lambdas, even anonymous ones, are named, for the sake of them adding a stack frame
  | App (Term name tyname builtin info) (NonEmpty (Term name tyname builtin info)) info
  -- ^ (e_1 e_2 .. e_n)
  | TyApp (Term name tyname builtin info) (Type tyname) info
  -- ^ (e_1 @t)
  | TyAbs tyname (Term name tyname builtin info) info
  -- ^ ΛX.e
  | ObjectLit (Row tyname) (Map Field (Term name tyname builtin info)) info
  -- ^ {f_1:e_1, .., f_n:e_n}
  | ListLit (Type tyname) (Vector (Term name tyname builtin info)) info
  -- ^ [e_1, e_2, .., e_n]
  | Error String (Type tyname) info
  -- ^ error terms + their inferred type
  | Builtin builtin info
  -- ^ Built-in functions (or natives)
  | Constant Literal info
  -- ^ Constant/Literal values
  deriving (Show)

termInfo :: Lens' (Term name tyname builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Lam n ns ty term i -> Lam n ns ty term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  TyApp term ty i -> TyApp term ty <$> f i
  TyAbs ty term i -> TyAbs ty term <$> f i
  ObjectLit row obj i -> ObjectLit row obj <$> f i
  ListLit ty v i -> ListLit ty v <$> f i
  Error s ty i -> Error s ty <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i

instance Plated (Term name tyname builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam n ty term i -> Lam n ty <$> f term <*> pure i
    App t1 t2 i -> App <$> f t1 <*> traverse f t2 <*> pure i
    TyApp term ty i -> TyApp <$> f term <*> pure ty <*> pure i
    TyAbs ty term i -> TyAbs ty <$> f term <*> pure i
    ObjectLit ty tm i ->
      ObjectLit ty <$> traverse f tm <*> pure i
    ListLit ty ts i ->
      ListLit ty <$> traverse f ts <*> pure i
    Error s ty i -> pure (Error s ty i)
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)

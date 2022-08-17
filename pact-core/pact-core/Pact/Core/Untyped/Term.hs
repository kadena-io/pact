{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Pact.Core.Untyped.Term
 ( Defun(..)
 , DefConst(..)
 , Def(..)
 , defType
 , defName
 , defTerm
 , Module(..)
 , Interface(..)
 , IfDefun(..)
 , IfDef(..)
 , TopLevel(..)
 , ReplTopLevel(..)
 , Term(..)
 , EvalTerm
 , EvalModule
 , fromTypedTerm
 , fromTypedDef
 , fromTypedModule
 , fromTypedTopLevel
 , termInfo
 ) where

import Control.Lens
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.List.NonEmpty(NonEmpty)
import Data.Vector (Vector)
import Data.Foldable(foldl')
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Imports
import Pact.Core.Hash
import Pact.Core.Guards
import Pact.Core.Pretty(Pretty(..), pretty, (<+>))

import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.Typed.Term as Typed

data Defun name builtin info
  = Defun
  { _dfunName :: Text
  , _dfunType :: Type NamedDeBruijn
  , _dfunTerm :: Term name builtin info
  , _dfunInfo :: info
  } deriving Show

data DefConst name builtin info
  = DefConst
  { _dcName :: Text
  , _dcType :: Type NamedDeBruijn
  , _dcTerm :: Term name builtin info
  , _dcInfo :: info
  } deriving Show

data Def name builtin info
  = Dfun (Defun name builtin info)
  | DConst (DefConst name builtin info)
   deriving Show

-- DCap (DefCap name builtin info)
-- DPact (DefPact name builtin info)
-- DSchema (DefSchema name info)
-- DTable (DefTable name info)
defType :: Def name builtin info -> Type NamedDeBruijn
defType = \case
  Dfun d -> _dfunType d
  DConst d -> _dcType d

defName :: Def name builtin i -> Text
defName = \case
  Dfun d -> _dfunName d
  DConst d -> _dcName d

defTerm :: Def name builtin info -> Term name builtin info
defTerm = \case
  Dfun d -> _dfunTerm d
  DConst d -> _dcTerm d

data Module name builtin info
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance name
  , _mDefs :: [Def name builtin info]
  , _mBlessed :: !(Set.Set ModuleHash)
  , _mImports :: [Import]
  , _mImplemented :: [ModuleName]
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
  , _ifdType :: Type NamedDeBruijn
  , _ifdInfo :: info
  } deriving Show

data IfDef name builtin info
  = IfDfun (IfDefun name info)
  | IFDConst (DefConst name builtin info)
  deriving Show

data TopLevel name builtin info
  = TLModule (Module name builtin info)
  | TLInterface (Interface name builtin info)
  | TLTerm (Term name builtin info)
  deriving Show

data ReplTopLevel name builtin info
  = RTLModule (Module name builtin info)
  -- | RTLInterface (Interface name builtin info)
  | RTLDefun (Defun name builtin info)
  | RTLDefConst (DefConst name builtin info)
  | RTLTerm (Term name builtin info)
  deriving Show

{-

V0 = Today's Pact
V1 = Theme: simple, modular, more efficient
     No user-observable changes, modulo:
     - error messages
     Able to replay from genesis, modulo:
     - module serialization
     - gas accounting
     All V0 transactions must succeed V1, unless there is a compelling
     rationale.
V2 = Types
V3 = Higher-kinded functions?
V4 = Macros?

Compilation pipeline:

    Untyped source code
==> Syntactic term, bijective with source
==> Desugared
==> Renamed
==> Overload resolution
==> Type-checked term, now with inferred types
-}

-- | Untyped pact core terms
data Term name builtin info
  = Var name info
  -- ^ single variables, e.g the term `x`
  | Lam (Term name builtin info) info
  -- ^ f = \a b c -> e
  -- All lambdas, even anonymous ones, are named, for the sake of them adding a stack frame
  | App (Term name builtin info) (Term name builtin info) info
  -- ^ Constant/Literal values
  | Block (NonEmpty (Term name builtin info)) info
  -- ^ (e_1 e_2 .. e_n)
  | Builtin builtin info
  -- ^ Built-in functions (or natives)
  | Constant Literal info
  -- ^ ΛX.e
  | ObjectLit (Map Field (Term name builtin info)) info
  -- ^ {f_1:e_1, .., f_n:e_n}
  | ListLit (Vector (Term name builtin info)) info
  -- ^ [e_1, e_2, .., e_n]
  | ObjectOp (ObjectOp (Term name builtin info)) info
  -- Object access, update and remove
  deriving (Show, Functor)

-- Post Typecheck terms + modules
type EvalTerm b i = Term Name b i
type EvalModule b i = Module Name b i

fromTypedTerm :: Typed.Term Name tyname b i -> Term Name b i
fromTypedTerm = \case
  Typed.Var n i -> Var n i
  Typed.Lam args body i ->
    foldr (\_ t -> Lam t i) (fromTypedTerm body) args
  Typed.App fn apps i ->
    foldl' (\f arg -> App f (fromTypedTerm arg) i) (fromTypedTerm fn) apps
  Typed.Let _ e1 e2 i ->
    App (Lam (fromTypedTerm e1) i) (fromTypedTerm e2) i
  Typed.Builtin b i ->
    Builtin b i
  Typed.Constant lit i -> Constant lit i
  Typed.TyApp te _ _ -> fromTypedTerm te
  Typed.TyAbs _ te _ -> fromTypedTerm te
  Typed.Block nel i ->
    Block (fromTypedTerm <$> nel) i
  Typed.ObjectLit m i ->
    ObjectLit (fromTypedTerm <$> m) i
  Typed.ListLit _ vec i ->
    ListLit (fromTypedTerm <$> vec) i
  Typed.ObjectOp oo i ->
    ObjectOp (fromTypedTerm <$> oo) i

fromTypedDefun
  :: Typed.Defun Name NamedDeBruijn builtin info
  -> Defun Name builtin info
fromTypedDefun (Typed.Defun n ty term i) =
  Defun n ty (fromTypedTerm term) i

fromTypedDConst
  :: Typed.DefConst Name NamedDeBruijn builtin info
  -> DefConst Name builtin info
fromTypedDConst (Typed.DefConst n ty term i) =
  DefConst n ty (fromTypedTerm term) i

fromTypedDef
  :: Typed.Def Name NamedDeBruijn builtin info
  -> Def Name builtin info
fromTypedDef = \case
  Typed.Dfun d -> Dfun (fromTypedDefun d)
  Typed.DConst d -> DConst (fromTypedDConst d)

fromTypedModule
  :: Typed.Module Name NamedDeBruijn builtin info
  -> Module Name builtin info
fromTypedModule (Typed.Module mn mgov defs blessed imports implements hs) =
  Module mn mgov (fromTypedDef <$> defs) blessed imports implements hs

fromTypedTopLevel :: Typed.TopLevel Name NamedDeBruijn builtin info -> TopLevel Name builtin info
fromTypedTopLevel = \case
  Typed.TLModule m -> TLModule (fromTypedModule m)
  Typed.TLInterface _ -> error "todo: implement interfaces"
  Typed.TLTerm e -> TLTerm (fromTypedTerm e)

instance (Pretty name, Pretty builtin) => Pretty (Term name builtin info) where
  pretty = \case
    Var n _ -> pretty n
    Lam term _ ->
      "λ." <> pretty term
    App t1 t2 _ ->
      Pretty.parens (Pretty.hsep [pretty t1, pretty t2])
    Builtin b _ -> pretty b
    Constant l _ -> pretty l
    Block nel _ -> Pretty.parens ("progn" <> Pretty.hsep (pretty <$> NE.toList nel))
    ObjectLit (Map.toList -> obj) _ ->
      Pretty.braces $
      Pretty.hsep $
      Pretty.punctuate Pretty.comma $
      fmap (\(f, o) -> pretty f <> ":" <+> pretty o) obj
    ListLit (V.toList -> li) _ ->
      Pretty.brackets $
      Pretty.hsep $
      Pretty.punctuate Pretty.comma (pretty <$> li)
    ObjectOp oop _ -> case oop of
      ObjectAccess fi te ->
        Pretty.parens $ Pretty.hsep ["@" <> pretty fi, pretty te]
      ObjectRemove fi te ->
        Pretty.parens $ Pretty.hsep ["#" <> pretty fi, pretty te]
      ObjectExtend fi v o ->
        Pretty.braces $ Pretty.hsep [pretty fi <> ":" <> pretty v, "|", pretty o]
      ReadEnvObject ty o ->
        "read-object" <+> "@{" <> pretty ty <> "}" <> pretty o


termInfo :: Lens' (Term name builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Lam term i -> Lam term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  Block terms i -> Block terms <$> f i
  ObjectLit obj i -> ObjectLit obj <$> f i
  ObjectOp o i -> ObjectOp o <$> f i
  ListLit v i -> ListLit v <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i

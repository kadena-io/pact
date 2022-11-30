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
 , EvalDef
 , fromIRTerm
 , fromIRDef
 , fromIRModule
 , fromIRTopLevel
 , termInfo
 ) where

import Control.Lens
import Data.Text(Text)
import Data.Void
import Data.Foldable(foldl')
import qualified Data.Set as Set

import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Imports
import Pact.Core.Hash
import Pact.Core.Pretty(Pretty(..), pretty, (<+>))

import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.IR.Term as IR

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
  , _dcType :: Type Void
  , _dcTerm :: Term name builtin info
  , _dcInfo :: info
  } deriving Show

-- data DefCap name builtin info
--   = DefCap
--   { _dcapName :: Text
--   , _dcapArgs :: [Text]
--   , _dcapTerm :: Term name builtin info
--   , _dcapCapType :: CapType name
--   , _dcapType :: Type NamedDeBruijn
--   , _dcapInfo :: info
--   } deriving Show

data Def name builtin info
  = Dfun (Defun name builtin info)
  | DConst (DefConst name builtin info)
  deriving Show
  -- | DCap (DefCap name builtin info)

-- DCap (DefCap name builtin info)
-- DPact (DefPact name builtin info)
-- DSchema (DefSchema name info)
-- DTable (DefTable name info)
defType :: Def name builtin info -> Type Void
defType = \case
  Dfun d -> _dfunType d
  DConst d -> _dcType d
  -- DCap d -> _dcapType d

defName :: Def name builtin i -> Text
defName = \case
  Dfun d -> _dfunName d
  DConst d -> _dcName d
  -- DCap d -> _dcapName d

defTerm :: Def name builtin info -> Term name builtin info
defTerm = \case
  Dfun d -> _dfunTerm d
  DConst d -> _dcTerm d
  -- DCap d -> _dcapTerm d

data Module name builtin info
  = Module
  { _mName :: ModuleName
  -- , _mGovernance :: Governance name
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
V1 = Theme: simple, modular, more efficientNamedDeBruijn
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
  | Sequence (Term name builtin info) (Term name builtin info) info
  -- ^ (e_1 e_2 .. e_n)
  | Builtin builtin info
  -- ^ Built-in functions (or natives)
  | Constant Literal info
  -- ^ ΛX.e
  | ListLit [Term name builtin info] info
  -- ^ [e_1, e_2, .., e_n]
  | Try (Term name builtin info) (Term name builtin info) info
  -- ^ Error catching
  | Error Text info
  -- | ObjectLit (Map Field (Term name builtin info)) info
  -- ^ {f_1:e_1, .., f_n:e_n}
  -- | ObjectOp (ObjectOp (Term name builtin info)) info
  -- Object access, update and remove
  deriving (Show, Functor)

-- Post Typecheck terms + modules
type EvalTerm b i = Term Name b i
type EvalDef b i = Def Name b i
type EvalModule b i = Module Name b i

fromIRTerm :: IR.Term n b i -> Term n b i
fromIRTerm = \case
  IR.Var n i -> Var n i
  IR.Lam nsts body i ->
    foldr (\_ t -> Lam t i) (fromIRTerm body) nsts
  IR.Let _ _ e1 e2 i ->
    App (Lam (fromIRTerm e1) i) (fromIRTerm e2) i
  IR.App fn apps i ->
    foldl' (\f arg -> App f (fromIRTerm arg) i) (fromIRTerm fn) apps
  IR.Builtin b i ->
    Builtin b i
  IR.Constant lit i ->
    Constant lit i
  IR.Sequence e1 e2 i ->
    Sequence (fromIRTerm e1) (fromIRTerm e2) i
  IR.ListLit v i ->
    ListLit (fromIRTerm <$> v) i
  IR.Try e1 e2 i ->
    Try (fromIRTerm e1) (fromIRTerm e2) i
  IR.Error e i ->
    Error e i
  -- IR.ObjectLit m i ->
  --   ObjectLit (fromIRTerm <$> m) i
  -- IR.ObjectOp oo i ->
  --   ObjectOp (fromIRTerm <$> oo) i

---------
fromIRDefun
  :: IR.Defun name builtin info
  -> Defun name builtin info
fromIRDefun (IR.Defun n ty term i) =
  Defun n (fmap absurd ty) (fromIRTerm term) i

fromIRDConst
  :: IR.DefConst name builtin info
  -> DefConst name builtin info
fromIRDConst (IR.DefConst n ty term i) =
  DefConst n (maybe TyUnit (fmap absurd) ty) (fromIRTerm term) i

-- fromIRDCap
--   :: IR.DefCap name builtin info
--   -> DefCap name builtin info
-- fromIRDCap (IR.DefCap name args term captype ty info) =
--   DefCap name args (fromIRTerm term) captype (absurd <$> ty) info

fromIRDef
  :: IR.Def name builtin info
  -> Def name builtin info
fromIRDef = \case
  IR.Dfun d -> Dfun (fromIRDefun d)
  IR.DConst d -> DConst (fromIRDConst d)
  -- IR.DCap d -> DCap (fromIRDCap d)

fromIRModule
  :: IR.Module name builtin info
  -> Module name builtin info
fromIRModule (IR.Module mn defs blessed imports implements hs) =
  Module mn (fromIRDef <$> defs) blessed imports implements hs

fromIRTopLevel
  :: IR.TopLevel name builtin info
  -> TopLevel name builtin info
fromIRTopLevel = \case
  IR.TLModule m -> TLModule (fromIRModule m)
  IR.TLInterface _ -> error "todo: implement interfaces"
  IR.TLTerm e -> TLTerm (fromIRTerm e)

instance (Pretty name, Pretty builtin) => Pretty (Term name builtin info) where
  pretty = \case
    Var n _ ->
      pretty n
    Lam term _ ->
      Pretty.parens ("λ." <> pretty term)
    App t1 t2 _ ->
      Pretty.parens (Pretty.hsep [pretty t1, pretty t2])
    Builtin b _ -> pretty b
    Constant l _ -> pretty l
    Sequence e1 e2 _ -> Pretty.parens ("seq" <+> pretty e1 <+> pretty e2)
    ListLit li _ ->
      Pretty.brackets $
      Pretty.hsep $
      Pretty.punctuate Pretty.comma (pretty <$> li)
    Try e1 e2 _ ->
      Pretty.parens ("try" <+> pretty e1 <+> pretty e2)
    Error e _ ->
      Pretty.parens ("error \"" <> pretty e <> "\"")
    -- ObjectLit (Map.toList -> obj) _ ->
    --   Pretty.braces $
    --   Pretty.hsep $
    --   Pretty.punctuate Pretty.comma $
    --   fmap (\(f, o) -> pretty f <> ":" <+> pretty o) obj
    -- ObjectOp oop _ -> case oop of
    --   ObjectAccess fi te ->
    --     Pretty.parens $ Pretty.hsep ["@" <> pretty fi, pretty te]
    --   ObjectRemove fi te ->
    --     Pretty.parens $ Pretty.hsep ["#" <> pretty fi, pretty te]
    --   ObjectExtend fi v o ->
    --     Pretty.braces $ Pretty.hsep [pretty fi <> ":" <> pretty v, "|", pretty o]

termInfo :: Lens' (Term name builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Lam term i -> Lam term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  Sequence e1 e2 i -> Sequence e1 e2 <$> f i
  -- ObjectLit obj i -> ObjectLit obj <$> f i
  -- ObjectOp o i -> ObjectOp o <$> f i
  ListLit v i -> ListLit v <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i
  Try e1 e2 i ->
    Try e1 e2 <$> f i
  Error e i ->
    Error e <$> f i

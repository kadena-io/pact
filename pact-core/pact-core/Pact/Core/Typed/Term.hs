{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Pact.Core.Typed.Term
 ( Defun(..)
 , DefConst(..)
 , DefCap(..)
 , Def(..)
 , Term(..)
 , Module(..)
 , Interface(..)
 , IfDefun(..)
 , IfDef(..)
 , TopLevel(..)
 , ReplTopLevel(..)
 , Literal(..)
 , TyVarType(..)
 , termInfo
 , traverseTermType
 , ETerm
 -- Post-overload
 , OverloadedTerm
 , OverloadedDefun
 , OverloadedDefConst
 , OverloadedDefCap
 , OverloadedDef
 , OverloadedModule
 , OverloadedTopLevel
 , OverloadedReplTopLevel
 -- Flexible Eval terms
 , EvalTerm
 , EvalModule
 -- On-chain eval terms
 , CoreEvalTerm
 , CoreEvalDefun
 , CoreEvalDefConst
 , CoreEvalDef
 , CoreEvalModule
 , CoreEvalTopLevel
 , CoreEvalReplTopLevel
 , defName
 , defType
 , defTerm
 ) where

import Control.Lens
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.List.NonEmpty(NonEmpty)
import Data.Vector (Vector)
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

data Defun name tyname builtin info
  = Defun
  { _dfunName :: Text
  , _dfunType :: Type tyname
  , _dfunTerm :: Term name tyname builtin info
  , _dfunInfo :: info
  } deriving Show

data DefConst name tyname builtin info
  = DefConst
  { _dcName :: Text
  , _dcType :: Type tyname
  , _dcTerm :: Term name tyname builtin info
  , _dcInfo :: info
  } deriving Show

data DefCap name tyname builtin info
  = DefCap
  { _dcapName :: Text
  , _dcapArgs :: [Text]
  , _dcapTerm :: Term name tyname builtin info
  , _dcapCapType :: CapType name
  , _dcapType :: Type tyname
  , _dcapInfo :: info
  } deriving Show

data Def name tyname builtin info
  = Dfun (Defun name tyname builtin info)
  | DConst (DefConst name tyname builtin info)
  | DCap (DefCap name tyname builtin info)
   deriving Show

-- DCap (DefCap name tyname builtin info)
-- DPact (DefPact name tyname builtin info)
-- DSchema (DefSchema name tyname info)
-- DTable (DefTable name tyname info)
defType :: Def name tyname builtin info -> Type tyname
defType = \case
  Dfun d -> _dfunType d
  DConst d -> _dcType d
  DCap d -> _dcapType d


defName :: Def name tyname builtin i -> Text
defName = \case
  Dfun d -> _dfunName d
  DConst d -> _dcName d
  DCap d -> _dcapName d

defTerm :: Def name tyname builtin info -> Term name tyname builtin info
defTerm = \case
  Dfun d -> _dfunTerm d
  DConst d -> _dcTerm d
  DCap d -> _dcapTerm d

data Module name tyname builtin info
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance name
  , _mDefs :: [Def name tyname builtin info]
  , _mBlessed :: !(Set.Set ModuleHash)
  , _mImports :: [Import]
  , _mImplemented :: [ModuleName]
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
  { _ifdName :: Text
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

data ReplTopLevel name tyname builtin info
  = RTLModule (Module name tyname builtin info)
  -- | RTLInterface (Interface name tyname builtin info)
  | RTLDefun (Defun name tyname builtin info)
  | RTLDefConst (DefConst name tyname builtin info)
  | RTLTerm (Term name tyname builtin info)
  deriving Show

data TyVarType
  = TyVarType
  | RowVarType
  deriving Show

-- | Typed pact core terms
data Term name tyname builtin info
  = Var name info
  -- ^ single variables, e.g the term `x`
  | Lam (NonEmpty (name, Type tyname)) (Term name tyname builtin info) info
  -- ^ f = \a b c -> e
  -- All lambdas, even anonymous ones, are named, for the sake of them adding a stack frame
  | App (Term name tyname builtin info) (NonEmpty (Term name tyname builtin info)) info
  -- let n = e1 in e2
  | Let name (Term name tyname builtin info) (Term name tyname builtin info) info
  -- ^ (e_1 e_2 .. e_n)
  | Builtin builtin info
  -- ^ Built-in functions (or natives)
  | Constant Literal info
  -- ^ Constant/Literal values
  | TyApp (Term name tyname builtin info) (NonEmpty (Type tyname)) info
  -- ^ (e_1 @t)
  | TyAbs (NonEmpty tyname) (Term name tyname builtin info) info
  -- ^ ΛX.e
  | Block (NonEmpty (Term name tyname builtin info)) info
  -- ^ ΛX.e
  | ObjectLit (Map Field (Term name tyname builtin info)) info
  -- ^ {f_1:e_1, .., f_n:e_n}
  | ListLit (Type tyname) (Vector (Term name tyname builtin info)) info
  -- ^ [e_1, e_2, .., e_n]
  | ObjectOp (ObjectOp (Term name tyname builtin info)) info
  -- ^ error terms + their inferred type
  deriving (Show, Functor)

-- Post Typecheck terms + modules
type OverloadedTerm b i =
  Term (OverloadedName (Pred NamedDeBruijn)) NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i
type OverloadedDefun b i =
  Defun (OverloadedName (Pred NamedDeBruijn)) NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i
type OverloadedDefConst b i =
  DefConst (OverloadedName (Pred NamedDeBruijn)) NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i
type OverloadedDefCap b i =
  DefCap (OverloadedName (Pred NamedDeBruijn)) NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i
type OverloadedDef b i =
  Def (OverloadedName (Pred NamedDeBruijn)) NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i
type OverloadedModule b i =
  Module (OverloadedName (Pred NamedDeBruijn)) NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i
type OverloadedTopLevel b i =
  TopLevel (OverloadedName (Pred NamedDeBruijn)) NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i
type OverloadedReplTopLevel b i =
  ReplTopLevel (OverloadedName (Pred NamedDeBruijn)) NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i

-- Ideally flexible eval types.
type EvalTerm b i = Term Name NamedDeBruijn b i
type EvalModule b i = Module Name NamedDeBruijn b i

-- On-chain, core builtin-types
type CoreEvalTerm i = Term Name NamedDeBruijn CoreBuiltin i
type CoreEvalDefun i = Defun Name NamedDeBruijn CoreBuiltin i
type CoreEvalDefConst i = DefConst Name NamedDeBruijn CoreBuiltin i
type CoreEvalDef i = Def Name NamedDeBruijn CoreBuiltin i
type CoreEvalModule i = Module Name NamedDeBruijn CoreBuiltin i
type CoreEvalTopLevel i = TopLevel Name NamedDeBruijn CoreBuiltin i
type CoreEvalReplTopLevel i = ReplTopLevel Name NamedDeBruijn CoreBuiltin i
type ETerm b = Term Name NamedDeBruijn b ()


instance (Pretty n, Pretty tn, Pretty b) => Pretty (Term n tn b i) where
  pretty = \case
    Var n _ -> pretty n
    Lam (NE.toList -> ns) body _ ->
      "λ" <> Pretty.hsep (fmap (\(n, t) -> Pretty.parens (pretty n <> ":" <+> pretty t)) ns) <+> "->" <+> pretty body
    App l (NE.toList -> nel) _ ->
      pretty l <> Pretty.parens (Pretty.hsep (Pretty.punctuate Pretty.comma (pretty <$> nel)))
    Let n e1 e2 _ ->
      "let" <+> pretty n <+> "=" <+> pretty e1 <+> prettyFollowing e2
      where
      prettyFollowing e@Let{} = Pretty.hardline <> pretty e
      prettyFollowing (Block nel _) = Pretty.nest 2 $
        "in {" <> Pretty.hardline <> prettyBlock nel <> "}"
      prettyFollowing e = Pretty.hardline <> "in" <+> pretty e
    TyApp t (NE.toList -> apps) _ ->
      pretty t <+> Pretty.hsep (fmap (prettyTyApp) apps)
    TyAbs (NE.toList -> tyabs) t _ ->
      "Λ" <> Pretty.hsep (pretty <$> tyabs) <+> "->" <+> pretty t
    Block nel _ -> Pretty.nest 2 $
      "{" <> Pretty.hardline <> prettyBlock nel <> "}"
    ObjectLit (Map.toList -> obj) _ ->
      Pretty.braces $ Pretty.hsep $ Pretty.punctuate Pretty.comma $ fmap (\(f, o) -> pretty f <> ":" <+> pretty o) obj
    ObjectOp oop _ -> case oop of
      ObjectAccess f o ->
        "accessObj" <> Pretty.brackets ("'" <> pretty f) <> Pretty.parens (pretty o)
      ObjectRemove f o ->
        "removeObj" <> Pretty.brackets ("'" <> pretty f) <> Pretty.parens (pretty o)
      ObjectExtend f v o ->
        "updateObj" <> Pretty.brackets ("'" <> pretty f) <> Pretty.parens (pretty o <> "," <+> pretty v)
      ReadEnvObject ty o ->
        "read-object" <+> "@{" <> pretty ty <> "}" <> pretty o
    ListLit ty (V.toList -> li) _ ->
      (Pretty.brackets $ Pretty.hsep $ Pretty.punctuate Pretty.comma $ (pretty <$> li)) <> if null li then prettyTyApp ty else mempty
    Builtin b _ -> pretty b
    Constant l _ -> pretty l
    where
    prettyTyApp ty = "@(" <> pretty ty <> ")"
    prettyBlock (NE.toList -> nel) =
      Pretty.vsep (pretty <$> nel)

termInfo :: Lens' (Term name tyname builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Lam ns term i -> Lam ns term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  Let n e1 e2 i ->
    Let n e1 e2 <$> f i
  TyApp term ty i -> TyApp term ty <$> f i
  TyAbs ty term i -> TyAbs ty term <$> f i
  Block terms i -> Block terms <$> f i
  ObjectLit obj i -> ObjectLit obj <$> f i
  ObjectOp o i -> ObjectOp o <$> f i
  ListLit ty v i -> ListLit ty v <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i

traverseTermType :: Traversal' (Term name tyname builtin info) (Type tyname)
traverseTermType f = \case
  Var n i -> pure (Var n i)
  Lam ns body i ->
    Lam <$> (traversed._2) f ns <*> traverseTermType f body <*> pure i
  App l r i ->
    App <$> traverseTermType f l <*> traverse (traverseTermType f) r <*> pure i
  Let n e1 e2 i ->
    Let n <$> traverseTermType f e1 <*> traverseTermType f e2 <*> pure i
  TyApp l tyapps i ->
    TyApp <$> traverseTermType f l <*> traverse f tyapps <*> pure i
  TyAbs tyabs body i ->
    TyAbs tyabs <$> traverseTermType f body <*> pure i
  Block nel i ->
    Block <$> traverse (traverseTermType f) nel <*> pure i
  ObjectLit obj i ->
    ObjectLit <$> traverse (traverseTermType f) obj <*> pure i
  ObjectOp oop i ->
    ObjectOp <$> traverse (traverseTermType f) oop <*> pure i
  ListLit ty v i ->
    ListLit <$> f ty <*> traverse (traverseTermType f) v <*> pure i
  Constant l i -> pure (Constant l i)
  Builtin b i -> pure (Builtin b i)

instance Plated (Term name tyname builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam ns term i -> Lam ns <$> f term <*> pure i
    App t1 t2 i -> App <$> f t1 <*> traverse f t2 <*> pure i
    Let n e1 e2 i ->
      Let n <$> f e1 <*> f e2 <*> pure i
    TyApp term ty i -> TyApp <$> f term <*> pure ty <*> pure i
    TyAbs ty term i -> TyAbs ty <$> f term <*> pure i
    ObjectLit tm i ->
      ObjectLit <$> traverse f tm <*> pure i
    ListLit ty ts i ->
      ListLit ty <$> traverse f ts <*> pure i
    ObjectOp oop i ->
      ObjectOp <$> traverse f oop <*> pure i
    Block terms i -> Block <$> traverse f terms <*> pure i
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)


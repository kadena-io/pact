{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Core.Syntax.Common where

import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.List(intersperse)
import Data.Foldable(fold)
import Data.List.NonEmpty(NonEmpty(..))

import Pact.Core.Names
import Pact.Core.Pretty
import Pact.Core.Type(PrimType(..))
import Pact.Core.Imports
import Pact.Core.Guards

import qualified Data.Map.Strict as Map


data UnaryOp
  = NegateOp
  | FlipBitsOp
  deriving Show

instance Pretty UnaryOp where
  pretty NegateOp = "-"
  pretty FlipBitsOp = "~"

data BinaryOp
  = AddOp
  | SubOp
  | MultOp
  | DivOp
  | GTOp
  | GEQOp
  | LTOp
  | LEQOp
  | EQOp
  | NEQOp
  | BitAndOp
  | BitOrOp
  | AndOp
  | OrOp
  deriving Show


instance Pretty BinaryOp where
  pretty = \case
    AddOp -> "+"
    SubOp -> "-"
    MultOp -> "*"
    DivOp -> "/"
    GTOp -> ">"
    GEQOp -> ">="
    LTOp -> "<"
    LEQOp -> "<="
    EQOp -> "=="
    NEQOp -> "!="
    BitAndOp -> "&"
    BitOrOp -> "|"
    AndOp -> "and"
    OrOp -> "or"


-- Todo: type constructors aren't 1-1 atm.
data Type
  = TyPrim PrimType
  | TyFun Type Type
  | TyList Type
  | TyObject (Map Field Type)
  | TyCap
  deriving Show

pattern TyInt :: Type
pattern TyInt = TyPrim PrimInt

pattern TyDecimal :: Type
pattern TyDecimal = TyPrim PrimDecimal

pattern TyTime :: Type
pattern TyTime = TyPrim PrimTime

pattern TyBool :: Type
pattern TyBool = TyPrim PrimBool

pattern TyString :: Type
pattern TyString = TyPrim PrimString

pattern TyUnit :: Type
pattern TyUnit = TyPrim PrimUnit

-- | Do we render parenthesis for the type if it shows nested in another
instance Pretty Type where
  pretty = \case
    TyPrim prim -> pretty prim
    TyFun l r -> case l of
      TyFun _ _ ->
        parens (pretty l) <+> "->" <+> pretty r
      _ -> pretty l <+> "->" <+> pretty r
    TyList t -> "List" <+> renderListParens t (pretty t)
    TyObject fields ->
      "{" <> fold (intersperse ", " $ renderMapObjs <$> Map.toList fields) <> "}"
    TyCap -> "Capability"
    where
    renderMapObjs (Field f, t) = pretty f <+> ":" <+> pretty t
    renderListParens = \case
      TyList _ -> parens
      TyFun _ _ -> parens
      _ -> id


----------------------------------------------------
-- Common structures
----------------------------------------------------

data Arg
  = Arg
  { _argName :: Text
  , _argType :: Type }
  deriving Show

data Defun expr i
  = Defun
  { _dfunName :: !Text
  , _dfunArgs :: ![Arg]
  , _dfunRetType :: !Type
  , _dfunTerm :: !expr
  , _dfunInfo :: i
  } deriving Show

data DefConst expr i
  = DefConst
  { _dcName :: Text
  , _dcType :: Maybe Type
  , _dcTerm :: expr
  , _dcInfo :: i
  } deriving Show

data Managed
  = AutoManaged
  | Managed Text ParsedName
  deriving (Show)
  --

data DefCap expr i
  = DefCap
  { _dcapName :: Text
  , _dcapArgs :: ![Arg]
  , _dcapManaged :: Maybe Managed
  , _dcapTerm :: expr
  } deriving Show

data Def expr i
  = Dfun (Defun expr i)
  | DConst (DefConst expr i)
  | DCap (DefCap expr i)
  deriving Show

data ExtDecl
  = ExtBless Text
  | ExtImport Import
  | ExtImplements ModuleName
  deriving Show

data Module expr i
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance Text
  , _mExternal :: [ExtDecl]
  , _mDefs :: NonEmpty (Def expr i)
  } deriving Show

data TopLevel expr i
  = TLModule (Module expr i)
  | TLTerm expr
  deriving Show

data ReplTopLevel expr i
  = RTLModule (Module expr i)
  | RTLDefun (Defun expr i)
  | RTLDefConst (DefConst expr i)
  | RTLTerm expr
  deriving Show

data Interface expr i
  = Interface
  { _ifName :: Text
  , _ifDefns :: [IfDef expr i]
  } deriving Show

data IfDefun i
  = IfDefun
  { _ifdName :: Text
  , _ifdArgs :: [Arg]
  , _ifdType :: Type
  , _ifdInfo :: i
  } deriving Show

data IfDef expr i
  = IfDfun (IfDefun i)
  | IFDConst (DefConst expr i)
  deriving Show

instance Pretty Arg where
  pretty (Arg n ty) =
    pretty n <> ":" <+> pretty ty

instance Pretty expr => Pretty (Defun expr i) where
  pretty (Defun n args rettype term _) =
    "defun" <+> pretty n <+> parens (prettyCommaSep args) <> ":" <+> pretty rettype <+> "=" <+> pretty term

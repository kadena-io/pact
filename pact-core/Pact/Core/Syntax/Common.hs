{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Core.Syntax.Common where

import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))

import Pact.Core.Names
import Pact.Core.Pretty
import Pact.Core.Type(PrimType(..))
import Pact.Core.Imports


data UnaryOp
  = NegateOp
  | ComplementOp
  deriving Show

instance Pretty UnaryOp where
  pretty NegateOp = "-"
  pretty ComplementOp = "~"

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
  -- | TyObject (Map Field Type)
  -- | TyCap
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
    -- TyObject fields ->
    --   "{" <> fold (intersperse ", " $ renderMapObjs <$> Map.toList fields) <> "}"
    -- TyCap -> "Capability"
    where
    -- renderMapObjs (Field f, t) = pretty f <+> ":" <+> pretty t
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

data Defun e i
  = Defun
  { _dfunName :: !Text
  , _dfunArgs :: ![Arg]
  , _dfunRetType :: !Type
  , _dfunTerm :: !e
  , _dfunInfo :: i
  } deriving Show

data DefConst e i
  = DefConst
  { _dcName :: Text
  , _dcType :: Maybe Type
  , _dcTerm :: e
  , _dcInfo :: i
  } deriving Show

data Managed
  = AutoManaged
  | Managed Text ParsedName
  deriving (Show)

-- data DefCap e i
--   = DefCap
--   { _dcapName :: Text
--   , _dcapArgs :: ![Arg]
--   , _dcapManaged :: Maybe Managed
--   , _dcapTerm :: e
--   , _dcapInfo :: i
--   } deriving Show

data Def e i
  = Dfun (Defun e i)
  | DConst (DefConst e i)
  -- | DCap (DefCap e i)
  deriving Show

data ExtDecl
  = ExtBless Text
  | ExtImport Import
  | ExtImplements ModuleName
  deriving Show

data Module e i
  = Module
  { _mName :: ModuleName
  -- , _mGovernance :: Governance Text
  , _mExternal :: [ExtDecl]
  , _mDefs :: NonEmpty (Def e i)
  } deriving Show

data TopLevel e i
  = TLModule (Module e i)
  | TLTerm e
  deriving Show

data ReplTopLevel e i
  = RTLModule (Module e i)
  | RTLDefun (Defun e i)
  | RTLDefConst (DefConst e i)
  | RTLTerm e
  deriving Show

data Interface e i
  = Interface
  { _ifName :: Text
  , _ifDefns :: [IfDef e i]
  } deriving Show

data IfDefun i
  = IfDefun
  { _ifdName :: Text
  , _ifdArgs :: [Arg]
  , _ifdType :: Type
  , _ifdInfo :: i
  } deriving Show

data IfDef e i
  = IfDfun (IfDefun i)
  | IFDConst (DefConst e i)
  deriving Show

instance Pretty Arg where
  pretty (Arg n ty) =
    pretty n <> ":" <+> pretty ty

instance Pretty e => Pretty (Defun e i) where
  pretty (Defun n args rettype term _) =
    "defun" <+> pretty n <+> parens (prettyCommaSep args)
      <> ":" <+> pretty rettype <+> "=" <+> pretty term

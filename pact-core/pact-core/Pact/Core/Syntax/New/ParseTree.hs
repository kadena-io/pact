{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Core.Syntax.New.ParseTree where

import Control.Lens hiding (List, op)
import Data.Foldable(fold)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import Data.List(intersperse)

import Pact.Core.Builtin
import Pact.Core.Type(PrimType(..))
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Imports
import Pact.Core.Pretty

data Arg
  = Arg
  { _argName :: Text
  , _argType :: Type }
  deriving Show

data Defun name i
  = Defun
  { _dfunName :: !Text
  , _dfunArgs :: ![Arg]
  , _dfunRetType :: !Type
  , _dfunTerm :: !(Expr name i)
  , _dfunInfo :: i
  } deriving Show

data DefConst name i
  = DefConst
  { _dcName :: Text
  , _dcType :: Maybe Type
  , _dcTerm :: Expr name i
  , _dcInfo :: i
  } deriving Show

data DefCap name i
  = DefCap
  { _dcapName :: Text
  , _dcapArgs :: ![Arg]
  , _dcapTerm :: Expr name i
  } deriving Show

data Def name i
  = Dfun (Defun name i)
  | DConst (DefConst name i)
  -- | DCap (DefCap name i)
  deriving Show

data ExtDecl
  = ExtBless Text
  | ExtImport Import
  | ExtImplements ModuleName
  deriving Show

data Module name i
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance Text
  , _mExternal :: [ExtDecl]
  , _mDefs :: (NonEmpty (Def name i))
  } deriving Show

data Interface name i
  = Interface
  { _ifName :: Text
  , _ifDefns :: [IfDef name i]
  } deriving Show

data IfDefun i
  = IfDefun
  { _ifdName :: Text
  , _ifdArgs :: [Arg]
  , _ifdType :: Type
  , _ifdInfo :: i
  } deriving Show

data IfDef name i
  = IfDfun (IfDefun i)
  | IFDConst (DefConst name i)
  deriving Show

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
    AndOp -> "&&"
    OrOp -> "||"


-- Todo: type constructors aren't 1-1 atm.
data Type
  = TyPrim PrimType
  | TyVar Text
  | TyFun Type Type
  | TyList Type
  -- | TyCtor (NonEmpty ParsedType)
  | TyObject (Map Field Type) (Maybe Text)
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
    TyVar t -> pretty t
    TyFun l r -> case l of
      TyFun _ _ ->
        parens (pretty l) <+> "->" <+> pretty r
      _ -> pretty l <+> "->" <+> pretty r
    TyList t -> "List" <+> renderListParens t (pretty t)
    TyObject fields r ->
      "{" <> fold (intersperse ", " $ renderMapObjs <$> Map.toList fields) <> renderRowVar r <> "}"
    TyCap -> "Capability"
    where
    renderMapObjs (Field f, t) = pretty f <+> ":" <+> pretty t
    renderRowVar = \case
      Nothing -> mempty
      Just rv -> " | "  <> pretty rv
    renderListParens = \case
      TyList _ -> parens
      TyFun _ _ -> parens
      _ -> id

data Expr name i
  = Var name i
  | Let Text (Maybe Type) (Expr name i) i
  | LetIn Text (Maybe Type) (Expr name i) (Expr name i) i
  | NestedDefun (Defun name i) i
  | Lam name (NonEmpty (Text, Maybe Type)) (Expr name i) i
  | If (Expr name i) (Expr name i) (Expr name i) i
  | App (Expr name i) [Expr name i] i
  | Block (NonEmpty (Expr name i)) i
  | Object (Map Field (Expr name i)) i
  | UnaryOp UnaryOp (Expr name i) i
  | BinaryOp BinaryOp (Expr name i) (Expr name i) i
  | List [Expr name i] i
  | Constant Literal i
  | ObjectOp (ObjectOp (Expr name i)) i
  | Error Text i
  deriving Show

termInfo :: Lens' (Expr name i) i
termInfo f = \case
  Var n i -> Var n <$> f i
  Let a b c i -> Let a b c <$> f i
  LetIn a b c d i ->
    LetIn a b c d <$> f i
  Lam n nel e i ->
    Lam n nel e <$> f i
  If e1 e2 e3 i ->
    If e1 e2 e3 <$> f i
  App e1 args i -> App e1 args <$> f i
  Block nel i -> Block nel <$> f i
  NestedDefun d i -> NestedDefun d <$> f i
  Object m i -> Object m <$> f i
  UnaryOp _op e i -> UnaryOp _op e <$> f i
  BinaryOp _op e1 e2 i -> BinaryOp _op e1 e2 <$> f i
  List nel i -> List nel <$> f i
  ObjectOp o i -> ObjectOp o <$> f i
  Constant l i -> Constant l <$> f i
  Error e i -> Error e <$> f i

prettyCommaSepNE :: Pretty a => NonEmpty a -> Doc ann
prettyCommaSepNE = fold . NE.intersperse ", " . fmap pretty

prettyCommaSep :: Pretty a => [a] -> Doc ann
prettyCommaSep = fold . intersperse ", " . fmap pretty


instance Pretty Arg where
  pretty (Arg n ty) =
    pretty n <> ":" <+> pretty ty

instance Pretty name => Pretty (Defun name i) where
  pretty (Defun n args rettype term _) =
    "defun" <+> pretty n <+> parens (prettyCommaSep args) <> ":" <+> pretty rettype <+> "=" <+> pretty term

instance Pretty name => Pretty (Expr name i) where
  pretty = \case
    Var n _ -> pretty n
    Let n mt e _ ->
      "let" <+> pretty n <> maybe mempty (\b -> " :" <+> pretty b) mt <+> "=" <+> pretty e
    LetIn n mt e1 e2 _ ->
      "let"
        <+> pretty n
        <> maybe mempty (\b -> " :" <+> pretty b) mt
        <+> "="
        <+> pretty e1
        <+> "in"
        <+> pretty e2
    Lam _ nel e _ ->
      "lambda" <+> renderLamTypes nel <+> "=>" <+> pretty e
    If cond e1 e2 _ ->
      "if" <+> pretty cond <+> "then" <+> pretty e1 <+> "else" <+> pretty e2
    App e1 nel _ ->
      pretty e1 <> parens (prettyCommaSep nel)
    UnaryOp uop e1 _ ->
      pretty uop <> pretty e1
    BinaryOp b e1 e2 _ ->
      pretty e1 <+> pretty b <+> pretty e2
    NestedDefun d _ ->
      pretty d
    Block nel _ ->
      "{" <+> nest 2 (hardline <> vsep (pretty <$> NE.toList nel)) <> hardline <> "}"
    Object m _ ->
      "{" <> prettyObj m <> "}"
    Constant l _ ->
      pretty l
    List nel _ ->
      "[" <> prettyCommaSep nel <> "]"
    ObjectOp op _ -> case op of
      ObjectAccess f o ->
        pretty o <> "->" <> pretty f
      ObjectRemove f o ->
        pretty o <> "#" <> pretty f
      ObjectUpdate f u o ->
        pretty o <> braces (pretty f <> ":=" <> pretty u)
    -- Todo: fix errors
    Error e _ ->
      "throw" <+> dquotes (pretty e)
    where
    prettyObj m =
      fold $ intersperse ", " $ fmap (\(k, v) -> pretty k <> ":" <+> pretty v) (Map.toList m)
    renderLamPair (n, mt) = case mt of
      Nothing -> pretty n
      Just t -> parens $ pretty n <+> ":" <+> pretty t
    renderLamTypes = fold . NE.intersperse " " . fmap renderLamPair

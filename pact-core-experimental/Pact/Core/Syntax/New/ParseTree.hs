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
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Pretty

import Pact.Core.Syntax.Common


data Expr name i
  = Var name i
  | Let Text (Maybe Type) (Expr name i) i
  | LetIn Text (Maybe Type) (Expr name i) (Expr name i) i
  | NestedDefun (Defun (Expr name i) i) i
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
      "lambda" <+> renderLamTypes nel <+> ":" <+> pretty e
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
      ObjectExtend f u o ->
        pretty o <> braces (pretty f <> ":=" <> pretty u)
      ReadEnvObject ty o ->
        "read-object" <+> "@{" <> pretty ty <> "}" <+> pretty o

    -- Todo: fix errors
    where
    prettyObj m =
      fold $ intersperse ", " $ fmap (\(k, v) -> pretty k <> ":" <+> pretty v) (Map.toList m)
    renderLamPair (n, mt) = case mt of
      Nothing -> pretty n
      Just t -> parens $ pretty n <+> ":" <+> pretty t
    renderLamTypes = fold . NE.intersperse ", " . fmap renderLamPair

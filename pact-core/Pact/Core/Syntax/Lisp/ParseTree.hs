{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Core.Syntax.Lisp.ParseTree where

import Control.Lens hiding (List, op)
import Data.Foldable(fold)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.List.NonEmpty as NE

import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Pretty

import Pact.Core.Syntax.Common

data Binder i =
  Binder Text (Maybe Type) (Expr i)
  deriving Show

data Expr i
  = Var ParsedName i
  | LetIn (NonEmpty (Binder i)) (Expr i) i
  | Lam ParsedName (NonEmpty (Text, Maybe Type)) (Expr i) i
  | If (Expr i) (Expr i) (Expr i) i
  | App (Expr i) [Expr i] i
  | Block (NonEmpty (Expr i)) i
  | UnaryOp UnaryOp (Expr i) i
  | BinaryOp BinaryOp (Expr i) (Expr i) i
  | List [Expr i] i
  | Constant Literal i
  deriving Show

termInfo :: Lens' (Expr i) i
termInfo f = \case
  Var n i -> Var n <$> f i
  LetIn bnds e1 i ->
    LetIn bnds e1 <$> f i
  Lam n nel e i ->
    Lam n nel e <$> f i
  If e1 e2 e3 i ->
    If e1 e2 e3 <$> f i
  App e1 args i -> App e1 args <$> f i
  Block nel i -> Block nel <$> f i
  -- Object m i -> Object m <$> f i
  UnaryOp _op e i -> UnaryOp _op e <$> f i
  BinaryOp _op e1 e2 i -> BinaryOp _op e1 e2 <$> f i
  List nel i -> List nel <$> f i
  -- ObjectOp o i -> ObjectOp o <$> f i
  Constant l i -> Constant l <$> f i

instance Pretty (Expr i) where
  pretty = \case
    Var n _ -> pretty n
    LetIn{} -> error "todo: implement"
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
    Block nel _ ->
      "{" <+> nest 2 (hardline <> vsep (pretty <$> NE.toList nel)) <> hardline <> "}"
    -- Object m _ ->
    --   "{" <> prettyObj m <> "}"
    Constant l _ ->
      pretty l
    List nel _ ->
      "[" <> prettyCommaSep nel <> "]"
    -- ObjectOp op _ -> case op of
    --   ObjectAccess f o ->
    --     pretty o <> "->" <> pretty f
    --   ObjectRemove f o ->
    --     pretty o <> "#" <> pretty f
    --   ObjectExtend f u o ->
    --     pretty o <> braces (pretty f <> ":=" <> pretty u)
    -- Todo: fix errors
    where
    -- prettyObj m =
    --   fold $ intersperse ", " $ fmap (\(k, v) -> pretty k <> ":" <+> pretty v) (Map.toList m)
    renderLamPair (n, mt) = case mt of
      Nothing -> pretty n
      Just t -> parens $ pretty n <+> ":" <+> pretty t
    renderLamTypes = fold . NE.intersperse " " . fmap renderLamPair

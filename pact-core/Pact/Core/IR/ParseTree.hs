
module Pact.Core.IR.ParseTree where

import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Map.Strict(Map)


import Pact.Core.Type(PrimType(..))
import Pact.Core.Literal
import Pact.Core.Names

data UnaryOp
  = NegateOp
  | FlipBitsOp
  deriving Show

data BinaryOp
  = AddOp
  | SubOp
  | MultOp
  | DivOp
  | GTOp
  | GTEQOp
  | LTOp
  | LTEQOp
  | EQOp
  | NEQOp
  | BitAndOp
  | BitOrOp
  | BitReverseOp
  deriving Show

data ParsedType
  = TyPrim PrimType
  | TyVar Text
  | TyFun ParsedType ParsedType
  | TyList ParsedType
  -- | TyCtor (NonEmpty ParsedType)
  | TyObject (Map Field ParsedType) (Maybe Text)
  | TyCap
  deriving Show

data Expr name i
  = Var name i
  | Let (NonEmpty (name, Maybe ParsedType)) (Expr name i) i
  | Lam name (NonEmpty (name, (Maybe ParsedType))) (Expr name i) i
  | If (Expr name i) (Expr name i) (Expr name i) i
  | App (Expr name i) (NonEmpty (Expr name i)) i
  | Block (NonEmpty (Expr name i)) i
  | Object (Map Field (Expr name i)) i
  | UnaryOp UnaryOp (Expr name i) i
  | BinaryOp BinaryOp (Expr name i) (Expr name i) i
  | List (NonEmpty (Expr name i)) i
  | Constant Literal i
  | Error Text i
  deriving Show

data ParsedName
  = QN QualifiedName
  | BN BareName
  deriving Show

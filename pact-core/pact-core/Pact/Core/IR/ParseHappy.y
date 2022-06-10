{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.IR.ParseHappy where

import Control.Lens(view)
import Control.Monad.Except

import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.Map.Strict as Map
import qualified Data.Text.Read as T
import qualified Data.List.NonEmpty as NE

import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Literal
import Pact.Core.Builtin
import Pact.Core.Type(PrimType(..))
import Pact.Core.IR.ParseTree
import Pact.Core.IR.LexUtils


}
%name parseExpr Statement

%tokentype { PosToken }
%monad { ParserT }

%errorhandlertype explist
%error { parseError }


%token
  let        { PosToken TokenLet _ }
  in         { PosToken TokenIn _ }
  if         { PosToken TokenIf _ }
  else       { PosToken TokenElse _ }
  then       { PosToken TokenThen _ }
  lam        { PosToken TokenLambda _ }
  '=>'       { PosToken TokenLambdaArrow _ }
  module     { PosToken TokenModule _ }
  interface  { PosToken TokenInterface _ }
  defun      { PosToken TokenDefun _ }
  bless      { PosToken TokenBless _}
  implements { PosToken TokenImplements _ }
  true       { PosToken TokenTrue _ }
  false      { PosToken TokenFalse _ }
  '{'        { PosToken TokenOpenBrace _ }
  '}'        { PosToken TokenCloseBrace _ }
  '('        { PosToken TokenOpenParens _ }
  ')'        { PosToken TokenCloseParens _ }
  '['        { PosToken TokenOpenBracket _ }
  ']'        { PosToken TokenCloseBracket _ }
  ','        { PosToken TokenComma _ }
  ':'        { PosToken TokenSemiColon _ }
  '.'        { PosToken TokenDot _ }
  TYLIST     { PosToken TokenTyList _ }
  TYTABLE    { PosToken TokenTyTable _ }
  TYINTEGER  { PosToken TokenTyInteger _ }
  TYDECIMAL  { PosToken TokenTyDecimal _ }
  TYSTRING   { PosToken TokenTyString _ }
  TYBOOL     { PosToken TokenTyBool _ }
  TYUNIT     { PosToken TokenTyUnit _ }
  '->'       { PosToken TokenTyArrow _ }
  TYVAR      { PosToken (TokenTyVar _) _ }
  '='        { PosToken TokenAssign _ }
  '=='       { PosToken TokenEq _ }
  '!='       { PosToken TokenNeq _ }
  '>'        { PosToken TokenGT _ }
  '>='       { PosToken TokenGEQ _ }
  '<'        { PosToken TokenLT _ }
  '<='       { PosToken TokenLEQ _ }
  '+'        { PosToken TokenPlus _ }
  '-'        { PosToken TokenMinus _}
  '*'        { PosToken TokenMult _ }
  '/'        { PosToken TokenDiv _ }
  '@'        { PosToken TokenObjAccess _ }
  '#'        { PosToken TokenObjRemove _ }
  '&'        { PosToken TokenBitAnd _ }
  '|'        { PosToken TokenBitOr _ }
  '&&'       { PosToken TokenAnd _ }
  '||'       { PosToken TokenOr _ }
  IDENT      { PosToken (TokenIdent _) _ }
  NUM        { PosToken (TokenNumber _) _ }
  STR        { PosToken (TokenString _) _ }
  OPEN       { PosToken TokenVOpen _ }
  SEMI       { PosToken TokenVSemi _ }
  CLOSE      { PosToken TokenVClose _ }


%%

Type :: { Type }
  : TyArrows '->' Type1 { foldr TyFun $3 (reverse $1) }
  | Type1 { $1 }

TyArrows :: { [Type] }
  : TyArrows '->' Type1 { $3:$1 }
  | Type1 { [$1] }

Type1 :: { Type }
  : TYLIST AtomicType { TyList $2 }
  | TYLIST '(' Type ')' { TyList $3 }
  | '{' RowType '}' { TyObject (Map.fromList $2) Nothing }
  | AtomicType { $1 }

AtomicType :: { Type }
  : PrimType { TyPrim $1 }
  | TYVAR { TyVar (getTyvar $1) }

TyFieldPair :: { (Field, Type) }
  : IDENT ':' Type { (Field (getIdent $1), $3) }

RowType :: { [(Field, Type)] }
  : RowType ',' TyFieldPair { $3:$1 }
  | TyFieldPair { [$1] }
  | {- empty -} { [] }

PrimType :: { PrimType }
  : TYINTEGER { PrimInt }
  | TYDECIMAL { PrimDecimal }
  | TYSTRING  { PrimString }
  | TYUNIT    { PrimUnit }
  | TYBOOL    { PrimBool }

MTypeAnn :: { Maybe Type }
  : ':' Type { Just $2 }
  | {- empty -} { Nothing }

BoolOp :: { BinaryOp }
  : '&&' { AndOp }
  | '||' { OrOp }

EqOp :: { BinaryOp }
  : '==' { EQOp }
  | '!=' { NEQOp }

OrdOp :: { BinaryOp }
  : '>' { GTOp }
  | '>=' { GEQOp }
  | '<'  { LTOp }
  | '<=' { LEQOp }

AdditionOp :: { BinaryOp }
  : '+' { AddOp }
  | '-' { SubOp }

MultOp :: { BinaryOp }
  : '*' { MultOp }
  | '/' { DivOp }
  | '&' { BitAndOp }
  | '|' { BitOrOp }

PrefixOp :: { UnaryOp }
  : '-' { NegateOp }
  -- | '~' { FlipBitsOp }

Statement :: { ParsedExpr }
  : IfStmt { $1 }
  | LetStmt { $1 }
  | Expr { $1 }

IfStmt :: { ParsedExpr }
  : if Expr then MaybeBlock else MaybeBlock { If $2 $4 $6 (combineSpans $2 $6) }

MaybeBlock :: { ParsedExpr }
  : '{' OPEN BlockBody CLOSE '}' { Block (NE.fromList (reverse $3)) (combineSpan (_ptInfo $1) (_ptInfo $5)) }
  | Expr { $1 }

BlockBody :: { [ParsedExpr] }
  : BlockBody SEMI Statement { $3:$1 }
  | Statement { [$1] }

LetStmt :: { ParsedExpr }
  : let IDENT MTypeAnn '=' ParensExpr { Let (getIdent $2) $3 $5 (_ptInfo $1) }

Expr :: { ParsedExpr }
  : Expr BoolOp EqExpr { BinaryOp $2 $1 $3 (view termInfo $1) }
  | EqExpr { $1 }

EqExpr :: { ParsedExpr }
  : EqExpr EqOp OrdExpr { BinaryOp $2 $1 $3 (view termInfo $1) }
  | OrdExpr { $1 }

OrdExpr :: { ParsedExpr }
  : OrdExpr OrdOp AddExpr { BinaryOp $2 $1 $3 (view termInfo $1) }
  | AddExpr { $1 }

AddExpr :: { ParsedExpr }
  : AddExpr AdditionOp MulExpr { BinaryOp $2 $1 $3 (view termInfo $1) }
  | MulExpr { $1 }

MulExpr :: { ParsedExpr }
  : MulExpr MultOp NegateExpr { BinaryOp $2 $1 $3 (view termInfo $1) }
  | NegateExpr { $1 }

NegateExpr :: { ParsedExpr }
  : PrefixOp Atom { UnaryOp $1 $2 (view termInfo $2) }
  | PostFix { $1 }

PostFix :: { ParsedExpr }
  : PostFix '(' ExprCommaSep ')' { App $1 (reverse $3) (view termInfo $1) }
  | PostFix '@' IDENT { ObjectOp (ObjectAccess (Field (getIdent $3)) $1) (view termInfo $1) }
  | PostFix '#' IDENT {  ObjectOp (ObjectRemove (Field (getIdent $3)) $1) (view termInfo $1) }
  | Atom { $1 }

ExprCommaSep :: { [ParsedExpr] }
  : ExprCommaSep ',' ParensExpr { $3:$1 }
  | ParensExpr { [$1] }
  | {- empty -} { [] }

ParensExpr :: { ParsedExpr }
  : LamExpr { $1 }
  | Expr { $1 }

LamExpr :: { ParsedExpr }
  : lam LamArgs '=>' Expr { Lam ln0 (NE.fromList (reverse $2)) $4 (_ptInfo $1) }

LamArgs :: { [(Text, Maybe Type)] }
  : LamArgs '(' IDENT ':' Type ')' { (getIdent $3, Just $5):$1 }
  | LamArgs IDENT { (getIdent $2, Nothing):$1 }
  | '(' IDENT ':' Type ')' { [(getIdent $2, Just $4)] }
  | IDENT { [(getIdent $1, Nothing)] }


Atom :: { ParsedExpr }
  : Name { $1 }
  | Number { $1 }
  | String { $1 }
  | Object { $1 }
  | List { $1 }
  | Bool { $1 }
  | '(' ParensExpr ')' { $2 }

Bool :: { ParsedExpr }
  : true { Constant (LBool True) (_ptInfo $1) }
  | false { Constant (LBool False) (_ptInfo $1) }

Name :: { ParsedExpr }
  : IDENT '.' ModName  { mkModuleName (getIdent $1) $3 (_ptInfo $1) }
  | IDENT { Var (mkBarename (getIdent $1)) (_ptInfo $1) }

ModName :: { (Text, Maybe Text) }
  : IDENT '.' IDENT { (getIdent $1, Just (getIdent $3)) }
  | IDENT { (getIdent $1, Nothing) }

Number :: { ParsedExpr }
  : NUM '.' NUM { undefined }
  | NUM {% mkIntegerConstant (getNumber $1) (_ptInfo $1) }

String :: { ParsedExpr }
 : STR  { Constant (LString (getStr $1)) (_ptInfo $1) }

Object :: { ParsedExpr }
  : '{' ObjectBody '}' { Object $2 (_ptInfo $1) }

ObjectBody
  : FieldPairs { Map.fromList $1 }

FieldPair :: { (Field, ParsedExpr) }
  : IDENT ':' ParensExpr { (Field (getIdent $1), $3) }

FieldPairs :: { [(Field, ParsedExpr)] }
  : FieldPairs ',' FieldPair { $3 : $1 }
  | FieldPair { [$1] }
  | {- empty -} { [] }

List :: { ParsedExpr }
  : '[' ExprCommaSep ']' { List (reverse $2) (_ptInfo $1) }

{

combineSpans lexpr rexpr =
  let li = view termInfo lexpr
      ri = view termInfo rexpr
  in combineSpan li ri

getIdent (PosToken (TokenIdent x) _) = x
getNumber (PosToken (TokenNumber x) _) = x
getStr (PosToken (TokenString x) _ ) = x
getTyvar (PosToken (TokenTyVar x) _) = x

parseError s = throwError (show s)

mkIntegerConstant n i =
  case T.decimal n of
    Right (d, _) -> pure (Constant (LInteger d) i)
    _ -> throwError "Impossible"

mkModuleName ns (mod, (Just ident)) info =
  let ns' = NamespaceName ns
      qn = QualifiedName ident (ModuleName mod (Just ns'))
  in Var (QN qn) info
mkModuleName mod (ident, Nothing) info =
  let qn = QualifiedName ident (ModuleName mod Nothing)
  in Var (QN qn) info

ln0 = BN (BareName "")

mkBarename tx = BN (BareName tx)


}

{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Syntax.New.Parser where

import Control.Lens(preview, view, _head)
import Control.Monad.Except

import Data.Decimal(DecimalRaw(..))
import Data.Char(digitToInt)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.List.NonEmpty as NE

import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Literal
import Pact.Core.Builtin
import Pact.Core.Type(PrimType(..))
import Pact.Core.Guards
import Pact.Core.Imports
import Pact.Core.Errors
import Pact.Core.Syntax.Common
import Pact.Core.Syntax.New.ParseTree
import Pact.Core.Syntax.New.LexUtils


}
%name parseExpr Statement
%name parseModule Module
%name parseReplProgram ReplProgram
%name parseProgram Program

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
  import     { PosToken TokenImport _ }
  keygov     { PosToken TokenKeyGov _ }
  capgov     { PosToken TokenCapGov _ }
  defun      { PosToken TokenDefun _ }
  defcap     { PosToken TokenDefCap _ }
  defconst   { PosToken TokenDefConst _ }
  defschema  { PosToken TokenDefSchema _ }
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
  ':'        { PosToken TokenColon _ }
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



-- Programs will always end with a virtual semi
Program :: { [ParsedTopLevel] }
  : ProgramList MTerminator { reverse $1 }

ProgramList :: { [ParsedTopLevel] }
  : ProgramList SEMI TopLevel { $3:$1 }
  | TopLevel { [$1] }
  | {- empty -} { [] }

ReplProgram :: { [ParsedReplTopLevel] }
  : ReplProgramList MTerminator { reverse $1 }

ReplProgramList :: { [ParsedReplTopLevel] }
  : ReplProgramList SEMI ReplTopLevel { $3:$1 }
  | ReplTopLevel { [$1] }
  | {- empty -} { [] }

TopLevel :: { ParsedTopLevel }
  : Module { TLModule $1 }
  | Expr { TLTerm $1 }

ReplTopLevel :: { ParsedReplTopLevel }
  : Module { RTLModule $1 }
  | Defun { RTLDefun $1 }
  | DefConst { RTLDefConst $1 }
  | Expr { RTLTerm $1 }

MTerminator :: { () }
  : SEMI { () }
  | {- empty -} { () }

Module :: { ParsedModule }
  : module IDENT '(' Gov ')' ':' OPEN Exts Defs CLOSE
    { Module (ModuleName (getIdent $2) Nothing) $4 (reverse $8) (NE.fromList (reverse $9)) }

Gov :: { Governance Text }
  : keygov STR { Governance (Left (KeySetName (getStr $2))) }
  | capgov IDENT { Governance (Right (getIdent $2)) }

Exts :: { [ExtDecl] }
  : Exts SEMI Ext SEMI { $3:$1 }
  | Ext { [$1] }
  | {- empty -} { [] }

Ext :: { ExtDecl }
  : import ModQual ImportList { ExtImport (Import (mkModName $2) Nothing $3)  }
  | implements ModQual { ExtImplements (mkModName $2) }

Defs :: { [ParsedDef] }
  : Defs SEMI Def { $3:$1 }
  | Def { [$1] }

Def :: { ParsedDef }
  : Defun { Dfun $1 }
  | DefConst { DConst $1 }

ImportList :: { Maybe [Text] }
  : '(' CommaSepIdents ')' { Just (reverse $2) }
  | {- empty -} { Nothing }

CommaSepIdents :: { [Text] }
  : CommaSepIdents ',' IDENT { (getIdent $3):$1 }
  | IDENT { [(getIdent $1)] }

DefConst :: { ParsedDefConst }
  : defconst IDENT MTypeAnn '=' Expr { DefConst (getIdent $2) $3 $5 (_ptInfo $1) }

Defun :: { ParsedDefun }
  : defun IDENT '(' ArgList ')' '->' Type MaybeFnBlock
    { Defun (getIdent $2) (reverse $4) $7 $8 (combineSpan (_ptInfo $1) (view termInfo $8)) }

MaybeFnBlock :: { ParsedExpr }
  : ':' OPEN BlockBody CLOSE { Block (NE.fromList (reverse $3)) (combineSpan (_ptInfo $1) (_ptInfo $4)) }
  | '=' Expr { $2 }

ArgList :: { [Arg] }
  : ArgList ',' IDENT ':' Type { (Arg (getIdent $3) $5):$1 }
  | IDENT ':' Type { [(Arg (getIdent $1) $3)] }
  | {- empty -} { [] }

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
  | Defun  { NestedDefun $1 (_dfunInfo $1) }
  | Expr { $1 }

IfStmt :: { ParsedExpr }
  : if Expr then MaybeBlock MaybeSemi else MaybeBlock { If $2 $4 $7 (combineSpans $2 $7) }

MaybeSemi :: { () }
  : SEMI { () }
  | {- empty -} { () }

IfExpr :: { ParsedExpr }
  : if Expr then Expr else Expr { If $2 $4 $6 (combineSpans $2 $6) }

MaybeBlock :: { ParsedExpr }
  : ':' OPEN BlockBody CLOSE { Block (NE.fromList (reverse $3)) (combineSpan (_ptInfo $1) (_ptInfo $4)) }
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
  | IfExpr { $1 }
  | Expr { $1 }

LamExpr :: { ParsedExpr }
  : lam LamArgs ':' Expr { Lam ln0 (NE.fromList (reverse $2)) $4 (_ptInfo $1) }
  | lam ':' Expr { Lam ln0 (("#unitArg", Just TyUnit) :| []) $3 (_ptInfo $1) }

LamArgs :: { [(Text, Maybe Type)] }
  : LamArgs ',' '(' IDENT ':' Type ')' { (getIdent $4, Just $6):$1 }
  | LamArgs ',' IDENT { (getIdent $3, Nothing):$1 }
  | '(' IDENT ':' Type ')' { [(getIdent $2, Just $4)] }
  | IDENT { [(getIdent $1, Nothing)] }


Atom :: { ParsedExpr }
  : Name { $1 }
  | Number { $1 }
  | String { $1 }
  | Object { $1 }
  | List { $1 }
  | Bool { $1 }
  | '(' ')' { Constant LUnit (_ptInfo $1) }
  | '(' ParensExpr ')' { $2 }

Bool :: { ParsedExpr }
  : true { Constant (LBool True) (_ptInfo $1) }
  | false { Constant (LBool False) (_ptInfo $1) }

Name :: { ParsedExpr }
  : IDENT '.' ModQual  { mkQualName (getIdent $1) $3 (_ptInfo $1) }
  | IDENT { Var (mkBarename (getIdent $1)) (_ptInfo $1) }

ModQual :: { (Text, Maybe Text) }
  : IDENT '.' IDENT { (getIdent $1, Just (getIdent $3)) }
  | IDENT { (getIdent $1, Nothing) }

Number :: { ParsedExpr }
  : NUM '.' NUM {% mkDecimal (getNumber $1) (getNumber $3) (_ptInfo $1) }
  | NUM { mkIntegerConstant (getNumber $1) (_ptInfo $1) }

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

parseError (remaining, exp) =
  let rendered = renderTokenText . _ptToken <$> remaining
      expected = T.pack <$> exp
      li = maybe (LineInfo 0 0 1) _ptInfo (preview _head remaining)
  in throwError $ PEParseError $ (ParsingError rendered expected li)

mkIntegerConstant n i =
  let strToNum = T.foldl' (\x d -> 10*x + toInteger (digitToInt d))
  in Constant (LInteger (strToNum 0 n)) i

mkDecimal num dec i = do
  let strToNum = T.foldl' (\x d -> 10*x + toInteger (digitToInt d))
      prec = T.length dec
  when (prec > 255) $ throwError $ PEParseError $ PrecisionOverflowError prec i
  let out = Decimal (fromIntegral prec) (strToNum (strToNum 0 num) dec)
  pure $ Constant (LDecimal out) i

mkQualName ns (mod, (Just ident)) info =
  let ns' = NamespaceName ns
      qn = QualifiedName ident (ModuleName mod (Just ns'))
  in Var (QN qn) info
mkQualName mod (ident, Nothing) info =
  let qn = QualifiedName ident (ModuleName mod Nothing)
  in Var (QN qn) info

mkModName (ident, Nothing) = ModuleName ident Nothing
mkModName (ns, Just ident) = ModuleName ident (Just (NamespaceName ns))

ln0 = BN (BareName "")

mkBarename tx = BN (BareName tx)


}

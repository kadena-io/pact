{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pact.Core.Syntax.Lisp.Parser where

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
import Pact.Core.Syntax.Lisp.ParseTree
import Pact.Core.Syntax.Lisp.LexUtils


}
%name parseExpr Expr
%name parseModule Module
%name parseReplProgram ReplProgram
%name parseProgram Program

%tokentype { PosToken }
%monad { ParserT }

%errorhandlertype explist
%error { parseError }


%token
  let        { PosToken TokenLet _ }
  if         { PosToken TokenIf _ }
  lam        { PosToken TokenLambda _ }
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
  progn      { PosToken TokenBlockIntro _ }
  err        { PosToken TokenError _ }
  try        { PosToken TokenTry _ }
  suspend    { PosToken TokenSuspend _ }
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

%%



-- Programs will always end with a virtual semi
Program :: { [ParsedTopLevel] }
  : ProgramList { reverse $1 }

ProgramList :: { [ParsedTopLevel] }
  : ProgramList TopLevel { $2:$1 }
  | {- empty -} { [] }

ReplProgram :: { [ParsedReplTopLevel] }
  : ReplProgramList { reverse $1 }

ReplProgramList :: { [ParsedReplTopLevel] }
  : ReplProgramList ReplTopLevel { $2:$1 }
  | {- empty -} { [] }

TopLevel :: { ParsedTopLevel }
  : Module { TLModule $1 }
  | Expr { TLTerm $1 }

ReplTopLevel :: { ParsedReplTopLevel }
  : Module { RTLModule $1 }
  | '(' Defun ')' { RTLDefun ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | '(' DefConst ')' { RTLDefConst ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | Expr { RTLTerm $1 }

Module :: { ParsedModule }
  : '(' module IDENT Exts Defs ')'
    { Module (ModuleName (getIdent $3) Nothing) (reverse $4) (NE.fromList (reverse $5)) }

-- Module :: { ParsedModule }
--   : '(' module IDENT '(' Gov ')' Exts Defs ')'
--     { Module (ModuleName (getIdent $3) Nothing) $5 (reverse $7) (NE.fromList (reverse $8)) }

Gov :: { Governance Text }
  : keygov STR { Governance (Left (KeySetName (getStr $2))) }
  | capgov IDENT { Governance (Right (getIdent $2)) }

Exts :: { [ExtDecl] }
  : Exts Ext { $2:$1 }
  | {- empty -} { [] }

Ext :: { ExtDecl }
  : '(' import ModQual ImportList ')' { ExtImport (Import (mkModName $3) Nothing $4)  }
  | '(' implements ModQual ')' { ExtImplements (mkModName $3) }

Defs :: { [ParsedDef] }
  : Defs Def { $2:$1 }
  | Def { [$1] }

Def :: { ParsedDef }
  : '(' Defun ')' { Dfun ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | '(' DefConst ')' { DConst ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }

ImportList :: { Maybe [Text] }
  : '[' ImportNames ']' { Just (reverse $2) }
  | {- empty -} { Nothing }

ImportNames :: { [Text] }
  : ImportNames IDENT { (getIdent $2):$1 }
  | {- empty -} { [] }

DefConst :: { LineInfo -> ParsedDefConst }
  : defconst IDENT MTypeAnn Expr { DefConst (getIdent $2) $3 $4 }

-- ident = $2,
Defun :: { LineInfo -> ParsedDefun }
  : defun IDENT ':' Type '(' ArgList ')'  Block
    { Defun (getIdent $2) (reverse $6) $4 $8 }

ArgList :: { [Arg] }
  : ArgList IDENT ':' Type { (Arg (getIdent $2) $4):$1 }
  | {- empty -} { [] }

Type :: { Type }
  -- : '(' TyArrows '->' Type1 ')' { foldr TyFun $4 (reverse $2) }
  : Type1 { $1 }

Type1 :: { Type }
  : TYLIST Type { TyList $2 }
  -- | '{' RowType '}' { TyObject (Map.fromList $2) }
  | AtomicType { $1 }

-- TyArrows :: { [Type] }
--   : TyArrows '->' Type1 { $3:$1 }
--   | Type1 { [$1] }

AtomicType :: { Type }
  : PrimType { TyPrim $1 }

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

Block :: { ParsedExpr }
  : BlockBody { mkBlock (reverse $1)  }

BlockBody :: { [ParsedExpr] }
  : BlockBody Expr { $2:$1 }
  | Expr { [$1] }

Expr :: { ParsedExpr }
  : '(' SExpr ')' { $2 (combineSpan (_ptInfo $1) (_ptInfo $3)) }
  | Atom { $1 }

SExpr :: { LineInfo -> ParsedExpr }
  : LamExpr { $1 }
  | LetExpr { $1 }
  | IfExpr { $1 }
  | TryExpr { $1 }
  | ErrExpr { $1 }
  | ProgNExpr { $1 }
  | GenAppExpr { $1 }
  | SuspendExpr { $1 }

ExprCommaSep :: { [ParsedExpr] }
  : ExprCommaSep ',' Expr { $3:$1 }
  | Expr { [$1] }
  | {- empty -} { [] }

LamExpr :: { LineInfo -> ParsedExpr }
  : lam '(' LamArgs ')' Expr { Lam ln0 (reverse $3) $5 }

IfExpr :: { LineInfo -> ParsedExpr }
  : if Expr Expr Expr { If $2 $3 $4 }

TryExpr :: { LineInfo -> ParsedExpr }
  : try Expr Expr { Try $2 $3 }

SuspendExpr :: { LineInfo -> ParsedExpr }
  : suspend Expr { Suspend $2 }

ErrExpr :: { LineInfo -> ParsedExpr }
  : err STR { Error (getStr $2) }

LamArgs :: { [(Text, Maybe Type)] }
  : LamArgs IDENT ':' Type { (getIdent $2, Just $4):$1 }
  | LamArgs IDENT { (getIdent $2, Nothing):$1 }
  -- | IDENT ':' Type { [(getIdent $1, Just $3)] }
  -- | IDENT { [(getIdent $1, Nothing)] }
  | {- empty -} { [] }

LetExpr :: { LineInfo -> ParsedExpr }
  : let '(' Binders ')' Block { LetIn (NE.fromList (reverse $3)) $5 }

Binders :: { [Binder LineInfo] }
  : Binders '(' IDENT MTypeAnn Expr ')' { (Binder (getIdent $3) $4 $5):$1 }
  | '(' IDENT MTypeAnn Expr ')' { [Binder (getIdent $2) $3 $4] }

GenAppExpr :: { LineInfo -> ParsedExpr }
  : Expr AppList { App $1 (reverse $2) }

ProgNExpr :: { LineInfo -> ParsedExpr }
  : progn BlockBody { Block (NE.fromList (reverse $2)) }

AppList :: { [ParsedExpr] }
  : AppList Expr { $2:$1 }
  | {- empty -} { [] }

Atom :: { ParsedExpr }
  : Name { $1 }
  | Number { $1 }
  | String { $1 }
  | List { $1 }
  | Bool { $1 }
  | Operator { $1 }
  | '(' ')' { Constant LUnit (_ptInfo $1) }

Operator :: { ParsedExpr }
  : '&&' { Operator AndOp (_ptInfo $1) }
  | '||' { Operator OrOp (_ptInfo $1) }
  | '==' { Operator EQOp (_ptInfo $1) }
  | '!=' { Operator NEQOp (_ptInfo $1) }
  | '>'  { Operator GTOp (_ptInfo $1) }
  | '>=' { Operator GEQOp (_ptInfo $1) }
  | '<'  { Operator LTOp (_ptInfo $1) }
  | '<=' { Operator LEQOp (_ptInfo $1) }
  | '+'  { Operator AddOp (_ptInfo $1) }
  | '-'  { Operator SubOp (_ptInfo $1) }
  | '*'  { Operator MultOp (_ptInfo $1) }
  | '/'  { Operator DivOp (_ptInfo $1) }
  | '&'  { Operator BitAndOp (_ptInfo $1) }
  | '|'  { Operator BitOrOp (_ptInfo $1) }

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

-- Object :: { ParsedExpr }
--   : '{' ObjectBody '}' { Object $2 (_ptInfo $1) }

-- ObjectBody
--   : FieldPairs { Map.fromList $1 }

FieldPair :: { (Field, ParsedExpr) }
  : IDENT ':' Expr { (Field (getIdent $1), $3) }

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
getIdentField = Field . getIdent

mkIntegerConstant n i =
  let (n', f) = if T.head n == '-' then (T.drop 1 n, negate) else (n, id)
      strToNum = T.foldl' (\x d -> 10*x + toInteger (digitToInt d))
  in Constant (LInteger (f (strToNum 0 n'))) i

mkDecimal num dec i = do
  let (num', f) = if T.head num == '-' then (T.drop 1 num, negate) else (num, id)
      strToNum = T.foldl' (\x d -> 10*x + toInteger (digitToInt d))
      prec = T.length dec
  when (prec > 255) $ throwParseError (PrecisionOverflowError prec) i
  let out = Decimal (fromIntegral prec) (f (strToNum (strToNum 0 num') dec))
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

mkBlock = \case
  [x] -> x
  li -> let
    nel = NE.fromList li
    i = combineSpans (NE.head nel) (NE.last nel)
    in Block nel i

ln0 = BN (BareName "")

mkBarename tx = BN (BareName tx)


}

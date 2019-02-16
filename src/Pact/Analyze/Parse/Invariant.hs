{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Parser from 'Exp' to the 'Invariant' language.
module Pact.Analyze.Parse.Invariant (expToInvariant) where

import           Control.Applicative      ((<|>))
import           Control.Lens             ((^.))
import           Control.Monad.Reader     (ask)
import           Data.Foldable            (asum, find)
import qualified Data.Text                as T
import           Prelude                  hiding (exp)

import           Pact.Types.Lang          hiding (KeySet, KeySetName, SchemaVar,
                                           TableName, Type)
import qualified Pact.Types.Lang          as Pact
import           Pact.Types.Pretty

import           Pact.Analyze.Feature     hiding (Type, Var, ks, obj, str)
import           Pact.Analyze.Parse.Types
import           Pact.Analyze.Types
import           Pact.Analyze.Util

expToInvariant :: SingTy a -> Exp Info -> InvariantParse (Invariant a)
expToInvariant ty exp = case (ty, exp) of
  (_, EAtom' varName) -> do
    schemaTys <- ask
    case find (\(arg, _vid) -> arg ^. aName == varName) schemaTys of
      Just (Pact.Arg _name (TyPrim primTy) _info, vid) -> case (ty, primTy) of
        (SInteger, Pact.TyInteger) -> pure (CoreInvariant (Var vid varName))
        (SDecimal, Pact.TyDecimal) -> pure (CoreInvariant (Var vid varName))
        (STime,    Pact.TyTime)    -> pure (CoreInvariant (Var vid varName))
        (SStr,     Pact.TyString)  -> pure (CoreInvariant (Var vid varName))
        (SBool,    Pact.TyBool)    -> pure (CoreInvariant (Var vid varName))
        (SGuard,   Pact.TyGuard _) -> pure (CoreInvariant (Var vid varName))
        (_,        Pact.TyValue)   -> throwErrorIn exp
          "Invariants can't constrain opaque values"
        (_,        _)         -> throwErrorIn exp $
          "found variable " <> pretty varName <> " of type " <> pretty primTy <>
          " where " <> pretty ty <> " was expected"
      _ -> throwErrorT $ "couldn't find column named " <> varName

  (SDecimal, ELiteral' (LDecimal d)) -> pure (ILiteral (fromPact decimalIso d))
  (SInteger, ELiteral' (LInteger i)) -> pure (ILiteral i)
  (SStr,     ELiteral' (LString s))  -> pure (ILiteral (Str (T.unpack s)))
  (STime,    ELiteral' (LTime t))    -> pure (ILiteral (fromPact timeIso t))
  (SBool,    ELiteral' (LBool b))    -> pure (ILiteral b)
  (_,        ELiteral _)             ->
    throwErrorIn exp "literal of unexpected type"

  (SInteger, ParenList [EAtom' SStringLength, str])
    -> CoreInvariant . StrLength <$> expToInvariant SStr str
  (SStr, ParenList [EAtom' SConcatenation, a, b]) -> CoreInvariant ... StrConcat
    <$> expToInvariant SStr a <*> expToInvariant SStr b

  (SDecimal, ParenList [EAtom' (toOp arithOpP -> Just op), a, b]) -> asum
    [ Inj ... DecArithOp    op <$> expToInvariant SDecimal a <*> expToInvariant SDecimal b
    , Inj ... DecIntArithOp op <$> expToInvariant SDecimal a <*> expToInvariant SInteger b
    , Inj ... IntDecArithOp op <$> expToInvariant SInteger a <*> expToInvariant SDecimal b
    ] <|> throwErrorIn exp "unexpected argument types"
  (SInteger, ParenList [EAtom' (toOp arithOpP -> Just op), a, b])
    -> Inj ... IntArithOp op    <$> expToInvariant SInteger a <*> expToInvariant SInteger b
  (SDecimal, ParenList [EAtom' (toOp unaryArithOpP -> Just op), a])
    -> Inj . DecUnaryArithOp op <$> expToInvariant SDecimal a
  (SInteger, ParenList [EAtom' (toOp unaryArithOpP -> Just op), a])
    -> Inj . IntUnaryArithOp op <$> expToInvariant SInteger a

  (SBool, ParenList [EAtom' op'@(toOp comparisonOpP -> Just op), a, b]) -> asum
    [ CoreInvariant ... IntegerComparison op
      <$> expToInvariant SInteger a <*> expToInvariant SInteger b
    , CoreInvariant ... DecimalComparison op
      <$> expToInvariant SDecimal a <*> expToInvariant SDecimal b
    , CoreInvariant ... TimeComparison op
      <$> expToInvariant STime a    <*> expToInvariant STime b
    , CoreInvariant ... BoolComparison op
      <$> expToInvariant SBool a    <*> expToInvariant SBool b
    , CoreInvariant ... StrComparison op
      <$> expToInvariant SStr a     <*> expToInvariant SStr b
    , case toOp eqNeqP op' of
        -- enforce that users can only test {,in}equality of guards
        Just eqNeq -> CoreInvariant ... GuardEqNeq eqNeq
          <$> expToInvariant SGuard a
          <*> expToInvariant SGuard b
        Nothing -> throwErrorIn exp $
          pretty op' <> " is an invalid operation for keysets (only " <>
            pretty SEquality <> " or " <> pretty SInequality <> " allowed)"
    ] <|> throwErrorIn exp "unexpected argument types"

  (SBool, ParenList (EAtom' op:args))
    | Just op' <- toOp logicalOpP op -> do
    operands' <- traverse (expToInvariant SBool) args
    case (op', operands') of
      (AndOp, [a, b]) -> pure (ILogicalOp AndOp [a, b])
      (OrOp, [a, b])  -> pure (ILogicalOp OrOp [a, b])
      (NotOp, [a])    -> pure (ILogicalOp NotOp [a])
      _ -> throwErrorIn exp $
        "logical op with wrong number of args: " <> pretty op

  (_, EAtom {})        -> throwErrorIn exp "illegal invariant form"
  (_, Pact.EList {})   -> throwErrorIn exp "illegal invariant form"
  (_, BraceList {})    -> throwErrorIn exp "illegal invariant form"
  (_, ESeparator {})   -> throwErrorIn exp "illegal invariant form"

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Decimal
import Data.List
import Data.Time
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty

-- import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = undefined

basicExpr :: Property
basicExpr = undefined

data Schema = Schema [String]

data LispExpr
  = EStr String
  | EInt Integer
  | EDec Decimal
  | EBool Bool
  | ETime UTCTime
  | EKeyset
  | EList [LispExpr]
  | EObject Schema
  | ETable Schema
  | EModule
  | ESym String
  | EParens [LispExpr]

instance Show LispExpr where
  show = \case
    -- jww (2022-09-20): Need to escape 's'
    EStr s -> "\"" ++ s ++ "\""
    EInt i -> show i
    EDec d -> show d
    EBool True -> "true"
    EBool False -> "false"
    ETime _ -> undefined
    EKeyset -> undefined
    EList xs -> "[" ++ intercalate ", " (map show xs) ++ "]"
    EObject _ -> undefined
    ETable _ -> undefined
    EModule -> undefined
    ESym s -> s
    EParens xs -> "(" ++ intercalate " " (map show xs) ++ ")"

pickField :: Schema -> Gen String
pickField (Schema fields) =
  Gen.element fields

genSym :: Gen LispExpr
genSym =
  ESym
    <$> ( (:)
            <$> Gen.alpha
            <*> Gen.string (Range.linear 0 31) Gen.alphaNum
        )

genStr :: Gen LispExpr
genStr = EStr <$> Gen.string (Range.linear 0 8192) Gen.latin1

genInt :: Gen LispExpr
genInt =
  EInt
    <$> Gen.integral
      ( Range.linear
          (-1_000_000_000_000_000)
          1_000_000_000_000_000
      )

genDec :: Gen LispExpr
genDec =
  EDec
    <$> Gen.realFrac_
      ( Range.linearFrac
          (-1_000_000_000_000_000)
          1_000_000_000_000_000
      )

genBool :: Gen LispExpr
genBool = EBool <$> Gen.bool

genAtom :: (Gen LispExpr -> Gen a) -> Gen a
genAtom f =
  Gen.frequency
    [ (1, f genStr),
      (1, f genInt),
      (1, f genDec),
      (1, f genBool)
    ]

-- A functional expression has the form (FUNCTION ARGS..), while a data
-- expression has the form (EXPR...). A generated symbol should always refer
-- to a bound name, but value arguments can appear anywhere.
data ExprKind = Functional | Value

genExpr :: ExprKind -> Gen LispExpr
genExpr Value =
  Gen.frequency
    [ (4, genAtom id)
    ]
genExpr Functional = undefined

genList :: Int -> Gen LispExpr
genList n =
  -- 25% of the time generate a heterogenous list
  EList
    <$> Gen.frequency
      [ (4, genAtom (\g -> Gen.list (Range.linear 0 n) g)),
        (1, Gen.list (Range.linear 0 n) (genAtom id))
      ]

builtin_at :: LispExpr -> Gen LispExpr
builtin_at arg@(EList xs) = do
  i <- Gen.integral $ Range.linear 0 (genericLength xs)
  pure $ EParens [ESym "at", EInt i, arg]
builtin_at arg@(EObject sch) = do
  field <- pickField sch
  pure $ EParens [ESym "at", EStr field, arg]
builtin_at _ = mzero

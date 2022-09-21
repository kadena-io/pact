{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Decimal
import Data.Time
import Data.List
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

data Schema = Schema

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

builtin_at :: LispExpr -> Gen LispExpr
builtin_at arg@(EList xs) = do
  i <- Gen.integral $ Range.linear 0 (genericLength xs)
  pure $ EParens [ESym "at", EInt i, arg]
builtin_at _ = mzero

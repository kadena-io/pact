-- |
-- Module      :  Pact.Types.Memoize
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Memoization of function applications, and the machinery for proving
-- the precomputed results are accurate.
--
-- The memoization table allows certain function applications to
-- be replaced by a pre-computed result.
--
-- Insertion into the table can be done via the repl builtin:
-- `env-add-memo-entry`.


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Pact.Types.Memoize (

  -- * Use a `MemoTable` to short-cut application of a function.
  memoLookup,

  -- * Add an entry to the `MemoTable`.
  unguardedInsert,

  -- * Runtime representation of a memo table. Meant to be part of
  -- the Eval environment.
  MemoTable (MemoTable),

  FunctionCall(..)

) where

import Control.DeepSeq (NFData)
import Control.Monad.Catch (MonadThrow)
import Data.Default (Default, def)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)

import Pact.Types.Names (DefName(..))
import Pact.Types.PactValue (PactValue, toPactValue, fromPactValue)
import Pact.Types.Pretty (Pretty)
import Pact.Types.Term (App(App), Def(..), Term(TApp, TDef, TNative, _tNativeName, _tDef), Name, QualifiedName(..), NativeDefName(..), DefType(Defun))

-- A MemoTable maps function applications to their evaluation result.
-- The MemoTable is part of `EvalEnv`.
--
-- Pairs are validated on their way into the table, so we do not
-- need to check their validity again when the table is used for
-- substitution.
--
-- NOTE: It is stored as an association list rather than a hashmap
-- in order to avoid the need to Eq, Ord, Hashable constraints
-- on `App` and `Term`. We can revisit this.
--
-- TODO: Can we assume it's always `Term Name` in the table, and remove
-- our type parameter `n`?
data MemoTable = MemoTable {
  unMemoTable :: Map FunctionCall PactValue
  } deriving (Show, Generic)

instance NFData MemoTable
instance Default MemoTable where def = MemoTable mempty

-- Insert a pair of function application and result into the memotable.
-- Insertion is guarded by a `Witness` - a means of asserting that
-- the pair is valid. The `Witness` is evaluated in some monad,
-- (normally `Eval`).
unguardedInsert
  :: (MonadThrow m, Pretty n)
  => (Term n, Term Name)
  -> MemoTable
  -> m MemoTable
unguardedInsert (funApp, result) MemoTable { unMemoTable = table0 } =
  case termFunctionCall funApp of
    Left _e -> undefined
    Right fnCall -> do
      resultValue <- case toPactValue result of
        Left _e -> undefined
        Right x -> return x
      return $ MemoTable { unMemoTable =
                  Map.insert fnCall resultValue table0 }

termFunctionCall :: Pretty n =>  Term n -> Either String FunctionCall
termFunctionCall t = case t of
  TApp (App fn args _) _ -> do
    fcFunction <- case fn of
      TNative { _tNativeName } -> return $ Left _tNativeName
      TDef { _tDef = Def { _dDefName = DefName fnName, _dModule, _dDefType = Defun } } ->
        return $ Right $ QualifiedName { _qnName = fnName, _qnQual = _dModule, _qnInfo = def }
      _ -> undefined
    fcArgs <- case traverse toPactValue args of
      Left _e -> undefined
      Right xs -> return xs
    return $ FunctionCall { fcFunction, fcArgs }
  _ -> Left "TODO"


memoLookup :: Pretty n => Term n -> MemoTable -> Maybe (Term Name)
memoLookup key MemoTable {unMemoTable = table} = do
  case termFunctionCall key of
    Left _ -> Nothing
    Right fnCall -> fromPactValue <$> Map.lookup fnCall table

data FunctionCall = FunctionCall {
  fcFunction :: Either NativeDefName QualifiedName,
  fcArgs :: [PactValue]
} deriving (Eq, Show, Generic, Ord)

instance NFData FunctionCall

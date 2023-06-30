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

  FunctionCall(..),
  termFunctionCall,
  termFunctionCall',

) where

import Control.DeepSeq (NFData)
import Data.Default (Default, def)
import Data.Map (Map)
import Data.Traversable (forM)
import qualified Data.Map as Map
import GHC.Generics (Generic)

import Pact.Types.PactValue (PactValue, toPactValue)
-- import Pact.Types.Pretty (Pretty)
import Pact.Types.Term

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
  :: (FunctionCall, PactValue)
  -> MemoTable
  -> MemoTable
unguardedInsert (funCall, result) MemoTable { unMemoTable = table0 } =
      MemoTable { unMemoTable =
                  Map.insert funCall result table0 }


memoLookup :: FunctionCall -> MemoTable -> Maybe PactValue
memoLookup key MemoTable {unMemoTable = table} = do
  Map.lookup key table


newtype AnyFunctionName = AnyFunctionName {
  unAnyFunctionName :: Either NativeDefName QualifiedName
  } deriving (Eq, Ord, Show, Generic)

instance NFData AnyFunctionName

data FunctionCall = FunctionCall {
  fcFunction :: AnyFunctionName,
  fcArgs :: [PactValue]
} deriving (Eq, Show, Generic, Ord)

instance NFData FunctionCall


termFunctionCall :: App (Term Ref) -> Either String FunctionCall
termFunctionCall App { _appFun = fn, _appArgs = args } = do
    fcFunction <- case fn of
      TNative { _tNativeName } -> return $ Left _tNativeName
      -- TDef { _tDef = Def { _dDefName = DefName fnName, _dModule, _dDefType = Defun } } ->
      --   return $ Right $ QualifiedName { _qnName = fnName, _qnQual = _dModule, _qnInfo = def }
      TVar { _tVar } -> case _tVar of
        Direct (TNative { _tNativeName  }) -> return $ Left _tNativeName
        _ -> Left "TODO: Something other than DIRECT"
      l -> Left (show l)
    fcArgs <- case traverse toPactValue args of
      Left e -> Left ("args toPactValue error: " ++ show e)
      Right xs -> return xs
    return $ FunctionCall { fcFunction = AnyFunctionName fcFunction , fcArgs }


termFunctionCall' :: Monad m => (Term Ref -> m (Term Name)) -> App (Term Ref) -> m FunctionCall
termFunctionCall' eval App { _appFun = fn, _appArgs = args } = do
    let
    fcFunction <-
        case fn of
          TNative { _tNativeName } -> return $ AnyFunctionName $ Left _tNativeName
          TVar { _tVar } -> case _tVar of
            Direct (TNative { _tNativeName  }) -> return $ AnyFunctionName $ Left _tNativeName
            Ref (TDef { _tDef = Def { _dDefName = DefName fnName, _dModule, _dDefType = Defun } }) ->
              return $ AnyFunctionName $ Right $ QualifiedName { _qnQual = _dModule, _qnName = fnName, _qnInfo = def}
            _ -> error ("TVar: " ++ show e)
          l -> error $ "TODO " ++ show l -- return $ Left (show l)
    fcArgs <- forM args $ \arg -> do
      reducedArg <- eval arg
      case toPactValue reducedArg of
        Left e -> error $ "TODO: " ++ show e
        Right a -> return a
    return $ FunctionCall { fcFunction = fcFunction , fcArgs }

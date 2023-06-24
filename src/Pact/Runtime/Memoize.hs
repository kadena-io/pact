-- |
-- Module      :  Pact.Runtime.Memoize
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Memoization of function applications, and the machinery for proving
-- the precomputed results are accurate.
--
-- The memoization facility allows for a request to specify in its
-- metadata that certain function applications should evaluate to
-- a particular result. During runtime, the evaluator should judge
-- whether the evaluation hint is valid, and substitute the hint
-- for the function application if so.
--
-- Example witnesses to the hint's validity:
--   - ZK-SNARK proof.
--   - Trust-based assertion (used only for debugging, or in
--     specialized settings where nodes trust one another).
--   - Manual validation (useful when we want to compute a
--     subexpression once and share its result when possible).
--
-- Momoized substitutions are meant to be visible within the
-- scope of a single transaction. They do not live across
-- multiple requests, across chains, or across module updates.
-- TODO: Should they be visible across steps in a multi-step
-- defpact?
--
-- The data flow will look like this:
--
-- 1. A request to run a command will contain metadata in the
--    format `[MemoEntryWireFormat]`. Each entry states the function
--    and its arguments, the result, and a witness proving that
--    the function application evaluates to the result.
--
-- 2. The function `parseMemoTable` parses this metadata and validates
--    the proof witnesses, to produce a memo table.
--   2a. Each `MemoEntryWireFormat` contains a `WitnessWireFormat` field.
--       The witness is checked by whatever method is specified in that
--       entry's `WitnessType` field.
--   2b. When `WitnessType` is `ZK`, dispatch to the (to-be-written) ZK
--       proover.
--   2c. When `WitnessType` is `Rerun`, manually evaluate the function
--       application and verify that the result matches that given in
--       the `MemoEntryWireFormat`.
--   2d. When `Witnesstype` is `Trust`, simply accept the pair.
--
-- 3. A client of this API (usually the `Eval` monad), will store the
--    result of `parseMemoTable` in a Reader environment. Any time
--    the evaluator tries to reduce a function application, that client
--    should use `memoLookup` to see if the function application is
--    already present in the memo table, and if so, use it rather
--    than evaluating the application.



{-# LANGUAGE NamedFieldPuns #-}

module Pact.Runtime.Memoize (

  -- * Generate a `MemoTable` from a request.
  parseMemoTable,

  -- * Using a `MemoTable` to short-cut application of functions.
  memoLookup,

  -- * Runtime representation of a memo table. Meant to be part of
  -- the Eval environment.
  MemoTable (MemoTable),

  -- * The Public API for specifying memo table entries and
  -- witnesses.
  WitnessWireFormat(..),
  MemoEntryWireFormat(..),
  ZKProof(..),

  -- * Labels for witnesses. These are are used in the Public API,
  -- and may also be used for configuring which types of witnesses
  -- are allowed by the evaluator.
  WitnessType (..),


  -- * Internal (consider not exporting).
  Witness (..),
  guardedInsert,
  parseWitness,
) where

import Control.Monad (foldM)
import qualified Data.List as List
import GHC.Int (Int64)

import Pact.Types.Term (App, Term, termEq)

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
data MemoTable n = MemoTable {
  unMemoTable :: [((App (Term n)), (Term n))]
  }

-- Insert a pair of function application and result into the memotable.
-- Insertion is guarded by a `Witness` - a means of asserting that
-- the pair is valid. The `Witness` is evaluated in some monad,
-- (normally `Eval`).
guardedInsert
  :: Monad m
  => (App (Term n), Term n)
  -> Witness n m
  -> MemoTable n
  -> m (MemoTable n)
guardedInsert (funApp, result) validityWitness table0 = do
  isValid <- (runWitness validityWitness) (funApp, result)
  return $ if isValid
    then
      let table1 = (funApp,result) : (unMemoTable table0)
      in MemoTable { unMemoTable = table1 }
    else table0

memoLookup :: Eq n => App (Term n) -> MemoTable n -> Maybe (Term n)
memoLookup key MemoTable {unMemoTable = table} =
  List.lookup key table

data WitnessType = ZK | Trust | Rerun

data Witness n m  = Witness {
  witnessType :: WitnessType,
  -- ^ The type of the wintess. This is used by the runtime
  -- for filtering out the types of witnesses we allow.
  runWitness :: (App (Term n), Term n) -> m Bool
}

-- This type represents the serializable data attached to
-- a pact request witnessing the validity of a memo table
-- pair.
data WitnessWireFormat = WitnessWireFormat
  { wireWitnessType :: WitnessType
  , wireZKProof :: Maybe ZKProof
  }

-- TODO
data ZKProof = ZKProof {
  coefficients :: [Int64]
}

-- Interpret the request metadata into a runnable witness.
parseWitness
  :: (Monad m, Eq n)
  => (App (Term n) -> m (Term n))
  -> WitnessWireFormat
  -> Either String (Witness n m)
parseWitness runEval WitnessWireFormat { wireWitnessType = witnessType, wireZKProof } = do
  runWitness <- case witnessType of
    ZK -> case wireZKProof of
      Nothing -> Left "No ZKProof material provided"
      Just p -> return $ \(funApp, res) ->
        return $ runZKProof funApp res p
    Trust -> return $ \_ -> return True
    Rerun -> return $ \(funApp, res) -> do
      res' <- runEval funApp
      return $ res `termEq` res'
  return Witness { witnessType, runWitness }


runZKProof :: App (Term n) -> Term n -> ZKProof -> Bool
runZKProof = undefined

data MemoEntryWireFormat n = MemoEntryWireFormat {
  funApp :: App (Term n),
  result :: Term n,
  witness :: WitnessWireFormat
}

-- Interpret the metadata of a request to generate a `MemoTable`, checking the
-- witnesses in the metadata along the way.
parseMemoTable
  :: (Monad m, Eq n)
  => (App (Term n) -> m (Term n))
  -> [MemoEntryWireFormat n]
  -> m (MemoTable n)
parseMemoTable runEval memoEntries = do

  let table0 = MemoTable []
  entries <- foldM processEntry table0 memoEntries
  return $ entries

  where
    processEntry table MemoEntryWireFormat { funApp, result, witness } = do
      case parseWitness runEval witness of
        Left _e -> undefined
        Right w -> guardedInsert (funApp, result) w table

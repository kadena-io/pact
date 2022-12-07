module Pact.Core.Repl.Runtime.ReplBuiltin where

import Control.Monad.Except
import Data.Default
import Data.Text(Text)
import qualified Data.Text as T
-- import qualified Data.Text.Prettyprint.Doc as Pretty

import Pact.Core.Builtin
import Pact.Core.Literal
-- import Pact.Core.Errors

import Pact.Core.Untyped.Eval.Runtime
import Pact.Core.Untyped.Eval.CEK
import Pact.Core.Untyped.Eval.Runtime.CoreBuiltin

import Pact.Core.Repl.Runtime

type ReplCEKValue b i = CEKValue (ReplBuiltin b) i (ReplEvalM (ReplBuiltin b) i)
type ReplBuiltinFn b i = BuiltinFn (ReplBuiltin b) i (ReplEvalM (ReplBuiltin b) i)
type ReplBM b i = ReplEvalM (ReplBuiltin b) i

asBool :: MonadCEK b i m => CEKValue b i m -> m Bool
asBool (VLiteral (LBool b)) = pure b
asBool _ = failInvariant "asBool"

asString :: MonadCEK b i m => CEKValue b i m -> m Text
asString (VLiteral (LString b)) = pure b
asString _ = failInvariant "asString"

tryError :: MonadError a m => m b -> m (Either a b)
tryError ma = catchError (Right <$> ma) (pure . Left)

mkReplBuiltinFn
  :: (BuiltinArity b)
  => ([ReplCEKValue b i] -> ReplBM b i (ReplCEKValue b i))
  -> ReplBuiltin b
  -> ReplBuiltinFn b i
mkReplBuiltinFn fn b =
  BuiltinFn b fn (builtinArity b) []
{-# INLINE mkReplBuiltinFn #-}

corePrint :: (BuiltinArity b, Default i) => ReplBuiltin b -> ReplBuiltinFn b i
corePrint = mkReplBuiltinFn $ \case
  [showInst, v] -> do
    showed <- asString =<< unsafeApplyOne showInst v
    liftIO $ putStrLn $ T.unpack showed
    pure (VLiteral LUnit)
  _ -> failInvariant "Print"

coreExpect :: (BuiltinArity b, Default i) => ReplBuiltin b -> ReplBuiltinFn b i
coreExpect = mkReplBuiltinFn $ \case
  [eqFn, showFn, VLiteral (LString msg), v1, clo@VClosure{}] -> do
    v2 <- unsafeApplyOne clo (VLiteral LUnit)
    unsafeApplyTwo eqFn v1 v2 >>= asBool >>= \case
       False -> do
        v1s <- asString =<< unsafeApplyOne showFn v1
        v2s <- asString =<< unsafeApplyOne showFn v2
        pure $ VError $ "FAILURE: " <> msg <> " expected: " <> v1s <> ", received: " <> v2s
       True -> do
        pure (VLiteral (LString ("Expect: success " <> msg)))
  _ -> failInvariant "Expect"

coreExpectThat :: (BuiltinArity b, Default i) => ReplBuiltin b -> ReplBuiltinFn b i
coreExpectThat = mkReplBuiltinFn $ \case
  [VLiteral (LString msg), vclo, v] -> do
    unsafeApplyOne vclo v >>= asBool >>= \case
      True -> do
        pure (VLiteral (LString ("Expect-that: success " <> msg)))
      False -> do
        pure (VLiteral (LString ("FAILURE: Expect-that: Did not satisfy condition: " <> msg)))
  _ -> failInvariant "Expect"

coreExpectFailure :: (BuiltinArity b, Default i) => ReplBuiltin b -> ReplBuiltinFn b i
coreExpectFailure = mkReplBuiltinFn $ \case
  [VLiteral (LString toMatch), vclo] -> do
    tryError (unsafeApplyOne vclo (VLiteral LUnit)) >>= \case
      Right (VError _e) ->
        pure $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Left _err -> do
        pure $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Right _ ->
        pure $ VLiteral $ LString $ "FAILURE: " <> toMatch <> ": expected failure, got result"
  _ -> failInvariant "Expect-failure"



replCoreBuiltinRuntime
  :: (Default i)
  => ReplBuiltin CoreBuiltin
  -> ReplBuiltinFn CoreBuiltin i
replCoreBuiltinRuntime = \case
  RBuiltinWrap cb ->
    coreBuiltinLiftedRuntime RBuiltinWrap cb
  RExpect -> coreExpect RExpect
  RExpectFailure -> coreExpectFailure RExpectFailure
  RExpectThat -> coreExpectThat RExpectThat
  RPrint -> corePrint RPrint


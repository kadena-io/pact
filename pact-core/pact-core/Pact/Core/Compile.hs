{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}

module Pact.Core.Compile where


import qualified Data.Text.IO as T

import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.IR.Parse as IR
import qualified Pact.Core.IR.Desugar as IR
import qualified Pact.Core.IR.TCRemy as IR
import qualified Pact.Core.Typed.Overload as Typed
import qualified Pact.Core.Typed.Eval.CEK as Runtime
import qualified Pact.Core.Typed.Eval.Builtin as Runtime


_compileToTyped :: String -> IO ()
_compileToTyped file = do
  source <- T.readFile file
  let parsed = IR.parseExpr file source
  putStrLn "---- Parsed Term ----"
  prettyPrint parsed
  (desugared, sup) <- IR.runDesugarTerm parsed
  let (ty, typed) = IR.runInferTerm sup IR.rawBuiltinType desugared
  resolved <- Typed.resolveOverload typed
  putStrLn "---- Type ----"
  prettyPrint ty
  putStrLn "---- System F Term ---"
  prettyPrint resolved
  let ?cekLoaded = mempty
      ?cekBuiltins = Runtime.coreBuiltinRuntime
  let initState = Runtime.CEKState 0 Nothing
  value <- Runtime.runEvalT initState (Runtime.eval mempty resolved)
  print value
  where
  prettyPrint :: Pretty.Pretty a => a -> IO ()
  prettyPrint = putStrLn . show . Pretty.pretty

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}

module Pact.Core.Compile where


import Data.Text(Text)
import qualified Data.Text.IO as T

import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.IR.Parse as IR
import qualified Pact.Core.IR.Desugar as IR
import qualified Pact.Core.IR.TCRemy as IR
import qualified Pact.Core.IR.Typecheck as IRTC
import qualified Pact.Core.Typed.Overload as Typed
import qualified Pact.Core.Typed.Eval.CEK as Runtime
import qualified Pact.Core.Typed.Eval.Builtin as Runtime


_compileFile :: String -> IO ()
_compileFile file = T.readFile file >>= _compile file


_compile :: String -> Text -> IO ()
_compile file source = do
  let parsed = IR.parseExpr file source

  (desugared, sup) <- IR.runDesugarTerm parsed
  let (ty, typed) = IR.runInferTerm sup IR.rawBuiltinType desugared
  resolved <- Typed.resolveOverload typed
  let ?cekLoaded = mempty
      ?cekBuiltins = Runtime.coreBuiltinRuntime
  let initState = Runtime.CEKState 0 Nothing
  (value, _) <- Runtime.runEvalT initState (Runtime.eval mempty resolved)
  putStrLn "---- Parsed Term ----"
  prettyPrint parsed
  putStrLn "---- Type ----"
  prettyPrint ty
  putStrLn "---- System F Term ---"
  prettyPrint resolved
  putStrLn "----- Result -----"
  prettyPrint value
  where
  prettyPrint :: Pretty.Pretty a => a -> IO ()
  prettyPrint = putStrLn . show . Pretty.pretty

_compileFile2 :: String -> IO ()
_compileFile2 file = T.readFile file >>= _compile2 file


_compile2 :: String -> Text -> IO ()
_compile2 file source = do
  let parsed = IR.parseExpr file source

  (desugared, sup) <- IR.runDesugarTerm parsed
  let (ty, typed) = IRTC.runInferTerm sup IRTC.rawBuiltinType desugared
  prettyPrint typed
  print ty
  -- resolved <- Typed.resolveOverload typed
  -- let ?cekLoaded = mempty
  --     ?cekBuiltins = Runtime.coreBuiltinRuntime
  -- let initState = Runtime.CEKState 0 Nothing
  -- (value, _) <- Runtime.runEvalT initState (Runtime.eval mempty resolved)
  -- putStrLn "---- Parsed Term ----"
  -- prettyPrint parsed
  -- putStrLn "---- Type ----"
  -- prettyPrint ty
  -- putStrLn "---- System F Term ---"
  -- prettyPrint resolved
  -- putStrLn "----- Result -----"
  -- prettyPrint value
  where
  prettyPrint :: Pretty.Pretty a => a -> IO ()
  prettyPrint = putStrLn . show . Pretty.pretty

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}

module Pact.Core.Compile where


import qualified Data.ByteString as BL

import Pact.Core.Persistence

import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.IR.Desugar as IR
import qualified Pact.Core.IR.Typecheck as IR
import qualified Pact.Core.Typed.Overload as Typed
import qualified Pact.Core.Typed.Eval.CEK as Runtime
import qualified Pact.Core.Typed.Eval.Builtin as Runtime

import qualified Pact.Core.Syntax.New.LexUtils as NLex
import qualified Pact.Core.Syntax.New.Lexer as NLex
import qualified Pact.Core.Syntax.New.Parser as NParse

_compileFile :: String -> IO ()
_compileFile s = BL.readFile s >>= _compile

_compile :: BL.ByteString -> IO ()
_compile source = do
  lexx <- either error pure $ NLex.lexer source
  let loaded = Loaded mempty mempty mempty
  putStrLn "---- Lexer output -----"
  print (NLex._ptToken <$> lexx)
  parsed <- either error pure $ NParse.parseExpr lexx
  putStrLn "---- Parsed Term ----"
  prettyPrint parsed
  (desugared, sup, loaded') <- IR.runDesugarTerm loaded parsed
  let (ty, typed) = IR.runInferTerm sup loaded' IR.rawBuiltinType desugared
  prettyPrint typed
  resolved <- Typed.runOverloadTerm typed
  putStrLn "---- Type ----"
  prettyPrint ty
  putStrLn "---- System F Term Post typecheck---"
  prettyPrint typed
  putStrLn "---- System F Term Post overload---"
  prettyPrint resolved
  let ?cekLoaded = mempty
      ?cekBuiltins = Runtime.coreBuiltinRuntime
  let initState = Runtime.CEKState 0 Nothing
  (value, _) <- Runtime.runEvalT initState (Runtime.eval mempty (() <$ resolved))
  putStrLn "----- Result -----"
  prettyPrint value
  where
  prettyPrint :: Pretty.Pretty a => a -> IO ()
  prettyPrint = putStrLn . show . Pretty.pretty

_compileModule :: String -> IO ()
_compileModule file = do
  source <- BL.readFile file
  lexx <- either error pure $ NLex.lexer source
  putStrLn "---- Lexer output -----"
  print (NLex._ptToken <$> lexx)
  parsed <- either error pure $ NParse.parseModule lexx
  putStrLn "------------ Parsed Module -------------"
  print parsed

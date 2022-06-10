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
import qualified Data.ByteString.Lazy as BL

import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.IR.Parse as IR
import qualified Pact.Core.IR.Desugar as IR
import qualified Pact.Core.IR.Typecheck as IR
import qualified Pact.Core.IR.Lexer as Lex
import qualified Pact.Core.IR.LexUtils as Lex
import qualified Pact.Core.IR.ParseHappy as Parse
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
  prettyPrint typed
  resolved <- Typed.runOverload typed
  putStrLn "---- Parsed Term ----"
  prettyPrint parsed
  putStrLn "---- Type ----"
  prettyPrint ty
  putStrLn "---- System F Term Post typecheck---"
  prettyPrint typed
  putStrLn "---- System F Term Post overload---"
  prettyPrint resolved
  let ?cekLoaded = mempty
      ?cekBuiltins = Runtime.coreBuiltinRuntime
  let initState = Runtime.CEKState 0 Nothing
  (value, _) <- Runtime.runEvalT initState (Runtime.eval mempty resolved)
  putStrLn "----- Result -----"
  prettyPrint value
  where
  prettyPrint :: Pretty.Pretty a => a -> IO ()
  prettyPrint = putStrLn . show . Pretty.pretty

_compileFile2 :: String -> IO ()
_compileFile2 s = BL.readFile s >>= _compile2


_compile2 :: BL.ByteString -> IO ()
_compile2 source = do
  lexx <- either error pure $ Lex.lexer source
  putStrLn "---- Lexer output -----"
  print (Lex._ptToken <$> lexx)
  parsed <- either error pure $ Parse.parseExpr lexx
  putStrLn "---- Parsed Term ----"
  prettyPrint parsed
  (desugared, sup) <- IR.runDesugarTerm parsed
  putStrLn "----- Desugared term ----- "
  putStrLn (show desugared)
  let (ty, typed) = IR.runInferTerm sup IR.rawBuiltinType desugared
  prettyPrint typed
  resolved <- Typed.runOverload typed
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

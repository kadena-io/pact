{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}


module Pact.Core.Compile
 ( InterpretOutput(..)
 , interpretExpr
 , interpretProgram
 , interpretProgramFile
 ) where

import Control.Lens
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Catch
import Data.Text as Text
import Data.ByteString(ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString as B

import Pact.Core.Info
import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Repl.Utils
import Pact.Core.Typed.Term
import Pact.Core.IR.Desugar
import Pact.Core.IR.Typecheck
import Pact.Core.Typed.Eval.CEK
import Pact.Core.Typed.Overload

import qualified Pact.Core.Typed.Eval.CEK as Runtime
import qualified Pact.Core.Typed.Eval.Builtin as Runtime

import qualified Pact.Core.Syntax.New.Lexer as NLex
import qualified Pact.Core.Syntax.New.Parser as NParse
import qualified Pact.Core.Syntax.New.ParseTree as PT

data InterpretOutput b i
  = InterpretValue (CEKValue b i)
  | InterpretLog Text
  deriving Show

interpretExpr
  :: ByteString
  -> ReplT CoreBuiltin (CEKValue CoreBuiltin LineInfo)
interpretExpr source = do
  loaded <- use replLoaded
  p <- use replPactDb
  let ?pactDb = p
  lexx <- liftIO (NLex.runLexerIO source)
  debugIfFlagSet DebugLexer lexx
  parsed <- either throwM pure $ NParse.parseExpr lexx
  debugIfFlagSet DebugParser parsed
  DesugarOutput desugared sup loaded' _ <- liftIO (runDesugarTerm loaded parsed)
  let (ty, typed) = runInferTerm sup loaded' rawBuiltinType desugared
  debugIfFlagSet DebugTypecheckerType ty
  debugIfFlagSet DebugTypechecker typed
  resolved <- liftIO (runOverloadTerm typed)
  let ?cekLoaded = _loAllLoaded loaded'
      ?cekBuiltins = Runtime.coreBuiltinRuntime
  let initState = Runtime.CEKState 0 Nothing
  (value, _) <- liftIO (Runtime.runEvalT initState (Runtime.eval mempty resolved))
  replLoaded .= loaded'
  pure value

interpretProgramFile :: FilePath -> ReplT CoreBuiltin [InterpretOutput CoreBuiltin LineInfo]
interpretProgramFile source = liftIO (B.readFile source) >>= interpretProgram

interpretProgram :: ByteString -> ReplT CoreBuiltin [InterpretOutput CoreBuiltin LineInfo]
interpretProgram source = do
  lexx <- liftIO (NLex.runLexerIO source)
  debugIfFlagSet DebugLexer lexx
  parsed <- either throwM pure $ NParse.parseProgram lexx
  traverse interpretTopLevel parsed


-- todo: Clean up function 
interpretTopLevel
  :: PT.TopLevel ParsedName LineInfo
  -> ReplT CoreBuiltin (InterpretOutput CoreBuiltin LineInfo)
interpretTopLevel parsed = do
  loaded <- use replLoaded
  p <- use replPactDb
  let ?pactDb = p
  dout <- liftIO (runDesugarTopLevel loaded parsed)
  let DesugarOutput desugared supply loaded' deps = dout
  let !typechecked = runInferTopLevel supply loaded' rawBuiltinType desugared
  liftIO (runOverloadTopLevel typechecked) >>= \case
    TLModule m -> do
      let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded loaded)
          mdata = ModuleData m deps'
      liftIO (_writeModule p mdata)
      let out = "Loaded module " <> renderModuleName (_mName m)
          newLoaded = Map.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
          loaded'' =
            over loModules (Map.insert (_mName m) mdata) $
            over loAllLoaded (Map.union newLoaded) loaded'
      replLoaded .= loaded''
      pure (InterpretLog out)
      where
      -- Todo: remove this duplication
      -- this is a trick copied over from desugar
      rawDefName def = _nName (defName def)
      toFqDep modName mhash def = let
        fqn = FullyQualifiedName modName (rawDefName def) mhash
        in (fqn, defTerm def)

    TLTerm e -> do
      let ?cekLoaded = _loAllLoaded loaded'
          ?cekBuiltins = Runtime.coreBuiltinRuntime
      let initState = Runtime.CEKState 0 Nothing
      (value, _) <- liftIO (Runtime.runEvalT initState (Runtime.eval mempty e))
      replLoaded .= loaded'
      pure (InterpretValue value)
    TLInterface _ -> error "interfaces not yet supported"

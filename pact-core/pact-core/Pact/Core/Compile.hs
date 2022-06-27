{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Pact.Core.Compile
 ( InterpretOutput(..)
 , interpretExprNew
 , interpretProgramNew
 , interpretProgramFileNew
 , interpretExprLisp
 , interpretProgramLisp
 , interpretProgramFileLisp
 , newInterpretBundle
 , lispInterpretBundle
 , InterpretBundle(..)
 ) where

import Control.Lens
import Control.Monad((>=>))
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

import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Typed.Eval.CEK as Runtime
import qualified Pact.Core.Typed.Eval.Builtin as Runtime

import qualified Pact.Core.Syntax.New.Lexer as New
import qualified Pact.Core.Syntax.New.Parser as New

import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
import qualified Pact.Core.Syntax.Lisp.Parser as Lisp

data InterpretOutput b i
  = InterpretValue (CEKValue b i)
  | InterpretLog Text
  deriving Show

-- | Auxiliary type
-- to assist in swapping from the lisp frontend
data InterpretBundle
  = InterpretBundle
  { expr :: ByteString -> ReplT CoreBuiltin (CEKValue CoreBuiltin LineInfo)
  , program :: ByteString -> ReplT CoreBuiltin [InterpretOutput CoreBuiltin LineInfo]
  }

newInterpretBundle :: InterpretBundle
newInterpretBundle = InterpretBundle interpretExprNew interpretProgramNew

lispInterpretBundle :: InterpretBundle
lispInterpretBundle = InterpretBundle interpretExprLisp interpretProgramLisp

interpretExprNew :: ByteString -> ReplT CoreBuiltin (CEKValue CoreBuiltin LineInfo)
interpretExprNew source = do
  p <- use replPactDb
  loaded <- use replLoaded
  lexx <- liftIO (New.runLexerIO source)
  debugIfFlagSet DebugLexer lexx
  parsed <- either throwM pure $ New.parseExpr lexx
  debugIfFlagSet DebugParser parsed
  let ?pactDb = p
  desugared <- liftIO (runDesugarTermNew loaded parsed)
  interpretExpr desugared

interpretExprLisp :: ByteString -> ReplT CoreBuiltin (CEKValue CoreBuiltin LineInfo)
interpretExprLisp source = do
  p <- use replPactDb
  loaded <- use replLoaded
  lexx <- liftIO (Lisp.runLexerIO source)
  debugIfFlagSet DebugLexer lexx
  parsed <- either throwM pure $ Lisp.parseExpr lexx
  debugIfFlagSet DebugParser parsed
  let ?pactDb = p
  desugared <- liftIO (runDesugarTermLisp loaded parsed)
  interpretExpr desugared

interpretExpr
  :: DesugarOutput CoreBuiltin LineInfo (IR.Term IRName TypeVar RawBuiltin LineInfo)
  -> ReplT CoreBuiltin (CEKValue CoreBuiltin LineInfo)
interpretExpr (DesugarOutput desugared sup loaded' _) = do
  (ty, typed) <- liftIO (runInferTerm sup loaded' rawBuiltinType desugared)
  debugIfFlagSet DebugTypecheckerType ty
  debugIfFlagSet DebugTypechecker typed
  resolved <- liftIO (runOverloadTerm typed)
  let ?cekEnv = Runtime.RuntimeEnv
        (_loAllLoaded loaded')
        Runtime.coreBuiltinRuntime
        Map.empty
  let initState = Runtime.CEKState 0 Nothing
  (value, _) <- liftIO (Runtime.runEvalT initState (Runtime.eval mempty resolved))
  replLoaded .= loaded'
  pure value

interpretProgramFileNew :: FilePath -> ReplT CoreBuiltin [InterpretOutput CoreBuiltin LineInfo]
interpretProgramFileNew source = liftIO (B.readFile source) >>= interpretProgramNew

interpretProgramNew :: ByteString -> ReplT CoreBuiltin [InterpretOutput CoreBuiltin LineInfo]
interpretProgramNew source = do
  loaded <- use replLoaded
  p <- use replPactDb
  let ?pactDb = p
  lexx <- liftIO (New.runLexerIO source)
  debugIfFlagSet DebugLexer lexx
  parsed <- either throwM pure $ New.parseProgram lexx
  traverse (liftIO . runDesugarTopLevelNew loaded >=> interpretTopLevel) parsed

interpretProgramFileLisp :: FilePath -> ReplT CoreBuiltin [InterpretOutput CoreBuiltin LineInfo]
interpretProgramFileLisp source = liftIO (B.readFile source) >>= interpretProgramLisp

interpretProgramLisp :: ByteString -> ReplT CoreBuiltin [InterpretOutput CoreBuiltin LineInfo]
interpretProgramLisp source = do
  loaded <- use replLoaded
  p <- use replPactDb
  let ?pactDb = p
  lexx <- liftIO (Lisp.runLexerIO source)
  debugIfFlagSet DebugLexer lexx
  parsed <- either throwM pure $ Lisp.parseProgram lexx
  traverse (liftIO . runDesugarTopLevelLisp loaded >=> interpretTopLevel) parsed

-- todo: Clean up function
interpretTopLevel
  :: DesugarOutput CoreBuiltin LineInfo (IR.TopLevel IRName TypeVar RawBuiltin LineInfo)
  -> ReplT CoreBuiltin (InterpretOutput CoreBuiltin LineInfo)
interpretTopLevel (DesugarOutput desugared supply loaded deps) = do
  p <- use replPactDb
  typechecked <- liftIO (runInferTopLevel supply loaded rawBuiltinType desugared)
  liftIO (runOverloadTopLevel typechecked) >>= \case
    TLModule m -> do
      let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded loaded)
          mdata = ModuleData m deps'
      liftIO (_writeModule p mdata)
      let out = "Loaded module " <> renderModuleName (_mName m)
          newLoaded = Map.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
          loaded' =
            over loModules (Map.insert (_mName m) mdata) $
            over loAllLoaded (Map.union newLoaded) loaded
      replLoaded .= loaded'
      pure (InterpretLog out)
      where
      -- Todo: remove this duplication
      -- this is a trick copied over from desugar
      rawDefName def = _nName (defName def)
      toFqDep modName mhash def = let
        fqn = FullyQualifiedName modName (rawDefName def) mhash
        in (fqn, defTerm def)

    TLTerm e -> do
      let ?cekEnv = Runtime.RuntimeEnv
            (_loAllLoaded loaded)
            Runtime.coreBuiltinRuntime
            Map.empty
      let initState = Runtime.CEKState 0 Nothing
      (value, _) <- liftIO (Runtime.runEvalT initState (Runtime.eval mempty e))
      replLoaded .= loaded
      pure (InterpretValue value)
    TLInterface _ -> error "interfaces not yet supported"

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Pact.Core.Compile
 ( InterpretOutput(..)
--  , interpretExprNew
--  , interpretProgramNew
--  , interpretProgramFileNew
 , interpretExprLisp
 , interpretProgramLisp
 , interpretProgramFileLisp
--  , newInterpretBundle
 , lispInterpretBundle
--  , interpretExprTypeNew
 , interpretExprTypeLisp
 , InterpretBundle(..)
 ) where

import Control.Lens
-- import Control.Monad.Catch
import Control.Monad.Except
import Data.Text as Text
import Data.ByteString(ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString as B

import Pact.Core.Info
import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Gas
import Pact.Core.Names
import Pact.Core.Repl.Utils
import Pact.Core.Untyped.Term
import Pact.Core.Untyped.Utils
import Pact.Core.IR.Desugar
import Pact.Core.IR.Typecheck
import Pact.Core.Type
import Pact.Core.Typed.Overload

import Pact.Core.Untyped.Eval.Runtime
import Pact.Core.Repl.Runtime
import Pact.Core.Repl.Runtime.ReplBuiltin

import qualified Pact.Core.IR.Term as IR

import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
import qualified Pact.Core.Syntax.Lisp.Parser as Lisp

data InterpretOutput b i
  = InterpretValue (CEKValue b i (ReplEvalM ReplCoreBuiltin LineInfo))
  | InterpretLog Text
  deriving Show

-- | Auxiliary type
-- to assist in swapping from the lisp frontend
data InterpretBundle
  = InterpretBundle
  { expr :: ByteString -> ReplM ReplCoreBuiltin (ReplCEKValue CoreBuiltin LineInfo)
  , exprType :: ByteString -> ReplM ReplCoreBuiltin (TypeScheme NamedDeBruijn)
  , program :: ByteString -> ReplM ReplCoreBuiltin [InterpretOutput ReplCoreBuiltin LineInfo]
  }

-- newInterpretBundle :: InterpretBundle
-- newInterpretBundle =
--   InterpretBundle
--   { expr = interpretExprNew
--   -- , exprType = interpretExprTypeNew
--   , program = interpretProgramNew }

lispInterpretBundle :: InterpretBundle
lispInterpretBundle =
    InterpretBundle
  { expr = interpretExprLisp
  , exprType = interpretExprTypeLisp
  , program = interpretProgramLisp }

-- interpretExprNew :: ByteString -> ReplT CoreBuiltin (CEKValue CoreBuiltin LineInfo)
-- interpretExprNew source = do
--   pactdb <- use replPactDb
--   loaded <- use replLoaded
--   lexx <- liftIO (New.runLexerIO source)
--   debugIfFlagSet DebugLexer lexx
--   parsed <- either throwM pure $ New.parseExpr lexx
--   debugIfFlagSet DebugParser parsed
--   desugared <- liftIO (runDesugarTermNew pactdb loaded parsed)
--   interpretExpr desugared

interpretExprLisp
  :: ByteString
  -> ReplM (ReplBuiltin CoreBuiltin) (CEKValue (ReplBuiltin CoreBuiltin) LineInfo (ReplEvalM (ReplBuiltin CoreBuiltin) LineInfo))
interpretExprLisp source = do
  pactdb <- use replPactDb
  loaded <- use replLoaded
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet DebugLexer lexx
  parsed <- liftEither $ Lisp.parseExpr lexx
  debugIfFlagSet DebugParser parsed
  desugared <- runDesugarTermLisp pactdb loaded parsed
  interpretExpr desugared

interpretExpr
  :: DesugarOutput (ReplBuiltin CoreBuiltin) LineInfo (IR.Term Name (ReplBuiltin RawBuiltin) LineInfo)
  -> ReplM (ReplBuiltin CoreBuiltin) (ReplCEKValue CoreBuiltin LineInfo)
interpretExpr (DesugarOutput desugared loaded' _) = do
  (ty, typed) <- liftEither (runInferTerm loaded' (replBuiltinType rawBuiltinType) desugared)
  debugIfFlagSet DebugTypecheckerType ty
  debugIfFlagSet DebugTypechecker typed
  resolved <- liftEither (runOverloadTerm typed)
  debugIfFlagSet DebugSpecializer resolved
  let untyped = fromTypedTerm resolved
  debugIfFlagSet DebugUntyped untyped
  evalGas <- use replGas
  evalLog <- use replEvalLog
  let rEnv = ReplEvalEnv evalGas evalLog
      cekEnv = CEKRuntimeEnv
             { _cekBuiltins = replCoreBuiltinRuntime
             , _cekLoaded = _loAllLoaded loaded'
             , _cekGasModel = freeGasEnv }
      rState = ReplEvalState cekEnv
  value <- liftEither =<< liftIO (runReplCEK rEnv rState untyped)
  replLoaded .= loaded'
  pure value


-- interpretExpr
--   :: DesugarOutput CoreBuiltin LineInfo (IR.Term Name RawBuiltin LineInfo)
--   -> ReplT CoreBuiltin (CEKValue CoreBuiltin LineInfo)
-- interpretExpr (DesugarOutput desugared loaded' _) = do
--   let untyped = fromIRTerm desugared
--   debugIfFlagSet DebugUntyped untyped
--   (value, _) <- liftIO (Runtime.runCoreCEK (_loAllLoaded loaded') undefined untyped)
--   replLoaded .= loaded'
--   pure value

interpretExprTypeLisp :: ByteString -> ReplM (ReplBuiltin CoreBuiltin) (TypeScheme NamedDeBruijn)
interpretExprTypeLisp source = do
  pactdb <- use replPactDb
  loaded <- use replLoaded
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet DebugLexer lexx
  parsed <- liftEither $ Lisp.parseExpr lexx
  debugIfFlagSet DebugParser parsed
  desugared <- runDesugarTermLisp pactdb loaded parsed
  interpretExprType desugared

-- interpretExprTypeNew :: ByteString -> ReplT RawBuiltin (TypeScheme NamedDeBruijn)
-- interpretExprTypeNew source = do
--   pactdb <- use replPactDb
--   loaded <- use replLoaded
--   lexx <- liftIO (New.runLexerIO source)
--   debugIfFlagSet DebugLexer lexx
--   parsed <- either throwM pure $ New.parseExpr lexx
--   debugIfFlagSet DebugParser parsed
--   desugared <- liftIO (runDesugarTermNew pactdb loaded parsed)
--   interpretExprType desugared

interpretExprType
  :: DesugarOutput ReplCoreBuiltin LineInfo (IR.Term Name ReplRawBuiltin LineInfo)
  -> ReplM (ReplBuiltin CoreBuiltin) (TypeScheme NamedDeBruijn)
interpretExprType (DesugarOutput desugared loaded' _) = do
  (ty, typed) <- either (error . show) pure (runInferTerm loaded' (replBuiltinType rawBuiltinType) desugared)
  debugIfFlagSet DebugTypecheckerType ty
  debugIfFlagSet DebugTypechecker typed
  pure ty

-- interpretProgramFileNew :: FilePath -> ReplT RawBuiltin [InterpretOutput RawBuiltin LineInfo]
-- interpretProgramFileNew source = liftIO (B.readFile source) >>= interpretProgramNew

-- interpretProgramNew :: ByteString -> ReplT RawBuiltin [InterpretOutput RawBuiltin LineInfo]
-- interpretProgramNew source = do
--   loaded <- use replLoaded
--   pactdb <- use replPactDb
--   lexx <- liftIO (New.runLexerIO source)
--   debugIfFlagSet DebugLexer lexx
--   parsed <- either throwM pure $ New.parseProgram lexx
--   traverse (liftIO . runDesugarTopLevelNew pactdb loaded >=> interpretTopLevel) parsed

interpretProgramFileLisp :: FilePath -> ReplM ReplCoreBuiltin [InterpretOutput ReplCoreBuiltin LineInfo]
interpretProgramFileLisp source = liftIO (B.readFile source) >>= interpretProgramLisp

interpretProgramLisp :: ByteString -> ReplM ReplCoreBuiltin [InterpretOutput ReplCoreBuiltin LineInfo]
interpretProgramLisp source = do
  loaded <- use replLoaded
  pactdb <- use replPactDb
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet DebugLexer lexx
  parsed <- liftEither $ Lisp.parseProgram lexx
  let f a = runDesugarTopLevelLisp pactdb loaded a
  traverse (f >=> interpretTopLevel) parsed

-- todo: Clean up function
-- interpretTopLevel
--   :: DesugarOutput RawBuiltin LineInfo (IR.TopLevel Name RawBuiltin LineInfo)
--   -> ReplT RawBuiltin (InterpretOutput RawBuiltin LineInfo)
-- interpretTopLevel (DesugarOutput desugared loaded deps) = do
--   p <- use replPactDb
--   case fromIRTopLevel desugared of
--     TLModule m -> do
--       let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded loaded)
--           mdata = ModuleData m deps'
--       liftIO (_writeModule p mdata)
--       let out = "Loaded module " <> renderModuleName (_mName m)
--           newLoaded = Map.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
--           loaded' =
--             over loModules (Map.insert (_mName m) mdata) $
--             over loAllLoaded (Map.union newLoaded) loaded
--       replLoaded .= loaded'
--       pure (InterpretLog out)
--       where
--       -- Todo: remove this duplication
--       -- this is a trick copied over from desugar
--       toFqDep modName mhash def = let
--         fqn = FullyQualifiedName modName (defName def) mhash
--         in (fqn, def)

--     TLTerm resolved -> do
--       (value, _) <- liftIO (Runtime.runRawCEK (_loAllLoaded loaded) undefined resolved)
--       replLoaded .= loaded
--       pure (InterpretValue value)
--     TLInterface _ -> error "interfaces not yet supported"

interpretTopLevel
  :: DesugarOutput ReplCoreBuiltin LineInfo (IR.TopLevel Name ReplRawBuiltin LineInfo)
  -> ReplM ReplCoreBuiltin (InterpretOutput ReplCoreBuiltin LineInfo)
interpretTopLevel (DesugarOutput desugared loaded deps) = do
  p <- use replPactDb
  let btyp = replBuiltinType rawBuiltinType
  typechecked <- liftEither (runInferTopLevel loaded btyp desugared)
  overloaded <- liftEither (runOverloadTopLevel typechecked)
  case fromTypedTopLevel overloaded of
    TLModule m -> do
      let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded loaded)
          mdata = ModuleData m deps'
      _writeModule p mdata
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
      toFqDep modName mhash def = let
        fqn = FullyQualifiedName modName (defName def) mhash
        in (fqn, def)

    TLTerm resolved -> do
      evalGas <- use replGas
      evalLog <- use replEvalLog
      let rEnv = ReplEvalEnv evalGas evalLog
          cekEnv = CEKRuntimeEnv
                 { _cekBuiltins = replCoreBuiltinRuntime
                 , _cekLoaded = _loAllLoaded loaded
                 , _cekGasModel = freeGasEnv }
          rState = ReplEvalState cekEnv
      value <- liftEither =<< liftIO (runReplCEK rEnv rState resolved)
      replLoaded .= loaded
      pure (InterpretValue value)
    TLInterface _ -> error "interfaces not yet supported"

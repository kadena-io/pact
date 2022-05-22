{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}


module Pact.Core.IR.TypecheckImperative where

import Control.Lens
import Control.Monad.Reader
-- import Data.Text(Text)
import Data.Set(Set)
import Data.Map.Strict(Map)
import Data.IntMap.Strict(IntMap)

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap

import Pact.Core.Names
import Pact.Core.Builtin
import Pact.Core.Type
import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Typed.Term as Typed

type InferT a = ReaderT TCEnv IO a

data TCEnv
  = TCEnv
  { _tcLocalVarEnv :: IntMap (Type TypeName)
  , _tcTopLevelEnv :: Map QualifiedName (Type TypeName)
  , _tcNonGen :: Set TypeName  }

makeLenses ''TCEnv

lookupLocal :: DeBruijn -> InferT TypeScheme
lookupLocal (DeBruijn i) =
  views tcLocalVarEnv (IntMap.lookup i) >>= \case
    Just ts -> pure ts
    Nothing -> error "unbound local"

lookupQualified :: QualifiedName -> InferT TypeScheme
lookupQualified qn =
  views tcTopLeveLEnv (Map.lookup qn) >>= \case

unts :: TypeScheme -> Either (Type TypeName) TypeScheme
unts = \case
  TypeScheme [] [] ty -> Left ty
  r -> Right r

enforceType :: Either (Type TypeName) TypeScheme -> InferT (Type TypeName)
enforceType = either pure (error "expected type in position")

inferTerm :: IR.Term Name TypeName RawBuiltin i -> InferT (Typed.Term Name TypeName CoreBuiltin i, Either (Type TypeName) TypeScheme)
inferTerm = \case
  IR.Var n i -> case _nKind n of
    LocallyBoundName db -> do
      ts <- lookupLocal db
      pure (Typed.Var n i, unts ts)
    TopLevelName mn -> do
      ts <- lookupQualified (QualifiedName (_nName n) mn)
      pure (Typed.Var n i, unts ts)
  IR.

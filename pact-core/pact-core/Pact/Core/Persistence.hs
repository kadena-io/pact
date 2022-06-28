{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}


module Pact.Core.Persistence
 ( ModuleData(..)
 , mdModule
 , mdDependencies
 , PactDb(..)
 , HasPactDb
 , Loaded(..)
 , loModules
 , loToplevel
 , loAllLoaded
 , mockPactDb
 , emptyLoaded
 ) where

import Control.Lens
import Data.Text(Text)
import Data.IORef
import Data.Map.Strict(Map)

import Pact.Core.Names
import Pact.Core.Untyped.Term

import qualified Data.Map.Strict as Map

-- | Modules are they are stored
-- in our backend.
-- That is: All module definitions, as well as
data ModuleData b i
  = ModuleData
  { _mdModule :: EvalModule b i
  , _mdDependencies :: Map FullyQualifiedName (EvalTerm b i)
  } deriving Show

-- | Fun-record type for Pact back-ends.
data PactDb b i
  = PactDb
  { _readModule :: ModuleName -> IO (Maybe (ModuleData b i))
  , _writeModule :: ModuleData b i -> IO ()
  }

type HasPactDb b i = (?pactDb :: PactDb b i)

data Loaded b i
  = Loaded
  { _loModules :: Map ModuleName (ModuleData b i)
  , _loToplevel :: Map Text FullyQualifiedName
  , _loAllLoaded :: Map FullyQualifiedName (EvalTerm b i)
  } deriving Show

makeLenses ''ModuleData
makeLenses ''Loaded

emptyLoaded :: Loaded b i
emptyLoaded = Loaded mempty mempty mempty

mockPactDb :: IO (PactDb b i)
mockPactDb = do
  ref <- newIORef Map.empty
  pure (PactDb (readMod ref) (writeMod ref))
  where
  readMod ref mn = do
    m <- readIORef ref
    pure (Map.lookup mn m)
  writeMod ref md = let
    mname = _mName (_mdModule md)
    in modifyIORef' ref (Map.insert mname md)


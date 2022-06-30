{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


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
import Pact.Core.Guards

import qualified Data.Map.Strict as Map

-- | Modules as they are stored
-- in our backend.
-- That is: All module definitions, as well as
data ModuleData b i
  = ModuleData
  { _mdModule :: EvalModule b i
  , _mdDependencies :: Map FullyQualifiedName (EvalTerm b i)
  } deriving Show

type FQKS = KeySet FullyQualifiedName

data Purity
  -- | Read-only access to systables.
  = PSysOnly
  -- | Read-only access to systables and module tables.
  | PReadOnly
  -- | All database access allowed (normal).
  | PImpure
  deriving (Eq,Show,Ord,Bounded,Enum)

-- | Fun-record type for Pact back-ends.
data PactDb b i
  = PactDb
  { _purity :: !Purity
  , _readModule :: ModuleName -> IO (Maybe (ModuleData b i))
  -- ^ Look up module by module name
  , _writeModule :: ModuleData b i -> IO ()
  -- ^ Save a module
  , _readKeyset :: KeySetName -> IO (Maybe FQKS)
  -- ^ Read in a fully resolve keyset
  , _writeKeyset :: KeySetName -> FQKS -> IO ()
  -- ^ write in a keyset
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
  refMod <- newIORef Map.empty
  refKs <- newIORef Map.empty
  pure $ PactDb
    { _purity = PImpure
    , _readModule = readMod refMod
    , _writeModule = writeMod refMod
    , _readKeyset = readKS refKs
    , _writeKeyset = writeKS refKs
    }
  where
  readKS ref ksn = do
    m <- readIORef ref
    pure (Map.lookup ksn m)

  writeKS ref ksn ks = modifyIORef' ref (Map.insert ksn ks)

  readMod ref mn = do
    m <- readIORef ref
    pure (Map.lookup mn m)

  writeMod ref md = let
    mname = _mName (_mdModule md)
    in modifyIORef' ref (Map.insert mname md)


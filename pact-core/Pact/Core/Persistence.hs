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
import Control.Monad.IO.Class

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
  , _mdDependencies :: Map FullyQualifiedName (EvalDef b i)
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
data PactDb m b i
  = PactDb
  { _purity :: !Purity
  , _readModule :: ModuleName -> m (Maybe (ModuleData b i))
  -- ^ Look up module by module name
  , _writeModule :: ModuleData b i -> m ()
  -- ^ Save a module
  , _readKeyset :: KeySetName -> m (Maybe FQKS)
  -- ^ Read in a fully resolve keyset
  , _writeKeyset :: KeySetName -> FQKS -> m ()
  -- ^ write in a keyset
  }

data Loaded b i
  = Loaded
  { _loModules :: Map ModuleName (ModuleData b i)
  , _loToplevel :: Map Text FullyQualifiedName
  , _loAllLoaded :: Map FullyQualifiedName (EvalDef b i)
  } deriving Show

makeLenses ''ModuleData
makeLenses ''Loaded

emptyLoaded :: Loaded b i
emptyLoaded = Loaded mempty mempty mempty

mockPactDb :: (MonadIO m1, MonadIO m2) => m1 (PactDb m2 b i)
mockPactDb = do
  refMod <- liftIO $ newIORef Map.empty
  refKs <- liftIO $ newIORef Map.empty
  pure $ PactDb
    { _purity = PImpure
    , _readModule = liftIO . readMod refMod
    , _writeModule = liftIO . writeMod refMod
    , _readKeyset = liftIO . readKS refKs
    , _writeKeyset = \ksn  -> liftIO . writeKS refKs ksn
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


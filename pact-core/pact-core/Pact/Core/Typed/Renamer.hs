{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Typed.Renamer where

import Control.Lens
import Control.Monad.Reader
import Data.Text(Text)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Typed.Term

data RenamerState
  = RenamerState
  { _rnArgBinds :: Map Text DeBruijn
  , _rnTyBinds :: Map Unique DeBruijn
  , _rnArgDepth :: DeBruijn
  , _rnTyDepth :: DeBruijn
  } deriving Show

makeLenses ''RenamerState

type RenamerT m = ReaderT RenamerState m

lamNameToDebruijn :: IRName -> Name
lamNameToDebruijn (IRName n nk _) = Name n $ case nk of
  IRLocallyBoundName -> LocallyBoundName 0
  _ -> undefined

renameTerm :: Monad m => Term IRName TypeName b i -> RenamerT m (Term Name NamedDeBruijn b i)
renameTerm = \case
  Var n i -> case _irNameKind n of
    IRLocallyBoundName -> do
      depth <- view rnArgDepth
      views rnArgBinds (Map.lookup (_irName n)) >>= \case
        Just db -> do
          let n' = Name (_irName n) (LocallyBoundName (depth - db))
          pure (Var n' i)
        Nothing -> error "unbound local"
    _ -> error "todo: support tl names and module names"
  Lam n nts body i -> do
    depth <- view rnArgDepth
    let (ns, tys) = NE.unzip nts
        names = _irName <$> ns
        len = DeBruijn $ fromIntegral $ NE.length ns
        ixs = [depth .. depth + len]
        m = Map.fromList $ zip (NE.toList names) ixs
        n' = lamNameToDebruijn n
        ns' = NE.zipWith (\nn d -> Name nn (LocallyBoundName d)) names (NE.fromList ixs)
    tys' <- traverse renameType tys
    body' <- locally rnArgBinds (Map.union m) $ renameTerm body
    pure (Lam n' (NE.zip ns' tys') body' i)
  _ -> undefined

renameType :: Type TypeName -> RenamerT m (Type NamedDeBruijn)
renameType = undefined

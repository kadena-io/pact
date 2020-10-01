{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Pact.Runtime.Utils
-- Copyright   :  (C) 2020 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Runtime utilities: module lookup, stack introspection, events.
--

module Pact.Runtime.Utils
  ( lookupModule
  , searchCallStackApps
  , calledByModule
  , getModule
  , findCallingModule
  , getCallingModule
  , emitEvent
  ) where

import Control.Lens
import Control.Monad
import Data.Default
import qualified Data.HashMap.Strict as HM

import Pact.Gas
import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Pretty

-- | Lookup module in state or database with exact match on 'ModuleName'.
lookupModule :: HasInfo i => i -> ModuleName -> Eval e (Maybe (ModuleData Ref))
lookupModule i mn = do
  loaded <- preuse $ evalRefs . rsLoadedModules . ix mn
  case loaded of
    Just (m,_) -> return $ Just m
    Nothing -> do
      stored <- readRow (getInfo i) Modules mn
      case stored of
        Just mdStored -> do
          _ <- computeGas (Left ((getInfo i), "lookup module")) $ case (_mdModule mdStored) of
            MDModule m -> GPostRead (ReadModule (_mName m) (_mCode m))
            MDInterface int -> GPostRead (ReadInterface (_interfaceName int) (_interfaceCode int))
          natives <- view $ eeRefStore . rsNatives
          let natLookup (NativeDefName n) = case HM.lookup (Name (BareName n def)) natives of
                Just (Direct t) -> Just t
                _ -> Nothing
          case traverse (traverse (fromPersistDirect natLookup)) mdStored of
            Right md -> do
              evalRefs . rsLoadedModules %= HM.insert mn (md,False)
              return $ Just md
            Left e -> evalError' i $ "Internal error: module restore failed: " <> pretty e
        Nothing -> return Nothing


-- | Search up through call stack apps to find the first `Just a`
searchCallStackApps :: (FunApp -> Maybe a) -> Eval e (Maybe a)
searchCallStackApps f = uses evalCallStack $
  preview (traverse . sfApp . _Just . _1 . to f . _Just)

-- | See if some entity was called by a module
--
calledByModule :: Module n -> Eval e Bool
calledByModule Module{..} =
  maybe False (const True) <$> searchCallStackApps forModule
  where
    forModule :: FunApp -> Maybe ()
    forModule FunApp{..} | _faModule == Just _mName = Just ()
                         | otherwise = Nothing


-- | Lookup a module and fail if not found.
getModule :: HasInfo i => i -> ModuleName -> Eval e (ModuleData Ref)
getModule i mn = lookupModule i mn >>= \r -> case r of
  Just m -> return m
  Nothing -> evalError' i $ "Unable to resolve module " <> pretty mn

-- | Look up the name of the most current module in the stack
--
findCallingModule :: Eval e (Maybe ModuleName)
findCallingModule =
  preuse $ evalCallStack . traverse . sfApp . _Just . _1 . faModule . _Just


-- | Retrieve current calling module data or fail if not found
--
getCallingModule :: HasInfo i => i -> Eval e (Module (Def Ref))
getCallingModule i = maybe resolveErr ((=<<) isModule . getModule i) =<< findCallingModule
  where
    resolveErr = evalError' i
      "Unable to resolve current calling module"

    isModule md = case _mdModule md of
      MDModule m -> return m
      MDInterface n -> evalError' i
        $ "Internal error: getCallingModule: called from interface"
        <> pretty (_interfaceName n)


emitEvent :: HasInfo i => i -> QualifiedName -> [PactValue] -> Eval e ()
emitEvent i name params = whenExecutionFlagSet FlagSupportPactEvents $ do
  callMod <- getCallingModule i
  unless (_mName callMod == _qnQual name) $ evalError' i $
    "emitEvent: event '" <> pretty name <>
    "' does not match emitting module: " <> pretty (_mName callMod)
  evalEvents %= (PactEvent name params (_mHash callMod):)

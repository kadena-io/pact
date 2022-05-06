{-# LANGUAGE BangPatterns #-}

module Pact.Core.Imports
( Import(..)
)
where

import Data.Text(Text)
import Pact.Core.Names
import Pact.Core.Hash


data Import
  = Import
  { _impModuleName  :: ModuleName
  , _impModuleHash :: Maybe ModuleHash
  , _impImported :: Maybe [Text] }
  deriving Show

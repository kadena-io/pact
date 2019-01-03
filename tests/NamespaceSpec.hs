{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module NamespaceSpec (spec) where

import Test.Hspec

import Control.Lens

import Data.Default (def)
import Data.Data.Lens (biplate)

import Pact.Repl
import Pact.Repl.Types
import Pact.Typechecker (die)
import Pact.Types.Info (Info(..))
import Pact.Types.Runtime (RefStore(..), ModuleData(..), eeRefStore, rsModules, mdModule)
import Pact.Types.Term (Module(..), ModuleName(..))

spec :: Spec
spec = platedSpec

--

platedSpec :: Spec
platedSpec = describe "Namespaced modules" $ do
  testModulenames "test-module" "test.ns-test-module"
  testTermTree

testModuleNames :: ModuleName -> ModuleName -> Spec
testModuleNames mn mn' = do
  m  <- runIO $ loadModule' "tests/pact/namespaces.repl" mn
  m' <- runIO $ loadModule' "tests/pact/namespaces.repl" mn'
  compareModules (over biplate (mangleName mn') m) m'
  where
    mangleName :: ModuleName -> ModuleName -> ModuleName
    mangleName n = const n

    compareModules :: Module -> Module -> Spec
    compareModules (Module mn _ _ _ _ _ ifs imports) (Module mn' _ _ _ _ _ ifs' imports') = do
      mn `shouldBe` mn'
      ifs `shouldBe` ifs'
      imports `shouldBe` imports'
    compareModules (Interface ifn _ _) (Interface ifn' _ _) =
      ifn `shouldBe` ifn'
    compareModules _ _ = die def $ "Cannot compare incompatible module types"

testTermTree :: Spec
testTermTree = undefined

loadRefStore :: FilePath -> IO RefStore
loadRefStore fp = do
  (r,s) <- execScript' Quiet fp
  either (die def) (const (return ())) r
  case preview (rEnv . eeRefStore) s of
    Just md -> return md
    Nothing -> die def $ "Could not load module data from " ++ show fp

loadModuleData :: FilePath -> ModuleName -> IO ModuleData
loadModuleData fp mn = do
  rs <- loadRefStore fp
  case preview (rsModules . ix mn) rs of
    Just md -> pure md
    Nothing -> die def $ "Could not load module data: " ++ show mn

loadModuleData' :: RefStore -> ModuleName -> IO ModuleData
loadModuleData' rs mn = case preview (rsModules . ix mn) rs of
  Just md -> pure md
  Nothing -> die def $ "Could not load module data: " ++ show mn

loadModule :: FilePath -> ModuleName -> IO Module
loadModule fp mn = do
  md <- loadModuleData fp mn
  case preview mdModule md of
    Just m  -> pure m
    Nothing -> die def $ "Could not load module: " ++ show mn

loadModule' :: RefStore -> ModuleName -> IO Module
loadModule' rs mn = do
  md <- loadModuleData' rs mn
  case preview mdModule md of
    Just m  -> pure m
    Nothing -> die def $ "Could not load module: " ++ show mn

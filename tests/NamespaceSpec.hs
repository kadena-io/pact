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
import Pact.Types.Runtime (RefStore(..), ModuleData(..), eeRefStore, rsModules, mdModule)
import Pact.Types.Term (Module(..), ModuleName(..), Use(..))

spec :: Spec
spec = platedSpec

--

platedSpec :: Spec
platedSpec = describe "Namespaced modules" $ do
  testModuleNames "test-module" "test.ns-test-module"

testModuleNames :: ModuleName -> ModuleName -> Spec
testModuleNames mn mn' = do
  m  <- runIO $ loadModule "tests/pact/namespaces.repl" mn
  m' <- runIO $ loadModule "tests/pact/namespaces.repl" mn'
  -- establish the isomorphism using biplate
  compareModules (over biplate (const mn') m) m'
  compareModules (over biplate (const mn) m') m
  where
    compareModules :: Module -> Module -> Spec
    compareModules mm mm' =
      case (mm, mm') of
        (Module (ModuleName n _) _ _ _ _ _ ifs imports
          , Module (ModuleName n' _) _ _ _ _ _ _ _) -> do
          it "should have matching namespaced module names" $ do
            n `shouldBe` n'
          describe "should have namespaces propagate through constraints" $
            ifs `shouldBe` ifs'
          describe "should have namaespaces propagate through imports" $
            mapM_ (uncurry testImports) $ imports `zip` imports'
        (Interface ifn _ _ _, Interface ifn' _ _ _) -> do
          it "should have namespaces propagated through constraints" $ do
            ifn `shouldBe` ifn'
        _ -> runIO $ die def "Cannot compare incompatible module types"
      where
        testImports (Use n _ _) (Use n' _ _) = testModuleNames n n'

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

loadModule :: FilePath -> ModuleName -> IO Module
loadModule fp mn = do
  md <- loadModuleData fp mn
  case preview mdModule md of
    Just m  -> pure m
    Nothing -> die def $ "Could not load module: " ++ show mn

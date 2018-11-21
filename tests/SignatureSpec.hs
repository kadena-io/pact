{-# LANGUAGE OverloadedStrings #-}
module SignatureSpec (spec) where

import Test.Hspec

import Control.Lens (preview, ix)
import Data.Default (def)
import qualified Data.HashMap.Lazy as HM

import Pact.Repl
import Pact.Repl.Types
import Pact.Typechecker (die)
import Pact.Types.Exp
import Pact.Types.Info (Info(..))
import Pact.Types.Runtime (RefStore(..), ModuleData(..),
                           eeRefStore, rsModules)
import Pact.Types.Term (Module(..), ModuleName(..),
                        Meta(..), Term(..), Ref(..))


spec :: Spec
spec = compareModelSpec


loadRefStore :: FilePath -> IO RefStore
loadRefStore fp = do
  (r,s) <- execScript' Quiet fp
  either (die def) (const (return ())) r
  case preview (rEnv . eeRefStore) s of
    Just md -> return md
    Nothing -> die def $ "Could not load module data from " ++ show fp

loadModuleData :: RefStore -> ModuleName -> IO ModuleData
loadModuleData rs mn = case preview (rsModules . ix mn) rs of
  Just md -> pure md
  Nothing -> die def $ "Could not load module data: " ++ show mn

compareModelSpec :: Spec
compareModelSpec = describe "Module models" $ do
  rs  <- runIO $ loadRefStore "tests/pact/signatures.repl"
  md  <- moduleDataOf rs "model-test1-impl"
  ifd <- moduleDataOf rs "model-test1"

  let mModels = _mModel . _mMeta . _mdModule $ md
      iModels = _mModel . _interfaceMeta . _mdModule $ ifd

  it "should contain all toplevel models defined in their implemented interfaces" $
    (compareToplevelModels mModels iModels) `shouldBe` True

  it "should reflect all models defined for functions defined in interfaces" $
    (compareFunctionModels md ifd) `shouldBe` True

  where
    moduleDataOf r = runIO . loadModuleData r . ModuleName

compareToplevelModels :: [Exp Info] -> [Exp Info] -> Bool
compareToplevelModels mexps iexps =
  all (\e -> any (expEquality e) mexps) iexps

compareFunctionModels :: ModuleData -> ModuleData -> Bool
compareFunctionModels md ifd =
  HM.foldrWithKey (compareRefs (_mdRefMap md)) True (_mdRefMap ifd)
  where
    compareRefs _ _ (Direct _) _ = False
    compareRefs mm refName (Ref t) acc =
      case HM.lookup refName mm of
        -- Module does not implement an interface function. Error.
        Nothing -> False
        -- Direct refs are not supported
        Just (Direct _) -> False
        Just (Ref s) -> defunEquality t s && acc

-- Because models will necessarily have conflicting Info values
-- we need to define a new form of equality which forgets
-- 'Info', and only compares relevant terms.
expEquality :: Exp Info -> Exp Info -> Bool
expEquality e1 e2 = ((def :: Info) <$ e1) == ((def :: Info) <$ e2)

-- Show that all models for a given interface function reference
-- appear as a subset of the models of the corresponding module function
defunEquality :: Term Ref -> Term Ref -> Bool
defunEquality t s =
  case (t, s) of
    (TDef _ _ _ _ _ Meta{_mModel=mModel} _,
     TDef _ _ _ _ _ Meta{_mModel=iModel} _) ->
      all (\e -> any (expEquality e) mModel) iModel
    _ -> False

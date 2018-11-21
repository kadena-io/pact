{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module SignatureSpec (spec) where

import Test.Hspec

import Control.Monad (forM_)
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
  md  <- runIO $ loadModuleData rs (ModuleName "model-test1-impl")
  ifd <- runIO $ loadModuleData rs (ModuleName "model-test1")

  let mModels    = _mModel . _mMeta . _mdModule $ md
      iModels    = _mModel . _interfaceMeta . _mdModule $ ifd
      mfunModels = aggregateFunctionModels md
      ifunModels = aggregateFunctionModels ifd

  -- test toplevel models
  hasAllExps mModels iModels
  -- test function models
  hasAllExps mfunModels ifunModels

hasAllExps :: [Exp Info] -> [Exp Info] -> Spec
hasAllExps mexps iexps = forM_ iexps $ \e ->
  it "should find all exps defined in interface in corresponding module" $
    (e,mexps) `shouldSatisfy` (\_ -> any (expEquality e) mexps)

aggregateFunctionModels :: ModuleData -> [Exp Info]
aggregateFunctionModels ModuleData{..} =
  foldMap (extractExp . snd) $ HM.toList _mdRefMap
  where
    extractExp (Ref (TDef _ _ _ _ _ Meta{_mModel=mModel} _)) = mModel
    extractExp _ = []

-- Because models will necessarily have conflicting Info values
-- we need to define a new form of equality which forgets
-- 'Info', and only compares relevant terms.
expEquality :: Exp Info -> Exp Info -> Bool
expEquality e1 e2 = ((def :: Info) <$ e1) == ((def :: Info) <$ e2)

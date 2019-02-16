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
import Pact.Types.Term (Module(..), Interface(..), ModuleName(..), ModuleDef(..),
                        Meta(..), Term(..), Ref(..), Def(..))


spec :: Spec
spec = compareModelSpec

compareModelSpec :: Spec
compareModelSpec = describe "Module models" $ do
  rs  <- runIO $ loadRefStore "tests/pact/signatures.repl"
  md  <- runIO $ loadModuleData rs (ModuleName "model-test1-impl" Nothing)
  ifd <- runIO $ loadModuleData rs (ModuleName "model-test1" Nothing)

  let mModels = case _mdModule md of
        MDModule m -> _mModel $ _mMeta m
        _ -> def
      iModels = case _mdModule ifd of
        MDInterface i -> _mModel $ _interfaceMeta i
        _ -> def
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
    extractExp (Ref (TDef (Def _ _ _ _ _ Meta{_mModel=mModel} _) _)) = mModel
    extractExp _ = []

-- Because models will necessarily have conflicting Info values
-- we need to define a new form of equality which forgets
-- 'Info', and only compares relevant terms.
expEquality :: Exp Info -> Exp Info -> Bool
expEquality e1 e2 = ((def :: Info) <$ e1) == ((def :: Info) <$ e2)

loadRefStore :: FilePath -> IO RefStore
loadRefStore fp = do
  (r,s) <- execScript' Quiet fp
  either (die def) (const (return ())) r
  case preview (rEnv . eeRefStore) s of
    Just md -> return md
    Nothing -> die def $ "Could not load module data from " ++ show fp

loadModuleData :: RefStore -> ModuleName -> IO ModuleData
loadModuleData rs mn =
  case preview (rsModules . ix mn) rs of
    Just md -> pure md
    Nothing -> die def $ "Could not load module data: " ++ show mn

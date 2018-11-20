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
import Pact.Types.Runtime (RefStore(..), ModuleData(..), eeRefStore,
                           rsModules, asString')
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
  md  <- moduleDataOf rs "model-test1"
  ifd <- moduleDataOf rs "model-test1-impl"

  let mModels = modelsOf md
      iModels = modelsOf ifd
      mName   = nameOf md

  it "should contain all toplevel models defined in their implemented interfaces" $
    expectSuccess mName (compareToplevel mModels iModels)

  it "should reflect all models defined for functions defined in interfaces" $
    expectSuccess mName (compareFunRefs md ifd)

  where
    moduleDataOf r = runIO . loadModuleData r . ModuleName
    modelsOf = _mModel . _mMeta . _mdModule
    nameOf = _mName . _mdModule
    compareToplevel mexps = all $ \e -> any (expEquality e) mexps


compareFunRefs :: ModuleData -> ModuleData -> Bool
compareFunRefs md ifd =
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


expectSuccess :: ModuleName -> Bool -> Expectation
expectSuccess mn b = if b
  then expectationFailure $ "Model consistency failed: " ++ asString' mn
  else pure ()

-- Because models will necessarily have conflicting Info values
-- we need to define a new form of equality which forgets
-- 'Info', and only compares relevant terms.
expEquality :: Exp Info -> Exp Info -> Bool
expEquality e1 e2 =
  case (e1, e2) of
    (ELiteral (LiteralExp l _), ELiteral (LiteralExp l' _)) -> l == l'
    (EAtom (AtomExp a qs _), EAtom (AtomExp a' qs' _)) -> a == a' && qs == qs'
    (EList (ListExp l d _), EList (ListExp l' d' _)) ->
      length l == length l' && all (uncurry expEquality) (l `zip` l') && d == d'
    (ESeparator (SeparatorExp s _), ESeparator (SeparatorExp s' _)) -> s == s'
    _ -> False

-- Show that all models for a given interface function reference
-- appear as a subset of the models of the corresponding module function
defunEquality :: Term Ref -> Term Ref -> Bool
defunEquality t s =
  case (t, s) of
    (TDef _ _ _ _ _ Meta{_mModel=mModel} _,
     TDef _ _ _ _ _ Meta{_mModel=iModel} _) ->
      all (\e -> any (expEquality e) mModel) iModel
    _ -> False

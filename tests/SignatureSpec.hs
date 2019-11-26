{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SignatureSpec (spec) where

import Test.Hspec

import Control.Error.Util (failWith, hoistEither)
import Control.Monad (forM_, void)
import Control.Monad.Trans.Except
import Data.Bifunctor (first)
import Data.Default (def)
import qualified Data.HashMap.Strict as HM
import Data.Set

import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Exp
import Pact.Types.Runtime


spec :: Spec
spec = compareModelSpec

compareModelSpec :: Spec
compareModelSpec = describe "Module models" $ do
  (r,s) <- runIO $ execScript' Quiet "tests/pact/signatures.repl"

  eres <- runIO . runExceptT $ do
    void $ hoistEither r
    (rs,_) <- ExceptT . fmap (first show) $ replGetModules s
    md <- failWith "Map lookup failed" $ HM.lookup (ModuleName "model-test1-impl" Nothing) rs
    ifd <- failWith "Map lookup failed" $ HM.lookup (ModuleName "model-test1" Nothing) rs
    pure (md, ifd)

  case eres of
    Left e -> it "script loading + lookups" $ expectationFailure e
    Right (md, ifd) -> do
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
      -- test function modules
      hasAllExps mfunModels ifunModels

hasAllExps :: Set (Exp Info) -> Set (Exp Info) -> Spec
hasAllExps mexps iexps = forM_ iexps $ \e ->
  it "should find all exps defined in interface in corresponding module" $
    (e,mexps) `shouldSatisfy` (\_ -> any (expEquality e) mexps)

aggregateFunctionModels :: ModuleData Ref -> Set (Exp Info)
aggregateFunctionModels ModuleData{..} =
  foldMap (extractExp . snd) $ HM.toList _mdRefMap
  where
    extractExp (Ref (TDef (Def _ _ _ _ _ Meta{_mModel=m} _ _) _)) = m
    extractExp _ = mempty

-- Because models will necessarily have conflicting Info values
-- we need to define a new form of equality which forgets
-- 'Info', and only compares relevant terms.
expEquality :: Exp Info -> Exp Info -> Bool
expEquality e1 e2 = ((def :: Info) <$ e1) == ((def :: Info) <$ e2)

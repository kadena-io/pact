{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module SignatureSpec (spec) where

import Test.Hspec

import Control.Monad (forM_)
import Data.Default (def)
import qualified Data.HashMap.Strict as HM


import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Exp
import Pact.Types.Info (Info(..))
import Pact.Types.Runtime
import Pact.Types.Term (Module(..), Interface(..), ModuleName(..), ModuleDef(..),
                        Meta(..), Term(..), Ref'(..), Ref, Def(..))


spec :: Spec
spec = compareModelSpec

compareModelSpec :: Spec
compareModelSpec = describe "Module models" $ do
  (r,s) <- runIO $ execScript' Quiet "tests/pact/signatures.repl"
  case r of
    Left e -> it "loaded script" $ expectationFailure e
    Right _ -> return ()
  Right (rs,_) <- runIO $ replGetModules s
  Just md <- return $ HM.lookup (ModuleName "model-test1-impl" Nothing) rs
  Just ifd <- return $ HM.lookup (ModuleName "model-test1" Nothing) rs

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

aggregateFunctionModels :: ModuleData Ref -> [Exp Info]
aggregateFunctionModels ModuleData{..} =
  foldMap (extractExp . snd) $ HM.toList _mdRefMap
  where
    extractExp (Ref (TDef (Def _ _ _ _ _ Meta{_mModel=m} _) _)) = m
    extractExp _ = []

-- Because models will necessarily have conflicting Info values
-- we need to define a new form of equality which forgets
-- 'Info', and only compares relevant terms.
expEquality :: Exp Info -> Exp Info -> Bool
expEquality e1 e2 = ((def :: Info) <$ e1) == ((def :: Info) <$ e2)

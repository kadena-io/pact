{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module SignatureSpec (spec) where

import Test.Hspec

import Pact.Repl
import Pact.Repl.Types
import Pact.Typechecker (die)
import Pact.Types.Exp
import Pact.Types.Info (Info(..))
import Pact.Types.Runtime (RefStore(..), eeRefStore,
                           rsModules, mdModule, asString')
import Pact.Types.Term (Module(..), ModuleName(..),
                        Meta(..))

import Control.Lens (preview, at, _Just)
import Control.Monad (forM)

import Data.Default (def)
import Data.Foldable (traverse_)


spec :: Spec
spec = compareModelSpec

loadRefStore :: FilePath -> IO RefStore
loadRefStore fp = do
  (r,s) <- execScript' Quiet fp
  either (die def) (const (return ())) r
  case preview (rEnv . eeRefStore) s of
    Just md -> return md
    Nothing -> die def $ "Could not load module data from " ++ show fp

loadModules :: RefStore -> [ModuleName] -> IO [Module]
loadModules rs mns = forM mns $ \mn -> case preview (rsModules . at mn . _Just . mdModule) rs of
  Just m -> return m
  Nothing -> die def $ "Module not found: " ++ show mn

compareModelSpec :: Spec
compareModelSpec = describe "pact modules should inherit models from interfaces" $ do
  rs <- runIO $ loadRefStore "tests/pact/signatures.repl"
  ms <- runIO $ loadModules rs ["model-test1-impl"]
  traverse_ (compareModels rs) ms
  where
    compareModels rs' Module{..} = do
      ifs <- runIO $ loadModules rs' _mInterfaces
      let mModels  = _mModel _mMeta
          ifModels = ifs >>= (_mModel . _interfaceMeta)
          ms' = traverse (\e -> if any (expEquality e) mModels then (Just e) else Nothing) ifModels
      it (asString' _mName ++ " should inherit all of its models") $ expectJust _mName ms'
    compareModels _ _ = pure ()

expectJust :: ModuleName -> Maybe [a] -> Expectation
expectJust mn = \case
  Nothing -> expectationFailure $ "Model mismatch in " ++ show mn
  Just _  -> pure ()

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

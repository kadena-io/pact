{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Pact.Types.Term.Arbitrary
(
) where

import Bound

import Data.Default

import Test.QuickCheck

-- internal modules

import Pact.Types.Hash
import Pact.Types.Term
import Pact.Types.Util

-- -------------------------------------------------------------------------- --
-- Arbitrary Orphans for Scope

instance Arbitrary (f (Var b (f a))) => Arbitrary (Scope b f a) where
  arbitrary = Scope <$> arbitrary

instance (Arbitrary b, Arbitrary a) => Arbitrary (Var b a) where
  arbitrary = oneof [B <$> arbitrary, F <$> arbitrary]

-- -------------------------------------------------------------------------- --
--

instance Arbitrary Meta where
    arbitrary = Meta <$> arbitrary <*> arbitrary

instance Arbitrary PactId where
  arbitrary = PactId . hashToText <$> arbitrary

instance (Arbitrary a) => Arbitrary (UserGuard a) where
  arbitrary = UserGuard <$> arbitraryName (0,1,1,0) <*> arbitrary

instance Arbitrary DefType where
  arbitrary = elements [Defun, Defpact, Defcap]

instance Arbitrary Gas where
  arbitrary = Gas <$> arbitrary

instance Arbitrary n => Arbitrary (BindType n) where
  arbitrary = oneof [ pure BindLet, BindSchema <$> arbitrary ]

instance Arbitrary n => Arbitrary (BindPair n) where
  arbitrary = BindPair <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (App t) where
  arbitrary = App <$> arbitrary <*> arbitrary <*> arbitrary

instance {-# OVERLAPPABLE #-} Arbitrary g => Arbitrary (Governance g) where
  arbitrary = Governance <$> arbitrary

instance {-# OVERLAPPING #-} Arbitrary (Governance Name) where
  arbitrary = Governance <$> oneof
    [ Left <$> arbitrary
    , Right <$> arbitraryName (0,1,1,0)
    ]

instance Arbitrary ModuleHash where
  -- Coin contract is about 20K characters
  arbitrary = ModuleHash <$> scale (min 20000) arbitrary

instance Arbitrary n => Arbitrary (DefcapMeta n) where
  arbitrary = oneof
    [ DefcapManaged <$> arbitrary
    , pure DefcapEvent
    ]

instance Arbitrary n => Arbitrary (DefMeta n) where
  arbitrary = DMDefcap <$> arbitrary

instance Arbitrary n => Arbitrary (ConstVal n) where
  arbitrary = oneof
    [ CVRaw <$> arbitrary
    , CVEval <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Example where
  arbitrary = oneof
    [ ExecExample <$> arbitrary
    , ExecErrExample <$> arbitrary
    , LitExample <$> arbitrary
    ]

instance Arbitrary FieldKey where
  arbitrary = scale (min 50) (FieldKey <$> genBareText)

instance Arbitrary n => Arbitrary (Step n) where
  arbitrary = Step <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ModRef where
  arbitrary = ModRef <$> arbitrary <*> scale (min 10) arbitrary <*> pure def

instance Arbitrary ModuleGuard where
  arbitrary = ModuleGuard <$> arbitrary <*> genBareText

instance Arbitrary PactGuard where
  arbitrary = PactGuard <$> arbitrary <*> genBareText

instance (Arbitrary v) => Arbitrary (ObjectMap v) where
  -- arbitrary = ObjectMap . M.fromList <$> listOf1 arbitrary
  arbitrary = ObjectMap <$> scale (min 10) arbitrary

instance Arbitrary Use where
  arbitrary = Use <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (CapabilityGuard a) where
  arbitrary = CapabilityGuard <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Guard a) where
  arbitrary = oneof
    [ GPact <$> arbitrary
    , GKeySet <$> arbitrary
    , GKeySetRef <$> arbitrary
    , GModule <$> arbitrary
    , GUser <$> arbitrary
    , GCapability <$> arbitrary
    ]

instance {-# OVERLAPPING #-} Arbitrary (Module Name) where
  arbitrary = Module
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> scale (min 10) arbitrary
    <*> scale (min 10) arbitrary

instance Arbitrary g => Arbitrary (Module g) where
  arbitrary = Module
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> scale (min 10) arbitrary
    <*> scale (min 10) arbitrary

instance Arbitrary Interface where
  arbitrary = Interface
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> scale (min 10) arbitrary

instance {-# OVERLAPPING #-} Arbitrary (ModuleDef Name) where
  arbitrary = oneof [MDModule <$> arbitrary, MDInterface <$> arbitrary]

instance Arbitrary g => Arbitrary (ModuleDef g) where
  arbitrary = oneof [MDModule <$> arbitrary, MDInterface <$> arbitrary]

-- -------------------------------------------------------------------------- --
-- Arbitrary Instances for Types that have a cyclic dependeny with Term

instance Arbitrary FunApp where
  arbitrary = FunApp
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary d => Arbitrary (Ref' d) where
  arbitrary = oneof [Direct <$> arbitrary, Ref <$> arbitrary]

instance Arbitrary NativeDFun where
  arbitrary = do
    n <- arbitrary
    g <- arbitrary
    t <- arbitrary
    return $ NativeDFun n $ \_ _ -> return (g,t)

instance Arbitrary n => Arbitrary (Def n) where
  arbitrary = Def
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary n => Arbitrary (Lam n) where
  arbitrary = Lam <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary n => Arbitrary (Object n) where
  arbitrary = Object
    <$> arbitrary
    <*> arbitrary
    <*> scale (min 10) arbitrary
    <*> arbitrary

instance Arbitrary n => Arbitrary (Term n) where
  arbitrary = sized $ \case
    0 -> oneof
      [ TLiteral <$> arbitrary <*> arbitrary
      , TVar <$> arbitrary <*> arbitrary
      , (\o -> TUse o (_uInfo o)) <$> arbitrary
      ]
    s -> do
      Positive k <- arbitrary
      resize (s `div` (k + 1)) $ oneof
        [ TModule <$> arbitrary <*> arbitrary <*> arbitrary
        , TList <$> arbitrary <*> arbitrary <*> arbitrary
        , (\o -> TDef o (_dInfo o)) <$> arbitrary

        -- TNative intentionally not marshallable
        -- , TNative <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

        , TConst <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , (\o -> TApp o (_appInfo o)) <$> arbitrary
        , TVar <$> arbitrary <*> arbitrary
        , TBinding <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , (\o -> TObject o (_oInfo o)) <$> arbitrary
        , TLiteral <$> arbitrary <*> arbitrary
        , (\o -> TLam o (_lamInfo o)) <$> arbitrary
        , TGuard <$> arbitrary <*> arbitrary
        , (\o -> TUse o (_uInfo o)) <$> arbitrary
        , TStep <$> arbitrary <*> arbitrary <*> arbitrary
        , TSchema <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , TTable <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , TDynamic <$> arbitrary <*> arbitrary <*> arbitrary
        , (\o -> TModRef o (_modRefInfo o)) <$> arbitrary
        ]


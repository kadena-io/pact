{-# LANGUAGE OverloadedStrings #-}
module PrincipalSpec
( spec
) where


import Data.Attoparsec.Text
import Data.Default (def)
import Data.Text

import Pact.Types.Principal
import Pact.Types.Info

import Test.Hspec

spec :: Spec
spec = do
  describe "k:" kSpec
  describe "w:" wSpec
  describe "r:" rSpec
  describe "m:" mSpec
  describe "u:" uSpec
  describe "p:" pSpec

-- | Default info is sufficient for this spec
--
i :: Info
i = def

kSpec :: Spec
kSpec = do
  it "k: parser roundtrips" $ do
    -- principal -> text
    pk `shouldBe` Right k'
    -- text -> principal
    mkPrincipalIdent k' `shouldBe` k

  it "k: has correct identifiers" $
    -- principal -> correct id
    fmap showPrincipalType pk `shouldBe` Right (showPrincipalType k')
  where
    k = "k:584deb6f81d8efe67767309d1732019cf6ad14f9f0007cff50c730ef62521c68"
    k' = K "584deb6f81d8efe67767309d1732019cf6ad14f9f0007cff50c730ef62521c68"
    pk = parseOnly (principalParser i) k

wSpec :: Spec
wSpec = do
  it "w: parser roundtrips" $ do
    pw `shouldBe` Right w'
    mkPrincipalIdent w' `shouldBe` w

  it "w: has correct identifiers" $
    fmap showPrincipalType pw `shouldBe` Right (showPrincipalType w')
  where
    w = "w:5PhRgNM3oePrkfAKhk9dYmjRqOhEEhbR2eyFz8HU_ew:keys-all"
    w' = W "5PhRgNM3oePrkfAKhk9dYmjRqOhEEhbR2eyFz8HU_ew" "keys-all"
    pw = parseOnly (principalParser i) w

rSpec :: Spec
rSpec = do
  it "r: parser roundtrips" $ do
    pr `shouldBe` Right r'
    mkPrincipalIdent r' `shouldBe` r

  it "r: has correct identifiers" $
    fmap showPrincipalType pr `shouldBe` Right (showPrincipalType r')
  where
    r = "r:ks"
    r' = R "ks"
    pr = parseOnly (principalParser i) r

mSpec :: Spec
mSpec = do
  it "m: parser roundtrips" $ do
    pm `shouldBe` Right m'
    mkPrincipalIdent m' `shouldBe` m

  it "m: has correct identifiers" $
    fmap showPrincipalType pm `shouldBe` Right (showPrincipalType m')
  where
    m = "m:test-ns.tester:tester"
    m' = M "test-ns.tester" "tester"
    pm = parseOnly (principalParser i) m

uSpec :: Spec
uSpec = do
  it "u: parser roundtrips" $ do
    pu `shouldBe` Right u'
    mkPrincipalIdent u' `shouldBe` u

  it "u: has correct identifiers" $
    fmap showPrincipalType pu `shouldBe` Right (showPrincipalType u')
  where
    u :: Text
    u = "u:test-ns.tester.both-guard:aqukm-5Jj6ITLeQfhNYydmtDccinqdJylD9CMlLKQDI"
    u' = U "test-ns.tester.both-guard" "aqukm-5Jj6ITLeQfhNYydmtDccinqdJylD9CMlLKQDI"
    pu = parseOnly (principalParser i) u

pSpec :: Spec
pSpec = do
  it "p: parser roundtrips" $ do
    pp `shouldBe` Right p'
    mkPrincipalIdent p' `shouldBe` p

  it "p: has correct identifiers" $
    fmap showPrincipalType pp `shouldBe` Right (showPrincipalType p')
  where
    p = "p:DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g:pact-guard"
    p' = P "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g" "pact-guard"
    pp = parseOnly (principalParser i) p

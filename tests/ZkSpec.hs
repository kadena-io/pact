{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZkSpec (spec) where

-- import Control.Monad (void)
import qualified Data.Curve.Weierstrass as C
import Data.Field.Galois as F hiding (pow)
import Data.Group (pow)
import Data.Pairing.BN254 hiding (pairing, Fq2, Fq12, Fq, G1, G2)
import Data.Pairing.BN254 qualified as Pairing
import Data.Field(Field)
-- import qualified Data.Pairing.Ate as Ate
-- import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pact.Native.Pairing
import Test.Hspec
import Test.Hspec.Hedgehog

-- y^2 = x^3 + b
isOnCurve :: (Num a, Eq a) => CurvePoint a -> a -> Bool
isOnCurve CurveInf _ = True
isOnCurve (Point x y) b =
  ((y ^ (2 :: Int)) - (x ^ (3 :: Int))) == b

pairingP1 :: Pairing.G1 BN254
pairingP1 =
  C.A
    0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd3
    0x15ed738c0e0a7c92e7845f96b2ae9c0a68a6a449e3538fc7ff3ebf7a5a18a2c4

p1 :: CurvePoint Fq
p1 = multiply g1 2

pairingP2 :: Pairing.G2 BN254
pairingP2 =
  C.A
    ( [ 0x6064e784db10e9051e52826e192715e8d7e478cb09a5e0012defa0694fbc7f5,
        0x1014772f57bb9742735191cd5dcfe4ebbc04156b6878a0a7c9824f32ffb66e85
      ]
    )
    ( [ 0x58e1d5681b5b9e0074b0f9c8d2c68a069b920d74521e79765036d57666c5597,
        0x21e2335f3354bb7922ffcc2f38d3323dd9453ac49b55441452aeaca147711b2
      ]
    )

p2 :: CurvePoint Fq2
p2 = multiply g2 3

pairingGtPt:: GT BN254
pairingGtPt =
  toU'
    [ [ [ 0x10227b2606c11f22f4b2dec3f69cee4332ebe2e8f869ea8ca9e6d45ce15bd110,
          0x27d1c9dae835182b272bb25b47b0d871382c9c2765fd1f42e07edbe852830157
        ],
        [ 0x1f5919cf59b218135aaeb137ac84c6ecf282feda6a8752ca291b7ec1d2f8bab4,
          0x2b7e44680d35a6676223538d54abcd7bc2c54281bf0f5277c81cf5b114d3a345
        ],
        [ 0x17e6d213292c2aa12ef3cc75aca8cb9cbd47d05086227db2dbd1262d3e89dbf0,
          0x291a53fea204b470bb901fb184155facd6e3b44fad848d536386b73d6c31fd52
        ]
      ],
      [ [ 0x2844ed362ecf2c491a471a18c2875fd727126a62c8151c356f81e02cff52f045,
          0x2a8245d55a3b3f9deae9cca372912a31b88dc77cee06dfa10a717acbf758cbd5
        ],
        [ 0x222ff2e20c4578e886027953a035cbd8784a9764bbcd353051ba9f02c4dce8ad,
          0x8532a0a75fb0acdf508c3bdd4c7700efb3a9ae403818daad5937d9ffffaca45
        ],
        [ 0x2e7e3a4aaef17a53de3c528319b426e35f53455107f49d7fe52de95849e7dcf6,
          0x2ba2bc83434031012424aad830a35c459c40a0b7ce87735010db68c10b61ddcb
        ]
      ]
    ]

pt' :: Fq12
pt' =
    [ [ [ 0x10227b2606c11f22f4b2dec3f69cee4332ebe2e8f869ea8ca9e6d45ce15bd110,
          0x27d1c9dae835182b272bb25b47b0d871382c9c2765fd1f42e07edbe852830157
        ],
        [ 0x1f5919cf59b218135aaeb137ac84c6ecf282feda6a8752ca291b7ec1d2f8bab4,
          0x2b7e44680d35a6676223538d54abcd7bc2c54281bf0f5277c81cf5b114d3a345
        ],
        [ 0x17e6d213292c2aa12ef3cc75aca8cb9cbd47d05086227db2dbd1262d3e89dbf0,
          0x291a53fea204b470bb901fb184155facd6e3b44fad848d536386b73d6c31fd52
        ]
      ],
      [ [ 0x2844ed362ecf2c491a471a18c2875fd727126a62c8151c356f81e02cff52f045,
          0x2a8245d55a3b3f9deae9cca372912a31b88dc77cee06dfa10a717acbf758cbd5
        ],
        [ 0x222ff2e20c4578e886027953a035cbd8784a9764bbcd353051ba9f02c4dce8ad,
          0x8532a0a75fb0acdf508c3bdd4c7700efb3a9ae403818daad5937d9ffffaca45
        ],
        [ 0x2e7e3a4aaef17a53de3c528319b426e35f53455107f49d7fe52de95849e7dcf6,
          0x2ba2bc83434031012424aad830a35c459c40a0b7ce87735010db68c10b61ddcb
        ]
      ]
    ]

fieldModulus :: Integer
fieldModulus = 21888242871839275222246405745257275088696311157297823662689037894645226208583

curveOrder :: Integer
curveOrder = 21888242871839275222246405745257275088548364400416034343698204186575808495617


genCurvePoint
  :: (Field a, Eq a, Num a)
  => CurvePoint a -- Generator
  -> Gen (CurvePoint a)
genCurvePoint gen' = do
  r <- Gen.integral $ Range.constant 0 (curveOrder - 1)
  pure (multiply gen' r)

genG1 :: Gen G1
genG1 = genCurvePoint g1

genG2 :: Gen G2
genG2 = genCurvePoint g2

pairingGenTest :: Spec
pairingGenTest = modifyMaxSuccess (const 20) $
  describe "Curve generated tests" $ do
    it "Generates a point on the curve, and obeys the pairing function" $ hedgehog $ do
      p1' <- forAll genG1
      assert (isOnCurve g1 b1)

      p2' <- forAll genG2
      assert (isOnCurve g2 b2)

      r1 <- forAll $ Gen.integral (Range.constant 0 1000)
      r2 <- forAll $ Gen.integral (Range.constant 0 1000)
      pairing (multiply p1' r1)  (multiply p2' r2) === pow (pairing p1' p2') (r1 * r2)

-- Tests from:
-- https://github.com/ethereum/py_pairing/blob/master/tests/test_bn128.py
pairingLibTest :: Spec
pairingLibTest =
  describe "pairing lib tests" $ do
    it "passes basic field arithmetic for Fq" $ do
      Fq 2 * Fq 2 `shouldBe` Fq 4
      Fq 2 / Fq 7 + Fq 9 / Fq 7 `shouldBe` Fq 11 / Fq 7
      Fq 2 * Fq 7 + Fq 9 * Fq 7 `shouldBe` Fq 11 * Fq 7
      Fq 9 ^ fieldModulus `shouldBe` Fq 9
    it "passes basic field arithmetic for Fq2" $ do
      let x :: Fq2 = [1, 0]
      let f :: Fq2 = [1, 2]
      let fpx :: Fq2 = [2, 2]
      x + f `shouldBe` fpx
      f / f `shouldBe` 1
      1 / f + x / f `shouldBe` (1 + x) / f
      1 * f + x * f `shouldBe` (1 + x) * f
      x ^ (fieldModulus ^ (2 :: Int) - 1) `shouldBe` 1
    it "passes basic field arithmetic for FQ12" $ do
      let x :: Fq12 = [1]
          f :: Fq12 = [[[1, 2], [3, 4], [5, 6]], [[7, 8], [9, 10], [11, 12]]]
          fpx :: Fq12 = [[[2, 2], [3, 4], [5, 6]], [[7, 8], [9, 10], [11, 12]]]
      x + f `shouldBe` fpx
      f / f `shouldBe` 1
      1 / f + x / f `shouldBe` (1 + x) / f
      1 * f + x * f `shouldBe` (1 + x) * f
      x ^ (fieldModulus ^ (2 :: Int) - 1) `shouldBe` 1
    it "passes basic elliptic curve operations for G1" $ do
      add (add (double g1) g1) g1 `shouldBe` double (double g1)
      double g1 `shouldNotBe` g1
      add (multiply g1 9) (multiply g1 5) `shouldBe` add (multiply g1 12) (multiply g1 2)
      multiply g1 curveOrder `shouldBe` CurveInf
    it "passes basic elliptic curve operations for G2" $ do
      add (add (double g2) g2) g2 `shouldBe` double (double g2)
      double g2 `shouldNotBe` g2
      add (multiply g2 9) (multiply g2 5) `shouldBe` add (multiply g2 12) (multiply g2 2)
      multiply g2 curveOrder `shouldBe` CurveInf
      multiply g2 (2 * fieldModulus - curveOrder) `shouldNotBe` CurveInf
      isOnCurve (multiply g2 9) b2 `shouldBe` True
    it "passes basic pairing tests" $ do
      -- Pairing operation on negated g1
      let pp1 = pairing g1 g2
      let pn1 = pairing (negatePt g1) g2
      pp1 * pn1 `shouldBe` 1

      -- Pairing op negated in g2
      let np1 = pairing g1 (negatePt g2)
      pp1 * np1 `shouldBe` 1
      np1 `shouldBe` pn1

      -- Pairing output has correct order
      pp1 ^ curveOrder `shouldBe` 1

      -- Pairing bilinearity in g1
      let pp2 = pairing (multiply g1 2) g2
      pp1 * pp1 `shouldBe` pp2

      -- Pairing is non-degenerate
      (pp1 /= pp2) && (pp1 /= np1) && (pp2 /= np1) `shouldBe` True

      -- Pairing bilinearity in G2
      let po2 = pairing g1 (multiply g2 2)
      pp1 * pp1 `shouldBe` po2

      -- Composite check
      let p3 = pairing (multiply g1 37) (multiply g2 27)
      let po3 = pairing (multiply g1 999) g2
      p3 `shouldBe` po3




spec :: Spec
spec = do
  pairingLibTest
  pairingGenTest
  describe "pairing tests" $ do
    it "pairing lib smoke test" $ do
      let a :: Integer = 2
      let b :: Integer = 3

      let p :: Pairing.G1 BN254 = pairingP1
      let q :: Pairing.G2 BN254 = pairingP2

      Pairing.pairing p q `shouldBe` pairingGtPt

      Pairing.pairing (C.mul' p a) (C.mul' q b)
        `shouldBe` pow (Pairing.pairing p q) (a * b)

    it "pairing smoke test" $ do
      let a :: Integer = 2
      let b :: Integer = 3

      pairing p1 p2 `shouldBe` pt'

      pairing (multiply p1 a) (multiply p2 b)
        `shouldBe` pow (pairing p1 p2) (a * b)

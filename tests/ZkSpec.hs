{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZkSpec (spec) where

import Data.Group (pow)
import Data.Field(Field)
import Data.Foldable(foldl')
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Pact.Native.Pairing
import Test.Hspec
import Test.Hspec.Hedgehog

p1 :: CurvePoint Fq
p1 = multiply g1 2

p2 :: CurvePoint Fq2
p2 = multiply g2 3

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
      addDoubling p1'

      p2' <- forAll genG2
      assert (isOnCurve g2 b2)
      addDoubling p2'

      r1 <- forAll $ Gen.integral (Range.constant 0 1000)
      r2 <- forAll $ Gen.integral (Range.constant 0 1000)
      pairing (multiply p1' r1)  (multiply p2' r2) === pow (pairing p1' p2') (r1 * r2)
      where
      addDoubling pt =
        add (add (double pt) pt) pt === double (double pt)

pairingProofTest :: Spec
pairingProofTest =
  describe "Proof system check" $
    it "Should verify the test proof" $ do
      let pp1 = Point 17899149025429256540670503450603840524526341770363252849540840688855727610005 6794888886586012478899094699714874747255503821264355877996121220781692052981
      let pp2 = Point
                [4555160965165375385578562333880156835913586562443164694386914449127412126755, 16845220796436439159658389520454136502557317448502144055381480626643346396453]
                [15740922883530394503972296892303076718862447518810507376564218784428077030254, 9794083499477745551885635852864140214811154513402172713835626845455029169909]
      let pp3 = Point 2188339130061078784977610313576641337709587353412678866175084864819379744795 7363399164077520072321162032202323356331016580445157674442815097597932017402
      let inp = [293440811465879871736579011234159205259, 82735329187654304797954025540247337640, 1125899906842623, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1]
      pp1 `shouldSatisfy` (`isOnCurve` b1)
      pp2 `shouldSatisfy` (`isOnCurve` b2)
      pp3 `shouldSatisfy` (`isOnCurve` b1)
      verifyProof pp1 pp2 pp3 inp `shouldBe` True


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
  pairingProofTest
  describe "pairing tests" $ do
    it "pairing smoke test" $ do
      let a :: Integer = 2
      let b :: Integer = 3

      pairing p1 p2 `shouldBe` pt'

      pairing (multiply p1 a) (multiply p2 b)
        `shouldBe` pow (pairing p1 p2) (a * b)


-- Verifying proof contract tests

data VerifyingKey
  = VerifyingKey
  { _alfa1 :: G1
  , _beta2 :: G2
  , _gamma2 :: G2
  , _delta2 :: G2
  , _ic :: [G1]
  } deriving (Show, Eq)

data Proof
  = Proof
  { _proofA :: G1
  , _proofB :: G2
  , _proofC :: G1 }
  deriving (Show, Eq)

-- Verifying key from Solidity code
-- NOTE: When porting over solidity code,
-- their representation of points in Fq2 are backwards (that is, 5+2x is represented as [2, 5] and not [5, 2]).
--
solVerifyingKey :: VerifyingKey
solVerifyingKey =
  VerifyingKey alfa1 beta2 gamma2 delta2 ic
  where
  alfa1 = Point
    20491192805390485299153009773594534940189261866228447918068658471970481763042
    9383485363053290200918347156157836566562967994039712273449902621266178545958
  beta2 = Point
    [ 6375614351688725206403948262868962793625744043794305715222011528459656738731
    , 4252822878758300859123897981450591353533073413197771768651442665752259397132]
    [ 10505242626370262277552901082094356697409835680220590971873171140371331206856
    , 21847035105528745403288232691147584728191162732299865338377159692350059136679
    ]
  gamma2 = Point
    [ 10857046999023057135944570762232829481370756359578518086990519993285655852781
    , 11559732032986387107991004021392285783925812861821192530917403151452391805634
    ]
    [ 8495653923123431417604973247489272438418190587263600148770280649306958101930
    , 4082367875863433681332203403145435568316851327593401208105741076214120093531
    ]
  delta2 = Point
    [ 16809031008450260338666218659281275370828342486329981864349494337906939571887
    , 14264224196899353800543367999525075765943744025449601386425105981609273614701]
    [ 11890256881228627469373664690032300678627026600164400771388911741873652827176
    , 19575774426779481952987745556743272872528397589822551825491193352706249147828]
  ic =
    [ Point 703628372913608924678229163876049246019207425954662225921071872483627421722 12211995813319149655177018938770234811518560618061207904053494967754185713570
    , Point 245257359888022973124621370122921842349425470670527924595405093609495308747 16424211039905278739428973076501641419102983596574674938829868639077765818142
    , Point 11110164462478062380497336442107420783806541677398299686522715666379498138472 11772875621558518653532220728777759671658134704623077088185806874340215959359
    , Point 18074393405015025057791386123633410704735277649988577562824116025859630543119 6512362579817099053449579131846840340322546772440905776424666285439665971742
    , Point 16324035526312367325456640773935782752062271159753322602146362004262088969135 1959877669644004327327941905732419844609901799055849407390385762919820073782
    , Point 7958732978061398276873406529212832852529841068044035448997300713023105585033 17143584956740843297694279539007817220119917091654840292522900244927912727369
    , Point 19790616331302654635046558077934057923437716290995001520546062733967158884432 3876239317603061711287361903875372717184929107501147494633374979429019396018
    , Point 14590717951490734152256639590507997933809755442616725401381713035954026634761 3225155507246149008951243692824143870155533409045696678069139586430835695226
    , Point 8650680088861200059927247719422818384661114515347998151694550511594524540419 9638849577460518420520485529873913372767621068985215869067476349645606505077
    , Point 17562317824746836410714834945198951796768727084595092618069846988441315688042 19452027031432595136507137552742780122072574021124781097949079775870562190348
    , Point 13182231104070542193327121010898638946743037034726286337778578885258172200370 757187892995880849330492963674577612574015215504544964795700288326850257327
    , Point 14409932519884296032513716882778643894210345146972579810764887578771580357222 814882272533738805340475214361663264998713952212684412413716253117631329790
    , Point 12509202143372575765947197406153125630356821791569394199509048702081149394252 7737627039987972603153686057063377754848525136672116479087023797531609007397
    , Point 8407744049840718757455802166031970590203865445336729285031156887247294225651 188049909694651097938181392474312752814356026969638293041076738773396096245
    , Point 334110586971536499255771782557320020258112908015957001506636732710862874984 16580656581000952485971124280378233725304032835996723142898957560222784358519
    , Point 7727226277603419079704813924510379012883865378360151539308685989224075286070 12912566309035429310049884735227185094569540217854933912859497659097301533657
    , Point 7681998061757907807252614087798324369243677279399770153101699113400315047554 17481742211680301513436042099326808806059638013948490790089189392596297352637
    , Point 12569417065062182916398142101141299032330235062582799620914438471333638326044 17523163572024314338870129158401181066960544407476643264775649919375538750903
    , Point 20366720006081427400904331708710783224493502999618856932501475384256136971442 12984908039677137046692512896720328898650869058550411984382690230037651160819
    , Point 8428544296631560843765157901673529267885840366893744558046063361202851291828 16289478447662297604149997612134385555472387391722487532370257859531844209269 ]

-- Proof code ported over from the solidity contract (without the weird boolean misdirection)
--     function verifyProof(
--             uint[2] memory a,
--             uint[2][2] memory b,
--             uint[2] memory c,
--             uint[19] memory input
--         ) public view returns (bool r) {
--         Proof memory proof;
--         proof.A = Pairing.G1Point(a[0], a[1]);
--         proof.B = Pairing.G2Point([b[0][0], b[0][1]], [b[1][0], b[1][1]]);
--         proof.C = Pairing.G1Point(c[0], c[1]);
--         uint[] memory inputValues = new uint[](input.length);
--         for(uint i = 0; i < input.length; i++){
--             inputValues[i] = input[i];
--         }
--         if (verify(inputValues, proof) == 0) {
--             return true;
--         } else {
--             return false;
--         }
--     }
verifyProof
  :: G1 -- Proof A
  -> G2 -- Proof B
  -> G1 -- Proof C
  -> [Integer]
  -> Bool
verifyProof pp1 pp2 pp3 inp = let
  p = Proof pp1 pp2 pp3
  in verify inp p

{-
Verify function ported over from Solidity

function verify(uint[] memory input, Proof memory proof) internal view returns (uint) {
        uint256 snark_scalar_field = 21888242871839275222246405745257275088548364400416034343698204186575808495617;
        VerifyingKey memory vk = verifyingKey();
        require(input.length + 1 == vk.IC.length,"verifier-bad-input");
        // Compute the linear combination vk_x
        Pairing.G1Point memory vk_x = Pairing.G1Point(0, 0);
        for (uint i = 0; i < input.length; i++) {
            require(input[i] < snark_scalar_field,"verifier-gte-snark-scalar-field");
            vk_x = Pairing.addition(vk_x, Pairing.scalar_mul(vk.IC[i + 1], input[i]));
        }
        vk_x = Pairing.addition(vk_x, vk.IC[0]);
        if (!Pairing.pairingProd4(
            Pairing.negate(proof.A), proof.B,
            vk.alfa1, vk.beta2,
            vk_x, vk.gamma2,
            proof.C, vk.delta2
        )) return 1;
        return 0;
    }
-}
verify :: [Integer] -> Proof -> Bool
verify inp p = let
  initial = CurveInf
  vk_x = foldl' (\pt (i, kpt) -> add pt (multiply kpt i)) initial (zip inp (drop 1 (_ic solVerifyingKey)))
  vk_x' = add vk_x (head (_ic solVerifyingKey))
  in pairingCheck
    [ (negatePt (_proofA p), _proofB p)
    , (_alfa1 solVerifyingKey, _beta2 solVerifyingKey)
    , (vk_x', _gamma2 solVerifyingKey)
    , (_proofC p, _delta2 solVerifyingKey)]


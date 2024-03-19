{-# LANGUAGE CPP #-}

import Test.Hspec

import qualified Blake2Spec
import qualified KeysetSpec
import qualified RoundTripSpec
import qualified PrincipalSpec
import qualified Test.Pact.Utils.LegacyValue
import qualified SizeOfSpec
import qualified Test.Pact.Native.Pairing

import qualified PactTestsSpec
import qualified ParserSpec
import qualified SchemeSpec
import qualified SignatureSpec
import qualified Test.Pact.Parse

#ifdef BUILD_TOOL
import qualified AnalyzePropertiesSpec
import qualified AnalyzeSpec
import qualified ClientSpec
import qualified DocgenSpec
import qualified GasModelSpec
import qualified GoldenSpec
import qualified HistoryServiceSpec
import qualified HyperlaneSpec
import qualified Keccak256Spec
import qualified PactContinuationSpec
import qualified PersistSpec
import qualified RemoteVerifySpec
import qualified TypecheckSpec
import qualified PactCLISpec
import qualified ZkSpec
import qualified ReplSpec
import qualified PoseidonSpec
import qualified CoverageSpec
#endif

main :: IO ()
main = hspec $ parallel $ do

  describe "Blake2Spec" Blake2Spec.spec
  describe "KeysetSpec" KeysetSpec.spec
  describe "RoundTripSpec" RoundTripSpec.spec
  describe "PrincipalSpec" PrincipalSpec.spec
  describe "Test.Pact.Utils.LegacyValue" Test.Pact.Utils.LegacyValue.spec
  describe "SizeOfSpec" SizeOfSpec.spec
  describe "Test.Pact.Native.Pairing" Test.Pact.Native.Pairing.spec
  describe "PactTestsSpec" PactTestsSpec.spec
  describe "ParserSpec" ParserSpec.spec
  describe "SignatureSpec" SignatureSpec.spec
  describe "SchemeSpec" SchemeSpec.spec
  describe "Test.Pact.Parse" Test.Pact.Parse.spec

#ifdef BUILD_TOOL

  describe "AnalyzePropertiesSpec" AnalyzePropertiesSpec.spec
  describe "AnalyzeSpec" AnalyzeSpec.spec
  describe "ClientSpec" ClientSpec.spec
  describe "DocgenSpec" DocgenSpec.spec
  describe "GasModelSpec" GasModelSpec.spec
  describe "GoldenSpec" GoldenSpec.spec
  describe "HistoryServiceSpec" HistoryServiceSpec.spec
  describe "HyperlaneSpec" HyperlaneSpec.spec
  describe "Keccak256Spec" Keccak256Spec.spec
  describe "PactContinuationSpec" PactContinuationSpec.spec
  describe "PersistSpec" PersistSpec.spec
  describe "RemoteVerifySpec" RemoteVerifySpec.spec
  describe "TypecheckSpec" TypecheckSpec.spec
  describe "PactCLISpec" PactCLISpec.spec
  describe "ZkSpec" ZkSpec.spec
  describe "ReplSpec" ReplSpec.spec
  describe "PoseidonSpec" PoseidonSpec.spec
  describe "CoverageSpec" CoverageSpec.spec

#endif

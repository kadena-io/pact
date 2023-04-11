{-# LANGUAGE CPP #-}

import Test.Hspec

import qualified Blake2Spec
import qualified KeysetSpec
import qualified RoundTripSpec
import qualified PrincipalSpec
import qualified Test.Pact.Utils.LegacyValue
import qualified SizeOfSpec
import qualified Test.Pact.Native.Pairing

#ifndef ghcjs_HOST_OS
import qualified PactTestsSpec
import qualified ParserSpec
import qualified SchemeSpec
import qualified SignatureSpec

# ifdef BUILD_TOOL
import qualified AnalyzePropertiesSpec
import qualified AnalyzeSpec
import qualified ClientSpec
import qualified DocgenSpec
import qualified GasModelSpec
import qualified GoldenSpec
import qualified HistoryServiceSpec
import qualified PactContinuationSpec
import qualified PersistSpec
import qualified RemoteVerifySpec
import qualified TypecheckSpec
import qualified PactCLISpec
import qualified ZkSpec
import qualified ReplSpec
# endif
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

#ifndef ghcjs_HOST_OS

  describe "PactTestsSpec" PactTestsSpec.spec
  describe "ParserSpec" ParserSpec.spec
  describe "SignatureSpec" SignatureSpec.spec
  describe "SchemeSpec" SchemeSpec.spec

# ifdef BUILD_TOOL

  describe "AnalyzePropertiesSpec" AnalyzePropertiesSpec.spec
  describe "AnalyzeSpec" AnalyzeSpec.spec
  describe "ClientSpec" ClientSpec.spec
  describe "DocgenSpec" DocgenSpec.spec
  describe "GasModelSpec" GasModelSpec.spec
  describe "GoldenSpec" GoldenSpec.spec
  describe "HistoryServiceSpec" HistoryServiceSpec.spec
  describe "PactContinuationSpec" PactContinuationSpec.spec
  describe "PersistSpec" PersistSpec.spec
  describe "RemoteVerifySpec" RemoteVerifySpec.spec
  describe "TypecheckSpec" TypecheckSpec.spec
  describe "PactCLISpec" PactCLISpec.spec
  describe "ZkSpec" ZkSpec.spec
  describe "ReplSpec" ReplSpec.spec


# endif
#endif

{-# LANGUAGE CPP #-}

import Test.Hspec

import qualified Blake2Spec
import qualified KeysetSpec
import qualified RoundTripSpec

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
import qualified SizeOfSpec

# endif
#endif

main :: IO ()
main = hspec $ do

  describe "Blake2Spec" Blake2Spec.spec
  describe "KeysetSpec" KeysetSpec.spec
  describe "RoundTripSpec" RoundTripSpec.spec

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
  describe "SizeOfSpec" SizeOfSpec.spec

# endif
#endif

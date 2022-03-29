{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module PactCLISpec(spec) where

import Test.Hspec

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either(isRight)
import qualified Data.Yaml as Y
import Data.Text(Text)
import System.FilePath

import Pact.ApiReq
import Pact.Types.Command
import Pact.Types.API(SubmitBatch)
import Pact.Types.SigData(SigData)

spec :: Spec
spec = do
  partialSigTests

-- note, generated with `pact -g`
key1, key2 :: FilePath
key1 = "tests" </> "add-sig" </> "key1.yaml"
key2 = "tests" </> "add-sig" </> "key2.yaml"

unsignedFile :: FilePath
unsignedFile = "tests" </> "add-sig" </> "unsigned.yaml"

unsignedFile2 :: FilePath
unsignedFile2 = "tests" </> "add-sig" </> "unsigned2.yaml"

partialSigTests :: Spec
partialSigTests =
  describe "partial sigs" $ do
    it "validates and combines two partial sigs" $ do
      unsigned <- BS.readFile unsignedFile
      sig1 <- Y.decodeEither' <$> addSigsReq [key1] True unsigned
      sig2 <- Y.decodeEither' <$> addSigsReq [key2] True unsigned
      sig1 `shouldSatisfy` isRight
      sig2 `shouldSatisfy` isRight
      let sig1' = either (error "impossible") id sig1
          sig2' = either (error "impossible") id sig2
      -- Works normally for local
      command <- A.eitherDecode @(Command Text) . BSL.fromStrict <$> combineSigDatas [sig1', sig2'] True
      command `shouldSatisfy` isRight
      -- Works as submitBatch
      commandBatch <- A.eitherDecode @SubmitBatch . BSL.fromStrict <$> combineSigDatas [sig1', sig2'] False
      commandBatch `shouldSatisfy` isRight
    it "validates when command portion is missing as well:" $ do
      unsigned <- BS.readFile unsignedFile2
      sig1 <- Y.decodeEither' @(SigData Text) <$> addSigsReq [key1] True unsigned
      sig2 <- Y.decodeEither' @(SigData Text) <$> addSigsReq [key2] True unsigned
      sig1 `shouldSatisfy` isRight
      sig2 `shouldSatisfy` isRight
    it "does not validate on missing signatures" $ do
      unsigned <- BS.readFile unsignedFile
      sig1 <- Y.decodeEither' <$> addSigsReq [key1] True unsigned
      sig1 `shouldSatisfy` isRight
      let sig1' = either (error "impossible") id sig1
      -- Works normally for local
      combineSigDatas [sig1'] True `shouldThrow` anyException

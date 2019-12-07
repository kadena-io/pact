{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GoldenSpec

  (spec)

  where

import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Either
import System.Directory

import Test.Hspec
import Test.Hspec.Golden


import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Persistence

spec :: Spec
spec = describe "goldenAccounts" $ goldenAccounts

goldenAccounts :: Spec
goldenAccounts = after_ (cleanupActual tn) $ do

  (r,s) <- runIO $ execScript' Quiet fp
  it ("loads " ++ fp) $ r `shouldSatisfy` isRight
  mr <- runIO $ replLookupModule s mn
  case mr of
    Left e -> it "module load failed" $ expectationFailure e
    Right m -> case traverse (traverse toPersistDirect) m of
      Left e -> it "failed to convert to PersistDirect" $ expectationFailure (show e)
      Right m' -> do
        it "matches golden" $ golden tn m'
  where
    fp = "golden/golden.accounts.repl"
    mn = "accounts"
    tn = "accounts-module"


cleanupActual :: String -> IO ()
cleanupActual testname =
  catch (removeFile $ "golden/" ++ testname ++ "/actual")
  (\(_ :: SomeException) -> return ())


golden :: (Show a,FromJSON a,ToJSON a) => String -> a -> Golden a
golden name obj = Golden
  { output = obj
  , encodePretty = elide . show
  , writeToFile = jsonEncode
  , readFromFile = jsonDecode
  , testName = name
  , directory = "golden"
  }
  where
    elide s | length s < 256 = s
            | otherwise = take 256 s ++ "..."
    jsonEncode fp = BL.writeFile fp . encode
    jsonDecode fp = do
      r <- eitherDecode <$> BL.readFile fp
      case r of
        Left e -> throwIO $ userError $ "golden decode failed: " ++ show e
        Right v -> return v

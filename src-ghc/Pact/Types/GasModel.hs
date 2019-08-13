{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Pact.Types.GasModel
  ( GasTest(..)
  , SomeGasTest(..)
  , GasUnitTests(..)

  , NoopNFData(..)
  
  , unitTests
  , allNatives
  , untestedNatives
  
  , parseCode
  , printResult
  , eitherDie

  ) where

import Control.Lens               hiding ((.=),DefName)
import Control.DeepSeq            (NFData(..))
import Control.Exception          (throwIO)
import Data.Aeson                 (toJSON)
import Data.Default               (def)
import Data.List                  (foldl')


import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.List.NonEmpty  as NEL


-- Internal exports
--
import Pact.Native
import Pact.Types.Native
import Pact.Compile               (compileExps, mkTextInfo)
import Pact.MockDb
import Pact.Types.PactValue       (toPactValueLenient)

import Pact.Gas
import Pact.Interpreter
import Pact.Parse
import Pact.Types.Command
import Pact.Types.Lang
import Pact.Types.Runtime
import Pact.Types.SPV



data GasTest e = GasTest
  { _gasTestExpression :: T.Text
  , _gasTestDescription :: T.Text
  , _gasTestMockState :: EvalState
  , _gasTestMockEnv :: IO (EvalEnv e)
  , _gasTestCleanup :: EvalEnv e -> IO ()
  }
makeLenses ''GasTest

data SomeGasTest
  = forall e. (Show e, Eq e) => SomeGasTest (GasTest e)

newtype GasUnitTests
  = GasUnitTests (NEL.NonEmpty SomeGasTest)



-- | Newtype to provide a noop NFData instance.
-- Intended for use in criterion's 'envWithCleanup'
-- which wants environment values to be NFData.
newtype NoopNFData a = NoopNFData a
  deriving (Show)
instance NFData (NoopNFData a) where
  rnf _ = ()



defGasTest :: T.Text -> GasTest ()
defGasTest expr =
  GasTest
  expr
  expr
  defEvalState
  createDefMockEnv
  (const $ return ())

defEvalState :: EvalState
defEvalState = def

defEvalEnv :: PactDbEnv e -> EvalEnv e
defEvalEnv db =
  setupEvalEnv db entity Transactional (initMsgData pactInitialHash)
  initRefStore freeGasEnv permissiveNamespacePolicy noSPVSupport def
  where entity = Just $ EntityName "entity"

createDefMockEnv :: IO (EvalEnv ())
createDefMockEnv = do
  db <- mkMockEnv def
  return $ defEvalEnv db


--- Helper functions
---

parseCode :: T.Text -> IO [Term Name]
parseCode m = do
  parsedCode <- ParsedCode m <$> eitherDie (parseExprs m)
  throwEither $ compileExps
                (mkTextInfo $ _pcCode parsedCode)
                (_pcExps parsedCode)

printResult :: Either PactError [Term Name] -> IO ()
printResult res = case res of
  Left err -> print $ show err
  Right ts -> print $ show $ toJSON $ map (toJSON . toPactValueLenient) ts

eitherDie :: (Show b) => Either b a -> IO a
eitherDie = either (throwIO . userError . show) (return $!)


--- Gas unit tests
---
allNatives :: [NativeDef]
allNatives = concatMap snd natives

untestedNatives :: [NativeDefName]
untestedNatives = foldl' check [] allNatives
  where
    check li (nativeName,_,_) = case (HM.lookup nativeName unitTests) of
      Nothing -> nativeName : li
      Just _ -> li


unitTests :: HM.HashMap NativeDefName GasUnitTests
unitTests = HM.empty


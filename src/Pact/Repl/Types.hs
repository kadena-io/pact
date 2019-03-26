{-# LANGUAGE TemplateHaskell #-}
module Pact.Repl.Types
  ( ReplMode(..)
  , Hdl(..)
  , ReplState(..),rEnv,rEvalState,rMode,rOut,rFile,rTermOut
  , TestResult(..)
  , Repl
  , LibOp(..)
  , LibState(..),rlsPure,rlsOp,rlsTxName,rlsTests,rlsVerifyUri,rlsMockSPV,rlsPacts
  , Tx(..)
  , SPVMockKey(..)
  ) where

import Control.Lens (makeLenses)
import Data.Default (Default(..))
import Data.Monoid (Endo(..))
import Control.Monad.State.Strict (StateT)
import Control.Concurrent (MVar)
import Pact.PersistPactDb (DbEnv)
import Pact.Persist.Pure (PureDb)
import Pact.Types.Runtime (EvalEnv,EvalState,Term,Name,FunApp,Info,Object,Term(..),PactId,PactExec)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Pact.Types.Pretty (Pretty,pretty,renderCompactText)

data ReplMode =
    Interactive |
    Script { rmTrace :: Bool, rmFile :: FilePath } |
    FailureTest |
    Quiet |
    StringEval |
    StdinPipe
    deriving (Eq,Show)

data Hdl = HOut|HErr

data TestResult = TestResult
  { trName :: Text
  , trFailure :: Maybe (FunApp,Text)
  }

data ReplState = ReplState {
      _rEnv :: EvalEnv LibState
    , _rEvalState :: EvalState
    , _rMode :: ReplMode
    , _rOut :: String
    , _rTermOut :: [Term Name]
    , _rFile :: Maybe FilePath
    }

type Repl a = StateT ReplState IO a


data LibOp =
    Noop |
    UpdateEnv (Endo (EvalEnv LibState)) |
    Load FilePath Bool |
    Tx Info Tx (Maybe Text) |
    Print (Term Name) |
    TcErrors [String]
instance Default LibOp where def = Noop

data Tx = Begin|Commit|Rollback deriving (Eq,Show,Bounded,Enum,Ord)

newtype SPVMockKey = SPVMockKey (Text,Object Name) deriving Show
instance Pretty SPVMockKey where
  pretty (SPVMockKey (t,o)) = pretty t <> pretty o
instance Eq SPVMockKey where
  a == b = renderCompactText a == renderCompactText b
instance Ord SPVMockKey where
  a `compare` b = renderCompactText a `compare` renderCompactText b

data LibState = LibState {
      _rlsPure :: MVar (DbEnv PureDb)
    , _rlsOp :: LibOp
    , _rlsTxName :: Maybe Text
    , _rlsTests :: [TestResult]
    , _rlsVerifyUri :: Maybe String
    , _rlsMockSPV :: M.Map SPVMockKey (Object Name)
    , _rlsPacts :: M.Map PactId PactExec
}


makeLenses ''LibState
makeLenses ''ReplState

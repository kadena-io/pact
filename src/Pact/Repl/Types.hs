{-# LANGUAGE TemplateHaskell #-}
module Pact.Repl.Types
  ( ReplMode(..)
  , Hdl(..)
  , ReplState(..),rEnv,rEvalState,rMode,rOut,rFile,rTermOut,rTxId
  , TestResult(..)
  , Repl
  , LibOp(..)
  , LibState(..),rlsPure,rlsOp,rlsTxName,rlsTests,rlsVerifyUri,rlsMockSPV,rlsPacts
  , Tx(..)
  , SPVMockKey(..)
  , getAllModules
  ) where

import Control.Lens (makeLenses)
import Control.Monad
import Data.Default (Default(..))
import Data.Monoid (Endo(..))
import Control.Monad.State.Strict (StateT)
import Control.Concurrent (MVar)
import Pact.PersistPactDb (DbEnv)
import Pact.Persist.Pure (PureDb)
import Pact.Types.Runtime
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Pact.Types.Pretty (Pretty,pretty,renderCompactText)
import Pact.Native.Internal

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
    , _rTxId :: Maybe TxId
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

getAllModules :: HasInfo i => i -> Eval e (HM.HashMap ModuleName (ModuleData Ref))
getAllModules i = do
  mks <- keys (getInfo i) Modules
  fmap HM.fromList $ forM mks $ \mk -> do
    m <- getModule i mk
    return (mk,m)

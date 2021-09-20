{-# LANGUAGE TemplateHaskell #-}
module Pact.Repl.Types
  ( ReplMode(..)
  , Hdl(..)
  , ReplState(..),rEnv,rEvalState,rMode,rOut,rFile,rTermOut
  , TestResult(..)
  , Repl
  , LibOp(..)
  , LibState(..),rlsPure,rlsOp,rlsTx,rlsTests,rlsVerifyUri,rlsMockSPV,rlsDynEnv
  , Tx(..)
  , SPVMockKey(..)
  , getAllModules
  ) where

import Control.Lens (makeLenses)
import Control.Monad
import Control.Monad.State.Strict (StateT)
import Control.Concurrent (MVar)

import Data.Default (Default(..))
import Data.Monoid (Endo(..))
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM

import Pact.PersistPactDb (DbEnv)
import Pact.Persist.Pure (PureDb)
import Pact.Runtime.Utils
import Pact.Types.Runtime
import Pact.Types.Pretty (Pretty, pretty, renderCompactText)
import Pact.Types.Typecheck

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

-- | A REPL operation that requires top-level evaluation.
data LibOp
    = Noop
    | UpdateEnv (Endo (EvalEnv LibState))
      -- ^ mutate the read-only environment
    | Load FilePath Bool
      -- ^ load a filepath script
    | Print (Term Name)
      -- ^ print to terminal
    | Output [RenderedOutput]
      -- ^ special output for typechecker/FV
instance Default LibOp where def = Noop

-- | Semi-pathological instance that either overwrites
-- or mappends to 'Endo' in 'UpdateEnv'.
instance Semigroup LibOp where
  UpdateEnv a <> UpdateEnv b = UpdateEnv (a <> b)
  _ <> b = b
instance Monoid LibOp where
  mempty = def

-- | Transaction action type.
data Tx = Begin|Commit|Rollback deriving (Eq,Show,Bounded,Enum,Ord)

newtype SPVMockKey = SPVMockKey (Text, (Object Name))
  deriving Show

instance Pretty SPVMockKey where
  pretty (SPVMockKey (t,o)) = pretty t <> pretty o

instance Eq SPVMockKey where
  a == b = renderCompactText a == renderCompactText b
instance Ord SPVMockKey where
  a `compare` b = renderCompactText a `compare` renderCompactText b

data LibState = LibState
  { _rlsPure :: MVar (DbEnv PureDb)
  , _rlsOp :: LibOp
  , _rlsTx :: (Maybe TxId, Maybe Text)
  , _rlsTests :: [TestResult]
  , _rlsVerifyUri :: Maybe String
  , _rlsMockSPV :: M.Map SPVMockKey (Object Name)
  , _rlsDynEnv :: DynEnv
  }


makeLenses ''LibState
makeLenses ''ReplState

getAllModules :: HasInfo i => i -> Eval e (HM.HashMap ModuleName (ModuleData Ref))
getAllModules i = do
  mks <- keys (getInfo i) Modules
  fmap HM.fromList $ forM mks $ \mk -> do
    m <- getModule i mk
    return (mk,m)

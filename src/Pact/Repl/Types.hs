{-# LANGUAGE TemplateHaskell #-}
module Pact.Repl.Types
  ( ReplMode(..)
  , Hdl(..)
  , ReplState(..),rEnv,rEvalState,rMode,rOut,rFile
  , Repl
  , LibOp(..)
  , LibState(..),rlsPure,rlsOp,rlsTxName
  , Tx(..)
  ) where

import Control.Lens (makeLenses)
import Data.Default (Default(..))
import Data.Monoid (Endo(..))
import Control.Monad.State.Strict (StateT)
import Control.Concurrent (MVar)
import Pact.PersistPactDb (DbEnv)
import Pact.Persist.Pure (PureDb)
import Pact.Types.Runtime (EvalEnv,EvalState)
import Data.Text (Text)

data ReplMode =
    Interactive |
    Script String |
    FailureTest |
    Quiet |
    StringEval
    deriving (Eq,Show)

data Hdl = HOut|HErr

data ReplState = ReplState {
      _rEnv :: EvalEnv LibState
    , _rEvalState :: EvalState
    , _rMode :: ReplMode
    , _rOut :: String
    , _rFile :: Maybe FilePath
    }

type Repl a = StateT ReplState IO a


data LibOp =
    Noop |
    UpdateEnv (Endo (EvalEnv LibState)) |
    Load FilePath Bool |
    TcErrors [String]
instance Default LibOp where def = Noop

data Tx = Begin|Commit|Rollback deriving (Eq,Show,Bounded,Enum,Ord)

data LibState = LibState {
      _rlsPure :: MVar (DbEnv PureDb)
    , _rlsOp :: LibOp
    , _rlsTxName :: Maybe Text
}


makeLenses ''LibState
makeLenses ''ReplState

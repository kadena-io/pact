module Pact.Repl.Types
  ( ReplMode(..)
  , Hdl(..)
  , ReplState(..),rEnv,rEvalState,rMode,rOut,rFile
  , TestResult(..)
  , Repl
  , LibOp(..)
  , LibState(..),rlsPure,rlsOp,rlsTxName,rlsTests
  , Tx(..)
  ) where

import Control.Lens
import Data.Default (Default(..))
import Data.Monoid (Endo(..))
import Control.Monad.State.Strict (StateT)
import Control.Concurrent (MVar)
import Pact.PersistPactDb (DbEnv)
import Pact.Persist.Pure (PureDb)
import Pact.Types.Runtime (EvalEnv,EvalState,Term,Name,FunApp)
import Data.Text (Text)

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
    , _rFile :: Maybe FilePath
    }

type Repl a = StateT ReplState IO a


data LibOp =
    Noop |
    UpdateEnv (Endo (EvalEnv LibState)) |
    Load FilePath Bool |
    Print (Term Name) |
    TcErrors [String]
instance Default LibOp where def = Noop

data Tx = Begin|Commit|Rollback deriving (Eq,Show,Bounded,Enum,Ord)

data LibState = LibState {
      _rlsPure :: MVar (DbEnv PureDb)
    , _rlsOp :: LibOp
    , _rlsTxName :: Maybe Text
    , _rlsTests :: [TestResult]
}


------------------------------------------------------------------------------
--makeLenses ''LibState
rlsOp :: Lens' LibState LibOp
rlsOp f_a7YBx (LibState x1_a7YBy x2_a7YBz x3_a7YBA x4_a7YBB)
  = fmap
      (\ y1_a7YBC -> LibState x1_a7YBy y1_a7YBC x3_a7YBA x4_a7YBB)
      (f_a7YBx x2_a7YBz)
{-# INLINE rlsOp #-}
rlsPure :: Lens' LibState (MVar (DbEnv PureDb))
rlsPure f_a7YBD (LibState x1_a7YBE x2_a7YBF x3_a7YBG x4_a7YBH)
  = fmap
      (\ y1_a7YBI -> LibState y1_a7YBI x2_a7YBF x3_a7YBG x4_a7YBH)
      (f_a7YBD x1_a7YBE)
{-# INLINE rlsPure #-}
rlsTests :: Lens' LibState [TestResult]
rlsTests f_a7YBJ (LibState x1_a7YBK x2_a7YBL x3_a7YBM x4_a7YBN)
  = fmap
      (\ y1_a7YBO -> LibState x1_a7YBK x2_a7YBL x3_a7YBM y1_a7YBO)
      (f_a7YBJ x4_a7YBN)
{-# INLINE rlsTests #-}
rlsTxName :: Lens' LibState (Maybe Text)
rlsTxName f_a7YBP (LibState x1_a7YBQ x2_a7YBR x3_a7YBS x4_a7YBT)
  = fmap
      (\ y1_a7YBU -> LibState x1_a7YBQ x2_a7YBR y1_a7YBU x4_a7YBT)
      (f_a7YBP x3_a7YBS)
{-# INLINE rlsTxName #-}

------------------------------------------------------------------------------
--makeLenses ''ReplState
rEnv :: Lens' ReplState (EvalEnv LibState)
rEnv
  f_a7YDc
  (ReplState x1_a7YDd x2_a7YDe x3_a7YDf x4_a7YDg x5_a7YDh)
  = fmap
      (\ y1_a7YDi
         -> ReplState y1_a7YDi x2_a7YDe x3_a7YDf x4_a7YDg x5_a7YDh)
      (f_a7YDc x1_a7YDd)
{-# INLINE rEnv #-}
rEvalState :: Lens' ReplState EvalState
rEvalState
  f_a7YDj
  (ReplState x1_a7YDk x2_a7YDl x3_a7YDm x4_a7YDn x5_a7YDo)
  = fmap
      (\ y1_a7YDp
         -> ReplState x1_a7YDk y1_a7YDp x3_a7YDm x4_a7YDn x5_a7YDo)
      (f_a7YDj x2_a7YDl)
{-# INLINE rEvalState #-}
rFile :: Lens' ReplState (Maybe FilePath)
rFile
  f_a7YDq
  (ReplState x1_a7YDr x2_a7YDs x3_a7YDt x4_a7YDu x5_a7YDv)
  = fmap
      (\ y1_a7YDw
         -> ReplState x1_a7YDr x2_a7YDs x3_a7YDt x4_a7YDu y1_a7YDw)
      (f_a7YDq x5_a7YDv)
{-# INLINE rFile #-}
rMode :: Lens' ReplState ReplMode
rMode
  f_a7YDx
  (ReplState x1_a7YDy x2_a7YDz x3_a7YDA x4_a7YDB x5_a7YDC)
  = fmap
      (\ y1_a7YDD
         -> ReplState x1_a7YDy x2_a7YDz y1_a7YDD x4_a7YDB x5_a7YDC)
      (f_a7YDx x3_a7YDA)
{-# INLINE rMode #-}
rOut :: Lens' ReplState String
rOut
  f_a7YDE
  (ReplState x1_a7YDF x2_a7YDG x3_a7YDH x4_a7YDI x5_a7YDJ)
  = fmap
      (\ y1_a7YDK
         -> ReplState x1_a7YDF x2_a7YDG x3_a7YDH y1_a7YDK x5_a7YDJ)
      (f_a7YDE x4_a7YDI)
{-# INLINE rOut #-}

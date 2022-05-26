module Pact.Core.Typed.Eval.Runtime where

import Pact.Core.Typed.Term

data StackFrame name
  = StackFrame
  { _sfName :: name
  , _sfType :: DefType
  } deriving Show

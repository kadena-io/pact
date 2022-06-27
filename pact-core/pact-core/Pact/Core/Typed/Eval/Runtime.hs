module Pact.Core.Typed.Eval.Runtime where

-- todo: move CEK typed and co. here
newtype StackFrame name
  = StackFrame
  { _sfName :: name
  } deriving Show

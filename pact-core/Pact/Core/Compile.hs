{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Pact.Core.Compile where

-- import Control.Lens
-- import Control.Monad.Except
-- import Data.Text as Text
-- import Data.ByteString(ByteString)
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
-- import qualified Data.ByteString as B

-- import Pact.Core.Info
-- import Pact.Core.Persistence
-- import Pact.Core.Builtin
-- import Pact.Core.Gas
-- import Pact.Core.Names
-- import Pact.Core.Repl.Utils
-- import Pact.Core.Untyped.Term
-- import Pact.Core.Untyped.Utils
-- import Pact.Core.IR.Desugar
-- import Pact.Core.IR.Typecheck
-- import Pact.Core.Type
-- import Pact.Core.Typed.Overload

-- import Pact.Core.Untyped.Eval.Runtime
-- import Pact.Core.Repl.Runtime
-- import Pact.Core.Repl.Runtime.ReplBuiltin

-- import qualified Pact.Core.IR.Term as IR

-- import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
-- import qualified Pact.Core.Syntax.Lisp.Parser as Lisp

-- compileExpr source pactDb loaded = do
--   lexed <- liftEither (Lisp.lexer source)
--   parsed <- liftEither (Lisp.parseExpr lexed)
--   (DesugarOutput desugared loaded'_) <- runDesugarTermLisp pactDb loaded parsed
--   (ty, typed) <- liftEither

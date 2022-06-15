{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}

module Pact.Core.Typed.Eval.CEK
 ( CEKTLEnv
 , CEKEnv
 , CEKValue(..)
 , BuiltinFn(..)
 , CEKState(..)
 , CEKRuntime
 , eval
 , runEvalT
 , EvalT(..)
 , Cont(..)
 , ETerm
 ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Vector(Vector)
import Data.List.NonEmpty(NonEmpty(..))
import Data.RAList(RAList)
import Data.Primitive(Array, indexArray)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.RAList as RAList
import qualified Data.Vector as V

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Typed.Term
import Pact.Core.Builtin
import Pact.Core.Pretty(Pretty(..), (<+>))
import Pact.Core.Gas
import qualified Pact.Core.Pretty as P

type CEKTLEnv b = Map FullyQualifiedName (ETerm b)
type CEKEnv b = RAList (CEKValue b)
newtype BuiltinFn b = BuiltinFn (CEKRuntime b => NonEmpty (CEKValue b) -> EvalT b (CEKValue b))
type CEKRuntime b = (?cekLoaded :: CEKTLEnv b, ?cekBuiltins :: Array (BuiltinFn b), Enum b)

data CEKState b
  = CEKState
  { _cekGas :: Gas
  , _cekEvalLog :: Maybe [Text]
  } deriving Show

newtype EvalT b a =
  EvalT { unEval :: StateT (CEKState b) IO a }
  deriving ( Functor, Applicative, Monad
           , MonadState (CEKState b), MonadIO
           , MonadFail)
           via (StateT (CEKState b) IO)

runEvalT :: CEKState b -> EvalT b a -> IO (a, CEKState b)
runEvalT s (EvalT action) = runStateT action s

data CEKValue b
  = VLiteral !Literal
  | VObject !(Map Field (CEKValue b))
  | VList !(Vector (CEKValue b))
  | VClosure ![Name] !(ETerm b) !(CEKEnv b)
  | VNative !b
  | VGuard !(Guard Name (CEKValue b))
  | VCap !Name
  | VModRef
  | VError Text
  deriving (Show)

data Cont b
  = Fn (CEKValue b) (Cont b)
  | Arg (CEKEnv b) (NonEmpty (ETerm b)) (Cont b)
  | BlockC (CEKEnv b) [ETerm b] (Cont b)
  | Mt
  deriving Show

-- Todo: exception handling? do we want labels
-- Todo: `traverse` usage should be perf tested.
-- It might be worth making `Arg` frames incremental, as opposed to a traverse call
eval
  :: forall b. CEKRuntime b
  => CEKEnv b
  -> ETerm b
  -> EvalT b (CEKValue b)
eval = evalCEK Mt
  where
  evalCEK
    :: Cont b
    -> CEKEnv b
    -> ETerm b
    -> EvalT b (CEKValue b)
  evalCEK cont env (Var n _)  =
    case _nKind n of
      NBound i -> returnCEK cont (env RAList.!! i)
      -- Top level names are not closures, so we wipe the env
      NTopLevel mname mh ->
        let !t = ?cekLoaded Map.! FullyQualifiedName mname (_nName n) mh
        in evalCEK cont RAList.Nil t
  evalCEK cont _env (Constant l _)=
    returnCEK cont (VLiteral l)
  evalCEK cont env (App fn arg _) =
    evalCEK (Arg env arg cont) env fn
  evalCEK cont env (Lam ns body _) =
    returnCEK cont (VClosure (NE.toList (fst <$> ns)) body env)
  evalCEK cont env (Let n e1 e2 _) =
    returnCEK (Arg env (e1 :| []) cont) (VClosure [n] e2 env)
  evalCEK cont _env (Builtin b _) = do
    returnCEK cont (VNative b)
  evalCEK cont env (ObjectLit obj _) = do
    vs <- traverse (evalCEK Mt env) obj
    returnCEK cont (VObject vs)
  evalCEK cont env (Block (t :| ts) _) = do
    evalCEK (BlockC env ts cont) env t
  evalCEK cont env (ListLit _ ts _) = do
    ts' <- traverse (evalCEK Mt env) ts
    returnCEK cont (VList ts')
  evalCEK cont env (TyApp t _ _) =
    evalCEK cont env t
  evalCEK cont env (TyAbs _ t _) =
    evalCEK cont env t
  evalCEK cont env (ObjectOp op _) = case op of
    ObjectAccess f o -> do
      o' <- evalCEK Mt env o
      v' <- objAccess f o'
      returnCEK cont v'
    ObjectRemove f o -> do
      o' <- evalCEK Mt env o
      v' <- objRemove f o'
      returnCEK cont v'
    ObjectUpdate f v o -> do
      o' <- evalCEK Mt env o
      v' <- evalCEK Mt env v
      out <- objUpdate f o' v'
      returnCEK cont out
  evalCEK _cont _env (Error s _ _) = error (T.unpack s) -- todo: proper error continuations, we actually have `try`
  returnCEK
    :: Cont b
    -> CEKValue b
    -> EvalT b (CEKValue b)
  returnCEK (Arg env args cont) fn =
    evalCEKArgs args env (Fn fn cont)
  returnCEK (Fn fn ctx) arg =
    applyLam fn (arg :| []) ctx
  returnCEK (BlockC env (t:ts) cont) _discarded =
    evalCEK (BlockC env ts cont) env t
  returnCEK (BlockC _ [] cont) v =
    returnCEK cont v
  returnCEK Mt v = return v
  evalCEKArgs args env cont = do
    args' <- traverse (evalCEK Mt env) args
    returnCEKArgs args' cont
  returnCEKArgs args (Fn fn cont) =
    applyLam fn args cont
  returnCEKArgs _args _ =
    error "Invalid stack frame"
  applyLam (VClosure ns body env) args cont =
    applyArgs ns (NE.toList args) env body cont
  applyLam (VNative b) args cont =
    let (BuiltinFn f) = indexArray ?cekBuiltins (fromEnum b)
    in f args >>= returnCEK cont
  applyLam _ _ _ = error "applying to non-fun"
  applyArgs (_ : ns') (arg : args') env body cont =
    applyArgs ns' args' (RAList.cons arg env) body cont
  -- Todo: create stack frame here.
  -- function application is saturated.
  applyArgs [] [] env body cont = evalCEK cont env body
    -- local (addFrame (StackFrame lamn DTDefun)) $ evalCEK cont env body
  -- Args unsaturated, create a closure and return as an argument
  applyArgs (n:ns') [] env body cont =
    returnCEK cont (VClosure (n:ns') body env)
  applyArgs [] (_args:_args') _env _body _cont =
    error "too many arguments in fn application"
  objAccess f (VObject o) = pure (o Map.! f)
  objAccess _ _ = error "fail"
  objRemove f (VObject o) = pure (VObject (Map.delete f o))
  objRemove _ _ = error "fail"
  objUpdate f v (VObject o) = pure (VObject (Map.insert f v o))
  objUpdate _ _ _ = error "fail"


instance Pretty b => Pretty (CEKValue b) where
  pretty = \case
    VLiteral i -> pretty i
    VObject o ->
      let toBind (k, e) = pretty k <> ":" <> pretty e
      in P.braces $ P.hsep (P.punctuate P.comma (toBind <$> Map.toList o))
    VList v ->
      P.brackets $ P.hsep (P.punctuate P.comma (V.toList (pretty <$> v)))
    VClosure{} ->
      P.angles "closure#"
    VNative b ->
      P.angles $ "native" <+> pretty b
    VGuard _ -> error "undefined"
    VCap _ -> P.angles "capability"
    VModRef -> "modref"
    VError e -> "Error:" <+> pretty e

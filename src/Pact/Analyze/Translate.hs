{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase                 #-}
{-# language MultiWayIf                 #-}
{-# language MonadFailDesugaring        #-}
{-# language OverloadedStrings          #-}
{-# language Rank2Types                 #-}
{-# language ScopedTypeVariables        #-}

module Pact.Analyze.Translate where

import Control.Applicative (Alternative, (<|>))
import Control.Lens hiding (op)
import Control.Monad.Except (Except, MonadError, throwError)
import Control.Monad.Fail
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.SBV (literal)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Thyme (parseTime)
import Data.Traversable (for)
import Data.Type.Equality
import Pact.Types.Lang (Literal(..), Type(..), PrimType(..), Arg(..))
import qualified Pact.Types.Lang as Pact
import Pact.Types.Typecheck hiding (Var, Schema, Object)
import qualified Pact.Types.Typecheck as Pact
import System.Locale (defaultTimeLocale)

import Pact.Analyze.Patterns
import Pact.Analyze.Types

data TranslateFailure
  = BranchesDifferentTypes EType EType
  | EmptyBody
  | MalformedArithOp Text [AST Node]
  | MalformedLogicalOp Text [AST Node]
  | MalformedComparison Text [AST Node]
  | NotConvertibleToSchema (Pact.Type Pact.UserType)
  | TypeMismatch EType EType
  | UnexpectedNode String (AST Node)
  | AlternativeFailures [TranslateFailure]
  | MonadFailure String
  deriving Show

instance Monoid TranslateFailure where
  mempty = AlternativeFailures []
  mappend (AlternativeFailures xs) (AlternativeFailures ys) = AlternativeFailures (xs `mappend` ys)
  mappend (AlternativeFailures xs) x = AlternativeFailures (x:xs)
  mappend x (AlternativeFailures xs) = AlternativeFailures (x:xs)
  mappend x y = AlternativeFailures [x, y]

newtype TranslateM a
  = TranslateM { unTranslateM :: ReaderT (Map Node Text) (Except TranslateFailure) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
    MonadReader (Map Node Text), MonadError TranslateFailure)

instance MonadFail TranslateM where
  fail s = throwError (MonadFailure s)

translateBody :: [AST Node] -> TranslateM ETerm
translateBody [] = throwError EmptyBody
translateBody [ast] = translateNode ast
translateBody (ast:asts) = do
  -- XXX: allow objects
  -- EAny ast' <- translateNode ast
  ETerm ast' _   <- translateNode ast
  ETerm asts' ty <- translateBody asts
  pure $ ETerm (Sequence ast' asts') ty

translateNode :: AST Node -> TranslateM ETerm
translateNode = \case
  AST_Let _ [] body -> translateBody body

  AST_Let node ((Named _ varNode _, rhsNode):bindingsRest) body -> do
    rhsETerm <- translateNode rhsNode
    let varName = varNode ^. aId ^. tiName
    local (at varNode ?~ varName) $ do
      --
      -- TODO: do we only want to allow subsequent bindings to reference
      --       earlier ones if we know it's let* rather than let? or has this
      --       been enforced by earlier stages for us?
      --
      -- XXX allow objects here
      body' <- translateNode $ AST_Let node bindingsRest body
      pure $ case body' of
        ETerm   bodyTm bodyTy -> ETerm   (Let varName rhsETerm bodyTm) bodyTy
        EObject bodyTm bodyTy -> EObject (Let varName rhsETerm bodyTm) bodyTy

  AST_Var node -> do
    varName <- view (ix node)
    pure $ case typeFromPact (_aTy node) of
      EType ty         -> ETerm (Var varName) ty
      EObjectTy schema -> EObject (Var varName) schema

  -- Int
  AST_NegativeLit lit -> case lit of
    LInteger i -> pure $ ETerm (IntUnaryArithOp Negate (Literal (literal i))) TInt
    LDecimal d -> pure $ ETerm (DecUnaryArithOp Negate (Literal (literal (mkDecimal d)))) TDecimal
    _ -> undefined

  AST_Lit lit -> case lit of
    LInteger i -> pure $ ETerm (Literal (literal i)) TInt
    LBool b    -> pure $ ETerm (Literal (literal b)) TBool
    LString s  -> pure $ ETerm (Literal $ literal $ T.unpack s) TStr
    LDecimal d -> pure $ ETerm (Literal (literal (mkDecimal d))) TDecimal
    LTime t    -> pure $ ETerm (Literal (literal (mkTime t))) TTime

  AST_NegativeVar node -> do
    name <- view (ix node)
    EType ty <- pure $ typeFromPact (_aTy node)
    case ty of
      TInt     -> pure (ETerm (IntUnaryArithOp Negate (Var name)) TInt)
      TDecimal -> pure (ETerm (DecUnaryArithOp Negate (Var name)) TDecimal)
      _ -> undefined

  AST_Enforce _ cond _msg -> do
    ETerm condTerm TBool <- translateNode cond
    pure $ ETerm (Enforce condTerm) TBool

  AST_EnforceKeyset ks
    | ks ^? aNode.aTy == Just (TyPrim TyString)
    -> do
      ETerm ksNameTerm TStr <- translateNode ks
      return $ ETerm (Enforce (NameAuthorized ksNameTerm)) TBool

  AST_Days days -> do
    ETerm days' daysTy <- translateNode days
    case daysTy of
      TInt     -> pure $ ETerm (IntArithOp Mul (1000000 * 60 * 60 * 24) days') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul (1000000 * 60 * 60 * 24) days') TDecimal
      _        -> throwError undefined

  AST_Hours hours -> do
    ETerm hours' hoursTy <- translateNode hours
    case hoursTy of
      TInt     -> pure $ ETerm (IntArithOp Mul (1000000 * 60 * 60) hours') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul (1000000 * 60 * 60) hours') TDecimal
      _        -> throwError undefined

  AST_Minutes minutes -> do
    ETerm minutes' minutes <- translateNode minutes
    case minutes of
      TInt     -> pure $ ETerm (IntArithOp Mul (1000000 * 60) minutes') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul (1000000 * 60) minutes') TDecimal
      _        -> throwError undefined

  AST_NFun _node "time" [AST_Lit (LString timeLit)]
    | Just timeLit'
      <- parseTime defaultTimeLocale Pact.simpleISO8601 (T.unpack timeLit)
    -> pure $ ETerm (Literal (literal (mkTime timeLit'))) TTime

  AST_NFun_Basic fn args ->
    let mkComparison :: TranslateM ETerm
        mkComparison = case args of
          [a, b] -> do
            ETerm a' ta <- translateNode a
            ETerm b' tb <- translateNode b
            op <- case fn of
              ">"  -> pure Gt
              "<"  -> pure Lt
              ">=" -> pure Gte
              "<=" -> pure Lte
              "="  -> pure Eq
              "!=" -> pure Neq
              _ -> throwError $ MalformedComparison fn args
            case typeEq ta tb of
              Just Refl -> pure $ ETerm (Comparison op a' b') TBool
              _         -> throwError (TypeMismatch (EType ta) (EType tb))
          _ -> throwError $ MalformedComparison fn args

        mkLogical :: TranslateM ETerm
        mkLogical = case args of
          [a] -> do
            ETerm a' TBool <- translateNode a
            case fn of
              "not" -> pure $ ETerm (Logical NotOp [a']) TBool
              _     -> throwError $ MalformedComparison fn args
          [a, b] -> do
            ETerm a' TBool <- translateNode a
            ETerm b' TBool <- translateNode b
            case fn of
              "and" -> pure $ ETerm (Logical AndOp [a', b']) TBool
              "or"  -> pure $ ETerm (Logical OrOp [a', b']) TBool
              _ -> throwError $ MalformedLogicalOp fn args
          _ -> throwError $ MalformedLogicalOp fn args

        mkArith :: TranslateM ETerm
        mkArith = case args of
          [a, b] -> do
            ETerm a' aTy <- translateNode a
            ETerm b' bTy <- translateNode b
            if
              | fn `Set.member` Set.fromList ["+", "-", "*", "/", "^"]
                -> let opFromNameID = \case
                         ("+" :: Text) -> Add
                         "-"           -> Sub
                         "*"           -> Mul
                         "/"           -> Div
                         "^"           -> Pow
                         "log"         -> Log

                       opFromNameDec = \case
                         ("+" :: Text) -> Add
                         "-"           -> Sub
                         "*"           -> Mul
                         "/"           -> Div
                         "^"           -> Pow
                         "log"         -> Log
                 in case (aTy, bTy) of
                   (TInt, TInt)         -> pure $ ETerm (IntArithOp (opFromNameID fn) a' b') TInt
                   (TDecimal, TDecimal) -> pure $ ETerm (DecArithOp (opFromNameID fn) a' b') TDecimal
                   (TInt, TDecimal)     -> pure $ ETerm (IntDecArithOp (opFromNameDec fn) a' b') TDecimal
                   (TDecimal, TInt)     -> pure $ ETerm (DecIntArithOp (opFromNameDec fn) a' b') TDecimal
                   _ -> throwError $ MalformedArithOp fn args
              | otherwise -> case (aTy, bTy, fn) of
                (TDecimal, TInt, "round")   -> pure $ ETerm (RoundingLikeOp2 Round a' b') TDecimal
                (TDecimal, TInt, "ceiling") -> pure $ ETerm (RoundingLikeOp2 Ceiling a' b') TDecimal
                (TDecimal, TInt, "floor")   -> pure $ ETerm (RoundingLikeOp2 Floor a' b') TDecimal
                _ -> throwError $ MalformedArithOp fn args
          [a] -> do
            ETerm a' ty <- translateNode a
            case (fn, ty) of
              ("-",    TInt) -> pure $ ETerm (IntUnaryArithOp Negate a') TInt
              ("sqrt", TInt) -> pure $ ETerm (IntUnaryArithOp Sqrt a') TInt
              ("ln",   TInt) -> pure $ ETerm (IntUnaryArithOp Ln a') TInt
              ("exp",  TInt) -> pure $ ETerm (IntUnaryArithOp Exp a') TInt
              ("abs",  TInt) -> pure $ ETerm (IntUnaryArithOp Abs a') TInt

              ("-",    TDecimal) -> pure $ ETerm (DecUnaryArithOp Negate a') TDecimal
              ("sqrt", TDecimal) -> pure $ ETerm (DecUnaryArithOp Sqrt a') TDecimal
              ("ln",   TDecimal) -> pure $ ETerm (DecUnaryArithOp Ln a') TDecimal
              ("exp",  TDecimal) -> pure $ ETerm (DecUnaryArithOp Exp a') TDecimal
              ("abs",  TDecimal) -> pure $ ETerm (DecUnaryArithOp Abs a') TDecimal

              ("round",   TDecimal) -> pure $ ETerm (RoundingLikeOp1 Round a') TInt
              ("ceiling", TDecimal) -> pure $ ETerm (RoundingLikeOp1 Ceiling a') TInt
              ("floor",   TDecimal) -> pure $ ETerm (RoundingLikeOp1 Floor a') TInt
              _         -> throwError $ MalformedArithOp fn args
          _ -> throwError $ MalformedArithOp fn args

        mkConcat :: TranslateM ETerm
        mkConcat = case (fn, args) of
          ("+", [a, b]) -> do
            ETerm a' TStr <- translateNode a
            ETerm b' TStr <- translateNode b
            pure (ETerm (Concat a' b') TStr)
          _ -> mzero

        mkMod :: TranslateM ETerm
        mkMod = case (fn, args) of
          ("mod", [a, b]) -> do
            ETerm a' TInt <- translateNode a
            ETerm b' TInt <- translateNode b
            pure (ETerm (ModOp a' b') TInt)
          _ -> mzero

    in mkMod <|> mkArith <|> mkComparison <|> mkLogical <|> mkConcat

  AST_NFun _node name [ShortTableName tn, row, obj]
    | elem name ["insert", "update", "write"] -> do
    ETerm row' TStr <- translateNode row
    EObject obj' _schema <- translateNode obj
    case schemaFromPact (_aTy (_aNode obj)) of
      Left err -> throwError err
      Right schema -> pure $ ETerm (Write (TableName (T.unpack tn)) row' obj') TStr

  AST_If _ cond tBranch fBranch -> do
    ETerm cond' TBool <- translateNode cond
    ETerm a ta <- translateNode tBranch
    ETerm b tb <- translateNode fBranch
    case typeEq ta tb of
      Just Refl -> pure $ ETerm (IfThenElse cond' a b) ta
      _ -> throwError (BranchesDifferentTypes (EType ta) (EType tb))

  AST_NFun _node "pact-version" [] -> pure $ ETerm PactVersion TStr

  {-
  AST_WithRead _node table key bindings body -> do
    let bindings' :: [(Text, String)]
        bindings' = flip map bindings $ \(Named name _ _, _var) -> name
    ETerm body' tbody <- translateBody body
    ETerm key' TStr   <- translateNode key
    -- let withRead = WithRead (TableName (T.unpack table)) key' bindings' body'
    let freshVar = undefined
    let body'' = foldr
          (\(colName, varName) body -> Let varName (At colName freshVar) body)
          body'
          bindings'
    pure $ Let freshVar (Read (TableName (T.unpack table)) _schema key') body''
-}

  -- Time
  -- Tricky: seconds could be either integer or decimal
  AST_AddTime time seconds
    | seconds ^. aNode . aTy == TyPrim TyInteger ||
      seconds ^. aNode . aTy == TyPrim TyDecimal -> do
      ETerm time' TTime <- translateNode time
      seconds'          <- translateNode seconds
      pure (ETerm (AddTime time' seconds') TTime)

  AST_Read node table key -> do
    ETerm key' TStr <- translateNode key
    schema <- case schemaFromPact (_aTy node) of
      Left err -> throwError err
      Right x -> pure x
    pure (EObject (Read (TableName (T.unpack table)) schema key') schema)

  AST_At node colName obj -> do
    EObject obj' schema <- translateNode obj
    ETerm colName' TStr <- translateNode colName
    -- let colName' = Literal $ literal $ T.unpack colName
    pure $ case typeFromPact (_aTy node) of
      ty@(EType ty')         -> ETerm   (At schema colName' obj' ty) ty'
      ty@(EObjectTy schema') -> EObject (At schema colName' obj' ty) schema'

  AST_Obj node kvs -> do
    kvs' <- for kvs $ \(k, v) -> do
      let k' = case k of
                 AST_Lit (LString t) -> T.unpack t
                 -- TODO: support non-const keys
                 _ -> undefined
      let ty = case v ^. aNode . aTy of
                 TyPrim TyBool    -> EType TBool
                 TyPrim TyDecimal -> EType TDecimal
                 TyPrim TyInteger -> EType TInt
                 TyPrim TyString  -> EType TStr
                 TyPrim TyTime    -> EType TTime
      v' <- translateNode v
      pure (k', (ty, v'))
    schema <- case schemaFromPact (_aTy node) of
      Left err -> throwError err
      Right x -> pure x
    pure $ EObject (LiteralObject $ Map.fromList kvs') schema

  --
  -- TODO: more cases.
  --

  ast -> throwError (UnexpectedNode "translateNode" ast)

typeFromPact :: Pact.Type Pact.UserType -> EType
typeFromPact ty = case ty of
  TyUser (Pact.Schema _ _ fields _) -> EObjectTy $ Schema $ Map.fromList $ flip map fields $
    \(Arg name ty _info) -> case ty of
      TyPrim TyBool    -> (T.unpack name, EType TBool)
      TyPrim TyDecimal -> (T.unpack name, EType TDecimal)
      TyPrim TyInteger -> (T.unpack name, EType TInt)
      TyPrim TyString  -> (T.unpack name, EType TStr)
      TyPrim TyTime    -> (T.unpack name, EType TTime)
      -- TODO: opaque data

  -- TODO(joel): understand the difference between the TyUser and TySchema cases
  TySchema _ ty' -> typeFromPact ty'

  TyPrim TyBool    -> EType TBool
  TyPrim TyDecimal -> EType TDecimal
  TyPrim TyInteger -> EType TInt
  TyPrim TyString  -> EType TStr
  TyPrim TyTime    -> EType TTime

schemaFromPact :: Pact.Type Pact.UserType -> Either TranslateFailure Schema
schemaFromPact ty = case typeFromPact ty of
  EType _primTy    -> Left $ NotConvertibleToSchema ty
  EObjectTy schema -> Right schema

{-# language GADTs               #-}
{-# language LambdaCase          #-}
{-# language MultiWayIf          #-}
{-# language OverloadedStrings   #-}
{-# language Rank2Types          #-}
{-# language ScopedTypeVariables #-}
module Pact.Analyze.Translate where

import Control.Lens hiding (op, (.>))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import qualified Data.Map as Map
import Pact.Types.Lang hiding (Term, TableName, TObject, EObject)
import qualified Pact.Types.Lang as Lang
import Pact.Types.Typecheck hiding (Var, Schema)
import qualified Pact.Types.Typecheck as TC
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError, name)
import qualified Data.Text as T
import Data.Type.Equality
import qualified Pact.Types.Lang as Pact
import qualified Pact.Types.Typecheck as Pact

import Pact.Analyze.Patterns
import Pact.Analyze.Types

translateBody :: [AST Node] -> TranslateM ETerm
translateBody [] = throwError EmptyBody
translateBody [ast] = translateNode ast
translateBody (ast:asts) = do
  ETerm ast' _   <- translateNode ast
  ETerm asts' ty <- translateBody asts
  pure $ ETerm (Sequence ast' asts') ty

nodeToTy :: Node -> EType
nodeToTy node = case node ^. aTy of
  TyPrim TyBool    -> EType TBool
  TyPrim TyDecimal -> EType TDecimal
  TyPrim TyInteger -> EType TInt
  TyPrim TyString  -> EType TStr
  TyPrim TyTime    -> EType TTime
  _                -> error "EType not yet supported"

translateNode :: AST Node -> TranslateM ETerm
translateNode = \case
  AST_Let _ [] body -> translateBody body

  AST_Let node ((Named _ varNode _, rhs):bindingsRest) body -> do
    let varName = varNode ^. aId ^. tiName
    local (at varNode ?~ varName) $ do
      --
      -- TODO: do we only want to allow subsequent bindings to reference
      --       earlier ones if we know it's let* rather than let? or has this
      --       been enforced by earlier stages for us?
      --
      ETerm rhs' _ <- translateNode rhs
      let subAst = AST_Let node bindingsRest body
      ETerm body' bodyTy <- translateNode subAst
      pure $ ETerm (Let varName rhs' body') bodyTy

  AST_Var node -> do
    varName <- view (ix node)
    case nodeToTy node of
      EType ty -> pure $ ETerm (Var varName) ty

  -- Int
  AST_NegativeLit (LInteger i)  -> do
    let tm = Arith Negate [Literal (literal i)]
    -- TODO: this should also work for decimal
    pure $ ETerm tm TInt

  AST_Lit         (LInteger i)  ->
    -- TODO: shouldn't this also work for decimal?
    pure (ETerm (Literal (literal i)) TInt)

  AST_NegativeVar n -> do
    name <- view (ix n)
    -- TODO: shouldn't this also work for decimal?
    pure (ETerm (Arith Negate [Var name]) TInt)

  AST_Days days -> do
    -- TODO: this should also work for decimal
    ETerm days' TInt <- translateNode days
    pure $ ETerm (Arith Mul [60 * 60 * 24, days']) TInt

  -- Bool
  AST_Lit (LBool b)     -> pure (ETerm (Literal (literal b)) TBool)

  AST_Enforce _ cond _msg -> do
    ETerm condTerm TBool <- translateNode cond
    pure $ ETerm (Enforce condTerm) TBool

  AST_EnforceKeyset ks
    | ks ^? aNode.aTy == Just (TyPrim TyString)
    -> do
      ETerm ksNameTerm TStr <- translateNode ks
      return $ ETerm (Enforce (NameAuthorized ksNameTerm)) TBool

  AST_NFun_Basic fn args -> do
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
              _         -> throwError (TypesDontMatch (EType ta) (EType tb))
          _ -> throwError $ MalformedComparison fn args

        mkLogical :: TranslateM ETerm
        mkLogical = case args of
          [a] -> do
            ETerm a' TBool <- translateNode a
            case fn of
              "not" -> pure $ ETerm (Logical NotOp [a']) TBool
              _ -> throwError $ MalformedComparison fn args
          [a, b] -> do
            ETerm a' TBool <- translateNode a
            ETerm b' TBool <- translateNode b
            case fn of
              "and" -> pure $ ETerm (Logical AndOp [a', b']) TBool
              "or" -> pure $ ETerm (Logical OrOp [a', b']) TBool
              _ -> throwError $ MalformedLogicalOp fn args
          _ -> throwError $ MalformedLogicalOp fn args

        -- TODO(joel): do this for decimal (etc) as well
        mkArith :: TranslateM ETerm
        mkArith = case args of
          [a, b] -> do
            ETerm a' TInt <- translateNode a
            ETerm b' TInt <- translateNode b
            -- case typeEq ta tb of
            --   Just Refl -> pure $ ETerm (Arith Sub [a', b']) TInt
            --   _         -> throwError (TypesDontMatch (EType ta) (EType tb))
            case fn of
              "-" -> pure $ ETerm (Arith Sub [a', b']) TInt
              "+" -> pure $ ETerm (Arith Add [a', b']) TInt
              "*" -> pure $ ETerm (Arith Mul [a', b']) TInt
              "/" -> pure $ ETerm (Arith Div [a', b']) TInt
              "^" -> pure $ ETerm (Arith Pow [a', b']) TInt
              "mod" -> pure $ ETerm (Arith Mod [a', b']) TInt
              "log" -> pure $ ETerm (Arith Log [a', b']) TInt
              _ -> throwError $ MalformedLogicalOp fn args
          [a] -> do
            ETerm a' TInt <- translateNode a
            case fn of
              "-" -> pure $ ETerm (Arith Negate [a']) TInt
              "sqrt" -> pure $ ETerm (Arith Sqrt [a']) TInt
              "ln" -> pure $ ETerm (Arith Ln [a']) TInt
              "exp" -> pure $ ETerm (Arith Exp [a']) TInt
              "abs" -> pure $ ETerm (Arith Abs [a']) TInt
              "round" -> pure $ ETerm (Arith Round [a']) TInt
              "ceiling" -> pure $ ETerm (Arith Ceiling [a']) TInt
              "floor" -> pure $ ETerm (Arith Floor [a']) TInt
              _ -> throwError $ MalformedArithOp fn args
          _ -> throwError $ MalformedArithOp fn args

    if
      -- integer, decimal, string, and time are all comparable. Use the type of
      -- the first argument to decide which to use.
      | isComparison fn -> mkComparison
      | isLogical    fn -> mkLogical
      | isArith      fn -> mkArith

  AST_NFun _node name [Table _tnode (Lang.TableName tn), row, _obj]
    | elem name ["insert", "update", "write"] -> do
    ETerm row' TStr <- translateNode row
    pure $ ETerm (Write (TableName (T.unpack tn)) row' undefined) TStr

  AST_If _ cond tBranch fBranch -> do
    ETerm cond' TBool <- translateNode cond
    ETerm a ta <- translateNode tBranch
    ETerm b tb <- translateNode fBranch
    case typeEq ta tb of
      Just Refl -> pure $ ETerm (IfThenElse cond' a b) ta
      _ -> throwError (BranchesDifferentTypes (EType ta) (EType tb))

  -- String
  AST_Lit (LString t) -> pure $ ETerm (Literal $ literal $ T.unpack t) TStr

  AST_NFun_Basic "+" [a, b] -> do
    ETerm a' TStr <- translateNode a
    ETerm b' TStr <- translateNode b
    pure (ETerm (Concat a' b') TStr)

  AST_NFun _node "pact-version" [] -> pure $ ETerm PactVersion TStr

  AST_WithRead _node table key bindings body -> do
    let bindings' = flip map bindings $ \(Named name _ _, _var) -> name
    ETerm body' tbody <- translateBody body
    ETerm key' TStr <- translateNode key
    let withRead = WithRead (TableName (T.unpack table)) key' bindings' body'
    pure $ ETerm withRead tbody

  -- Decimal
  AST_Lit (LDecimal d) -> pure (ETerm (Literal (literal (mkDecimal d))) TDecimal)

  -- Time
  -- Tricky: seconds could be either integer or decimal
  AST_AddTime time seconds
    | seconds ^. aNode . aTy == TyPrim TyInteger -> do
      ETerm time' TTime <- translateNode time
      ETerm seconds' TInt <- translateNode seconds
      pure (ETerm (AddTimeInt time' seconds') TTime)
    | seconds ^. aNode . aTy == TyPrim TyDecimal -> do
      ETerm time' TTime <- translateNode time
      ETerm seconds' TDecimal <- translateNode seconds
      pure (ETerm (AddTimeDec time' seconds') TTime)

  AST_Lit (LTime t) -> pure (ETerm (Literal (literal (mkTime t))) TTime)

  AST_Read node table key -> do
    ETerm key' TStr <- translateNode key
    schema <- case schemaFromPact (_aTy node) of
      Left err -> throwError err
      Right x -> pure x
    pure (EObject (Read (TableName (T.unpack table)) schema key') TObject)

  AST_At colName obj -> do
    EObject obj' TObject <- translateNode obj
    -- XXX return correct type
    pure $ ETerm (At (T.unpack colName) obj') TInt

  --
  -- TODO: more cases.
  --

  ast -> throwError (UnexpectedNode "translateNode" ast)

schemaFromPact :: Pact.Type Pact.UserType -> Either AnalyzeFailure Schema
schemaFromPact ty = case ty of
  TyUser (TC.Schema _ _ fields _) -> Right $ Schema $ Map.fromList $ flip map fields $
    \(Arg name ty _info) -> case ty of
      TyPrim TyBool    -> (T.unpack name, EType TBool)
      TyPrim TyDecimal -> (T.unpack name, EType TDecimal)
      TyPrim TyInteger -> (T.unpack name, EType TInt)
      TyPrim TyString  -> (T.unpack name, EType TStr)
      TyPrim TyTime    -> (T.unpack name, EType TTime)

  TySchema _ ty' -> schemaFromPact ty'

  ty -> Left $ NotConvertibleToSchema ty

{-# language LambdaCase          #-}
{-# language MultiWayIf          #-}
{-# language OverloadedStrings   #-}
{-# language Rank2Types          #-}
{-# language ScopedTypeVariables #-}
module Pact.Analyze.Translate where

import Control.Lens hiding (op, (.>), (...))
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import Pact.Types.Lang hiding (Term, TableName)
import qualified Pact.Types.Lang as Lang
import Pact.Types.Typecheck hiding (Var)
import Data.SBV hiding (Satisfiable, Unsatisfiable, Unknown, ProofError, name)
import qualified Data.Text as T

import Pact.Analyze.K
import Pact.Analyze.Patterns
import Pact.Analyze.Types

translateBody
  :: forall a. (Show a, SymWord a)
  => K (Either String (Term a))
  -> [AST Node]
  -> TranslateM (Term a)
translateBody _k [] = throwError EmptyBody
translateBody k [ast] = translateNode' k ast
translateBody k (ast:asts) = join $ flip translateNode ast $ uniformK
  (\a -> translateBody (preMap (Sequence a) k) asts)

-- TODO: can we get rid of translateNode'?
translateNode' :: forall a. K (Either String a) -> AST Node -> TranslateM a
translateNode' k node = do
  x <- translateNode k node
  case x of
    Left e   -> throwError (UnexpectedNode e node)
    Right x' -> pure x'

kApplyCase
  :: Node
  -> AST Node
  -> (Term Bool    -> TranslateM b)
  -> (Term Decimal -> TranslateM b)
  -> (Term Integer -> TranslateM b)
  -> (Term String  -> TranslateM b)
  -> (Term Time    -> TranslateM b)
  -> TranslateM b
kApplyCase node term fb fd fi fs ft = case node ^. aTy of
  TyPrim TyBool -> do
    Right term' <- translateNode kExpectBool term
    fb term'
  TyPrim TyDecimal -> do
    Right term' <- translateNode kExpectDecimal term
    fd term'
  TyPrim TyInteger -> do
    Right term' <- translateNode kExpectInt term
    fi term'
  TyPrim TyString -> do
    Right term' <- translateNode kExpectStr term
    fs term'
  TyPrim TyTime -> do
    Right term' <- translateNode kExpectTime term
    ft term'

translateNode :: forall a. K a -> AST Node -> TranslateM a
translateNode k = \case
  AST_Let node [] body -> do
    case node ^. aTy of
      TyPrim TyBool -> do
        body' <- translateBody kExpectBool body
        pure $ kApplyBool k body'
      TyPrim TyDecimal -> do
        body' <- translateBody kExpectDecimal body
        pure $ kApplyDecimal k body'
      TyPrim TyInteger -> do
        body' <- translateBody kExpectInt body
        pure $ kApplyInt k body'
      TyPrim TyString -> do
        body' <- translateBody kExpectStr body
        pure $ kApplyStr k body'
      TyPrim TyTime -> do
        body' <- translateBody kExpectTime body
        pure $ kApplyTime k body'

  AST_Let node ((Named _ varNode _, rhs):bindingsRest) body -> do
    let varName = varNode ^. aId ^. tiName
    local (at varNode ?~ varName) $ do
      --
      -- TODO: do we only want to allow subsequent bindings to reference
      --       earlier ones if we know it's let* rather than let? or has this
      --       been enforced by earlier stages for us?
      --
      let subAst = AST_Let node bindingsRest body
      kApplyCase varNode rhs
        (\val ->
          kApplyCase node subAst
            (\rest -> pure $ kApplyBool    k (Let varName val rest))
            (\rest -> pure $ kApplyDecimal k (Let varName val rest))
            (\rest -> pure $ kApplyInt     k (Let varName val rest))
            (\rest -> pure $ kApplyStr     k (Let varName val rest))
            (\rest -> pure $ kApplyTime    k (Let varName val rest))
        )
        (\val ->
          kApplyCase node subAst
            (\rest -> pure $ kApplyBool    k (Let varName val rest))
            (\rest -> pure $ kApplyDecimal k (Let varName val rest))
            (\rest -> pure $ kApplyInt     k (Let varName val rest))
            (\rest -> pure $ kApplyStr     k (Let varName val rest))
            (\rest -> pure $ kApplyTime    k (Let varName val rest))
        )
        (\val ->
          kApplyCase node subAst
            (\rest -> pure $ kApplyBool    k (Let varName val rest))
            (\rest -> pure $ kApplyDecimal k (Let varName val rest))
            (\rest -> pure $ kApplyInt     k (Let varName val rest))
            (\rest -> pure $ kApplyStr     k (Let varName val rest))
            (\rest -> pure $ kApplyTime    k (Let varName val rest))
        )
        (\val ->
          kApplyCase node subAst
            (\rest -> pure $ kApplyBool    k (Let varName val rest))
            (\rest -> pure $ kApplyDecimal k (Let varName val rest))
            (\rest -> pure $ kApplyInt     k (Let varName val rest))
            (\rest -> pure $ kApplyStr     k (Let varName val rest))
            (\rest -> pure $ kApplyTime    k (Let varName val rest))
        )
        (\val ->
          kApplyCase node subAst
            (pure . kApplyBool    k . Let varName val)
            (pure . kApplyDecimal k . Let varName val)
            (pure . kApplyInt     k . Let varName val)
            (pure . kApplyStr     k . Let varName val)
            (pure . kApplyTime    k . Let varName val)
        )

  AST_Var node -> do
    varName <- view (ix node)
    -- traceShowM ("AST_Var node", varName, node)
    pure $ kApplyUniform node k (Var varName)

  -- Int
  AST_NegativeLit (LInteger i)  -> kApplyInt k . Arith Negate . pure <$>
    pure (Literal (literal i))
  AST_Lit         (LInteger i)  -> pure (kApplyInt k (Literal (literal i)))
  AST_NegativeVar n -> do
    name <- view (ix n)
    pure $ kApplyInt k $ Arith Negate [Var name]
  AST_Days days -> do
    days' <- translateNode' kExpectInt days
    pure $ kApplyInt k $ Arith Mul [60 * 60 * 24, days']

  -- Bool
  AST_Lit (LBool b)     -> pure (kApplyBool k (Literal (literal b)))

  AST_Enforce _ cond _msg -> do
    condTerm <- translateNode' kExpectBool cond
    return $ kApplyBool k $ Enforce condTerm

  AST_EnforceKeyset ks
    | ks ^? aNode.aTy == Just (TyPrim TyString)
    -> do
      ksNameTerm <- translateNode' kExpectStr ks
      return $ kApplyBool k $ Enforce $ NameAuthorized ksNameTerm

  AST_NFun_Basic fn args -> do
    let mkComparison
          :: (Show b, SymWord b)
          => (AST Node -> TranslateM (Term b))
          -> TranslateM a
        mkComparison translate = fmap (kApplyBool k) $ case (fn, args) of
          -- TODO: this could compare integer, decimal, string, or time. use the node
          -- to decide which to dispatch to
          (">", [a, b]) -> Comparison Gt
            <$> translate a
            <*> translate b
          ("<", [a, b]) -> Comparison Lt
            <$> translate a
            <*> translate b
          ("<=", [a, b]) -> Comparison Lte
            <$> translate a
            <*> translate b
          (">=", [a, b]) -> Comparison Gte
            <$> translate a
            <*> translate b
          ("=", [a, b]) -> Comparison Eq
            <$> translate a
            <*> translate b
          ("!=", [a, b]) -> Comparison Neq
            <$> translate a
            <*> translate b
          _ -> throwError $ MalformedComparison fn args

        mkLogical :: TranslateM a
        mkLogical = fmap (kApplyBool k) $ case (fn, args) of
          ("and", [a, b]) -> do
            a' <- translateNode' kExpectBool a
            b' <- translateNode' kExpectBool b
            pure $ Logical AndOp [a', b']
          ("or", [a, b]) -> do
            a' <- translateNode' kExpectBool a
            b' <- translateNode' kExpectBool b
            pure $ Logical OrOp [a', b']
          ("not", [a]) -> do
            a' <- translateNode' kExpectBool a
            pure $ Logical NotOp [a']
          _ -> throwError $ MalformedLogicalOp fn args

        -- TODO(joel): do this for decimal (etc) as well
        mkArith :: TranslateM a
        mkArith = fmap (kApplyInt k) $ case (fn, args) of
          ("-", [a, b]) -> do
            a' <- translateNode' kExpectInt a
            b' <- translateNode' kExpectInt b
            pure $ Arith Sub [a', b']
          ("-", [a]) -> Arith Negate . pure <$> translateNode' kExpectInt a
          ("+", [a, b]) -> do
            a' <- translateNode' kExpectInt a
            b' <- translateNode' kExpectInt b
            pure $ Arith Add [a', b']
          ("*", [a, b]) -> do
            a' <- translateNode' kExpectInt a
            b' <- translateNode' kExpectInt b
            pure $ Arith Mul [a', b']
          ("/", [a, b]) -> do
            a' <- translateNode' kExpectInt a
            b' <- translateNode' kExpectInt b
            pure $ Arith Div [a', b']
          ("^", [a, b]) -> do
            a' <- translateNode' kExpectInt a
            b' <- translateNode' kExpectInt b
            pure $ Arith Pow [a', b']
          ("sqrt", [a]) -> Arith Sqrt . pure <$> translateNode' kExpectInt a
          ("mod", [a, b]) -> do
            a' <- translateNode' kExpectInt a
            b' <- translateNode' kExpectInt b
            pure $ Arith Mod [a', b']
          ("log", [a, b]) -> do
            a' <- translateNode' kExpectInt a
            b' <- translateNode' kExpectInt b
            pure $ Arith Log [a', b']
          ("ln", [a]) -> Arith Ln . pure <$> translateNode' kExpectInt a
          ("exp", [a]) -> Arith Pow . pure <$> translateNode' kExpectInt a
          ("abs", [a]) -> Arith Abs . pure <$> translateNode' kExpectInt a

          ("round", [a]) -> Arith Round . pure <$> translateNode' kExpectInt a
          ("ceiling", [a]) -> Arith Ceiling . pure <$> translateNode' kExpectInt a
          ("floor", [a]) -> Arith Floor . pure <$> translateNode' kExpectInt a
          _ -> throwError $ MalformedArithOp fn args

    if
      -- integer, decimal, string, and time are all comparable. Use the type of
      -- the first argument to decide which to use.
      | isComparison fn -> case args ^? ix 0 . aNode . aTy of
        Just (TyPrim TyInteger) -> mkComparison (translateNode' kExpectInt)
        Just (TyPrim TyDecimal) -> mkComparison (translateNode' kExpectDecimal)
        Just (TyPrim TyString)  -> mkComparison (translateNode' kExpectStr)
        Just (TyPrim TyTime)    -> mkComparison (translateNode' kExpectTime)
        _ -> throwError $ MalformedComparison fn args
      | isLogical fn -> mkLogical
      | isArith   fn -> mkArith

  AST_NFun _node name [Table _tnode (Lang.TableName tn), row, _obj]
    | elem name ["insert", "update", "write"]
    -> kApplyStr k . Write (TableName . T.unpack $ tn) <$> translateNode' kExpectStr row

  AST_If node cond tBranch fBranch -> case node ^. aTy of
    -- TODO(joel): express this more succinctly

    TyPrim TyBool    -> do
      ite' <- IfThenElse
        <$> translateNode' kExpectBool cond
        <*> translateNode' kExpectBool tBranch
        <*> translateNode' kExpectBool fBranch
      pure $ kApplyBool k ite'

    TyPrim TyDecimal -> do
      ite' <- IfThenElse
        <$> translateNode' kExpectBool cond
        <*> translateNode' kExpectDecimal tBranch
        <*> translateNode' kExpectDecimal fBranch
      pure $ kApplyDecimal k ite'

    TyPrim TyInteger -> do
      ite' <- IfThenElse
        <$> translateNode' kExpectBool cond
        <*> translateNode' kExpectInt tBranch
        <*> translateNode' kExpectInt fBranch
      pure $ kApplyInt k ite'

    TyPrim TyString  -> do
      ite' <- IfThenElse
        <$> translateNode' kExpectBool cond
        <*> translateNode' kExpectStr tBranch
        <*> translateNode' kExpectStr fBranch
      pure $ kApplyStr k ite'

    TyPrim TyTime    -> do
      ite' <- IfThenElse
        <$> translateNode' kExpectBool cond
        <*> translateNode' kExpectTime tBranch
        <*> translateNode' kExpectTime fBranch
      pure $ kApplyTime k ite'

    -- TODO
    _ -> error "unimplemented type analysis"

  -- String
  AST_Lit (LString t) -> pure $ kApplyStr k $ Literal $ literal $ T.unpack t
  AST_NFun_Basic "+" [a, b] -> kApplyStr k ... Concat
    <$> translateNode' kExpectStr a
    <*> translateNode' kExpectStr b

  AST_NFun _node "pact-version" [] -> pure $ kApplyStr k PactVersion

  AST_WithRead _node table key bindings body -> do
    -- traceShowM ("bindings", bindings)
    let bindings' = flip map bindings $ \(Named name _ _, _var) -> name
    -- TODO: this should not be specialized to just String
    body' <- translateBody kExpectStr body
    key'  <- translateNode' kExpectStr key
    let withRead = WithRead (TableName . T.unpack $ table) key' bindings' body'
    pure $ kApplyStr k withRead

  -- Decimal
  AST_Lit (LDecimal d) -> pure (kApplyDecimal k (Literal (literal (mkDecimal d))))

  -- Time
  -- Tricky: seconds could be either integer or decimal
  AST_AddTime time seconds
    | seconds ^. aNode . aTy == TyPrim TyInteger -> kApplyTime k ... AddTimeInt
      <$> translateNode' kExpectTime time
      <*> translateNode' kExpectInt seconds
    | seconds ^. aNode . aTy == TyPrim TyDecimal -> kApplyTime k ... AddTimeDec
      <$> translateNode' kExpectTime time
      <*> translateNode' kExpectDecimal seconds

  AST_Lit (LTime t) -> pure (kApplyTime k (Literal (literal (mkTime t))))

  --
  -- TODO: more cases.
  --

  ast -> throwError (UnexpectedNode "translateNode" ast)

infixr 9 ...
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = ((.) . (.))

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MonadFailDesugaring        #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Pact.Analyze.Translate where

import           Control.Applicative        (Alternative, (<|>))
import           Control.Lens               (at, ix, view, (%=), (<&>), (?~),
                                             (^.), (^?), _3)
import           Control.Monad              (MonadPlus (mzero))
import           Control.Monad.Except       (Except, MonadError, throwError)
import           Control.Monad.Fail         (MonadFail (fail))
import           Control.Monad.Reader       (MonadReader (local), ReaderT)
import           Control.Monad.State.Strict (MonadState (..), StateT)
import           Data.Foldable              (foldl')
import qualified Data.Map                   as Map
import           Data.Map.Strict            (Map)
import           Data.Monoid                ((<>))
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Thyme                 (parseTime)
import           Data.Traversable           (for)
import           Data.Type.Equality         ((:~:) (Refl))
import           Pact.Types.Lang            (Arg (..), Literal (..),
                                             PrimType (..), Type (..))
import qualified Pact.Types.Lang            as Pact
import           Pact.Types.Typecheck       (AST, Named (Named), Node, aId,
                                             aNode, aTy, tiName, _aTy)
import qualified Pact.Types.Typecheck       as Pact
import           System.Locale              (defaultTimeLocale)

import           Pact.Analyze.Patterns
import           Pact.Analyze.Term
import           Pact.Analyze.Types

data TranslateFailure
  = BranchesDifferentTypes EType EType
  | NonStringLitInBinding (AST Node)
  | EmptyBody
  | MalformedArithOp Text [AST Node]
  | MalformedLogicalOp Text [AST Node]
  | MalformedComparison Text [AST Node]
  | NotConvertibleToSchema (Pact.Type Pact.UserType)
  | TypeMismatch EType EType
  | UnexpectedNode (AST Node)
  | MissingConcreteType (Pact.Type Pact.UserType)
  | AlternativeFailures [TranslateFailure]
  | MonadFailure String
  | NonStaticColumns (AST Node)
  | BadNegationType (AST Node)
  | BadTimeType (AST Node)
  | NonConstKey (AST Node)
  -- For cases we don't handle yet:
  | UnhandledType Node (Pact.Type Pact.UserType)
  deriving Show

describeTranslateFailure :: TranslateFailure -> Text
describeTranslateFailure = \case
  BranchesDifferentTypes t1 t2 -> "two branches unexpectedly have different types: (" <> tShow t1 <> ") vs (" <> tShow t2 <> ")"
  NonStringLitInBinding ast -> "We only support analysis of binding forms (bind / with-read) binding string literals. Instead we found " <> tShow ast
  EmptyBody -> "can't translate an empty body"
  MalformedArithOp op args -> "Unsupported arithmetic op " <> op <> " with args " <> tShow args
  MalformedLogicalOp op args -> "Unsupported logical op " <> op <> " with args " <> tShow args
  MalformedComparison op args -> "Unsupported comparison op " <> op <> " with args " <> tShow args
  NotConvertibleToSchema ty -> "Expected a schema, but found " <> tShow ty
  TypeMismatch ty1 ty2 -> "Type mismatch: (" <> tShow ty1 <> ") vs (" <> tShow ty2 <> ")"
  UnexpectedNode ast -> "Unexpected node in translation: " <> tShow ast
  MissingConcreteType ty -> "The typechecker should always produce a concrete type, but we found " <> tShow ty
  AlternativeFailures failures -> "Multiple failures: " <> T.unlines (mappend "  " . describeTranslateFailure <$> failures)
  MonadFailure str -> "Translation failure: " <> T.pack str
  NonStaticColumns col -> "When reading only certain columns we require all columns to be concrete in order to do analysis. We found " <> tShow col
  BadNegationType node -> "Invalid: negation of a non-integer / decimal: " <> tShow node
  BadTimeType node -> "Invalid: days / hours / minutes applied to non-integer / decimal: " <> tShow node
  NonConstKey k -> "Pact can currently only analyze constant keys in objects. Found " <> tShow k
  UnhandledType node ty -> "Found a type we don't know how to translate yet: " <> tShow ty <> " at node: " <> tShow node
  where
    tShow :: Show a => a -> Text
    tShow = T.pack . show

instance Monoid TranslateFailure where
  mempty = AlternativeFailures []
  mappend (AlternativeFailures xs) (AlternativeFailures ys) = AlternativeFailures (xs `mappend` ys)
  mappend (AlternativeFailures xs) x = AlternativeFailures (x:xs)
  mappend x (AlternativeFailures xs) = AlternativeFailures (x:xs)
  mappend x y = AlternativeFailures [x, y]

-- next fresh variable ID
newtype FreshId
  = FreshId Int
  deriving (Enum, Num)

newtype TranslateM a
  = TranslateM { unTranslateM :: ReaderT (Map Node Text) (StateT FreshId (Except TranslateFailure)) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
    MonadReader (Map Node Text), MonadState FreshId,
    MonadError TranslateFailure)

instance MonadFail TranslateM where
  fail s = throwError (MonadFailure s)

genFresh :: Text -> TranslateM (Text, Term a)
genFresh baseName = do
  varId <- get
  id %= succ
  -- similar to how let-bound variables are named e.g. "let4_x":
  let varName = "fresh" <> T.pack (show (fromEnum varId)) <> "_" <> baseName
  return (varName, Var varName)

translateType :: Node -> TranslateM EType
translateType node = go $ _aTy node
  where
    go :: Pact.Type Pact.UserType -> TranslateM EType
    go = \case
      TyUser (Pact.Schema _ _ fields _) ->
        fmap (EObjectTy . Schema) $ sequence $ Map.fromList $ fields <&>
          \(Arg name ty _info) -> (T.unpack name, go ty)

      -- TODO(joel): understand the difference between the TyUser and TySchema cases
      TySchema _ ty' -> go ty'

      TyPrim TyBool    -> pure $ EType TBool
      TyPrim TyDecimal -> pure $ EType TDecimal
      TyPrim TyInteger -> pure $ EType TInt
      TyPrim TyString  -> pure $ EType TStr
      TyPrim TyTime    -> pure $ EType TTime
      TyPrim TyKeySet  -> pure $ EType TKeySet

      -- Pretend any and var are the same -- we can't analyze either of them.
      TyAny            -> pure $ EType TAny
      TyVar _          -> pure $ EType TAny

      --
      -- TODO: handle these:
      --
      ty@(TyPrim TyValue)  -> throwError $ UnhandledType node ty
      ty@(TyList _)        -> throwError $ UnhandledType node ty
      ty@(TyFun _)         -> throwError $ UnhandledType node ty

translateSchema :: Node -> TranslateM Schema
translateSchema node = do
  ty <- translateType node
  case ty of
    EType _primTy    -> throwError $ NotConvertibleToSchema $ _aTy node
    EObjectTy schema -> pure schema

translateBody :: [AST Node] -> TranslateM ETerm
translateBody [] = throwError EmptyBody
translateBody [ast] = translateNode ast
translateBody (ast:asts) = do
  ast'  <- translateNode ast
  asts' <- translateBody asts
  pure $ case asts' of
    ETerm   astsT ty -> ETerm   (Sequence ast' astsT) ty
    EObject astsO ty -> EObject (Sequence ast' astsO) ty

translateBinding
  :: [(Named Node, AST Node)]
  -> Schema
  -> [AST Node]
  -> ETerm
  -> TranslateM ETerm
translateBinding bindingsA schema bodyA rhsT = do
  (bindings :: [(String, EType, (Node, Text))]) <- for bindingsA $
    \(Named _ varNode _, colAst) -> do
      let varName = varNode ^. aId.tiName
      varType <- translateType varNode
      case colAst of
        AST_StringLit colName ->
          pure (T.unpack colName, varType, (varNode, varName))
        _ ->
          throwError $ NonStringLitInBinding colAst

  (freshName, freshVar :: Term Object) <- genFresh "binding"

  let translateLet :: Term a -> Term a
      translateLet innerBody = Let freshName rhsT $
        -- NOTE: *left* fold for proper shadowing/overlapping name semantics:
        foldl'
          (\body (colName, varType, (_varNode, varName)) ->
            let colTerm = lit colName
            in Let varName
              (case varType of
                 EType ty ->
                   ETerm   (At schema colTerm freshVar varType) ty
                 EObjectTy sch ->
                   EObject (At schema colTerm freshVar varType) sch)
              body)
          innerBody
          bindings

      nodeNames = Map.fromList $ view _3 <$> bindings

  mapETerm translateLet <$> local (nodeNames <>) (translateBody bodyA)

translateNode :: AST Node -> TranslateM ETerm
translateNode astNode = case astNode of
  AST_Let _ [] body -> translateBody body

  AST_Let node ((Named _ varNode _, rhsNode):bindingsRest) body -> do
    rhsETerm <- translateNode rhsNode
    let varName = varNode ^. aId.tiName
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

  AST_InlinedApp body -> translateBody body

  AST_Var node -> do
    varName <- view (ix node)
    ty <- translateType node
    pure $ case ty of
      EType ty'        -> ETerm (Var varName) ty'
      EObjectTy schema -> EObject (Var varName) schema

  -- Int
  AST_NegativeLit l -> case l of
    LInteger i -> pure $ ETerm (IntUnaryArithOp Negate (lit i)) TInt
    LDecimal d -> pure $ ETerm (DecUnaryArithOp Negate (lit (mkDecimal d))) TDecimal
    _ -> throwError $ BadNegationType astNode

  AST_Lit l -> case l of
    LInteger i -> pure $ ETerm (lit i) TInt
    LBool b    -> pure $ ETerm (lit b) TBool
    LString s  -> pure $ ETerm (lit $ T.unpack s) TStr
    LDecimal d -> pure $ ETerm (lit (mkDecimal d)) TDecimal
    LTime t    -> pure $ ETerm (lit (mkTime t)) TTime

  AST_NegativeVar node -> do
    name <- view (ix node)
    EType ty <- translateType node
    case ty of
      TInt     -> pure (ETerm (IntUnaryArithOp Negate (Var name)) TInt)
      TDecimal -> pure (ETerm (DecUnaryArithOp Negate (Var name)) TDecimal)
      _        -> throwError $ BadNegationType astNode

  AST_Enforce _ cond -> do
    ETerm condTerm TBool <- translateNode cond
    pure $ ETerm (Enforce condTerm) TBool

  AST_ReadKeyset nameA -> do
    ETerm nameT TStr <- translateNode nameA
    return $ ETerm (ReadKeySet nameT) TKeySet

  AST_EnforceKeyset ksA
    | ksA ^? aNode.aTy == Just (TyPrim TyString)
    -> do
      ETerm ksnT TStr <- translateNode ksA
      return $ ETerm (Enforce (NameAuthorized ksnT)) TBool

  AST_EnforceKeyset ksA
    | ksA ^? aNode.aTy == Just (TyPrim TyKeySet)
    -> do
      ETerm ksT TKeySet <- translateNode ksA
      return $ ETerm (Enforce (KsAuthorized ksT)) TBool

  AST_Days days -> do
    ETerm days' daysTy <- translateNode days
    case daysTy of
      TInt     -> pure $ ETerm (IntArithOp Mul (1000000 * 60 * 60 * 24) days') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul (1000000 * 60 * 60 * 24) days') TDecimal
      _        -> throwError $ BadTimeType astNode

  AST_Hours hours -> do
    ETerm hours' hoursTy <- translateNode hours
    case hoursTy of
      TInt     -> pure $ ETerm (IntArithOp Mul (1000000 * 60 * 60) hours') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul (1000000 * 60 * 60) hours') TDecimal
      _        -> throwError $ BadTimeType astNode

  AST_Minutes minutes -> do
    ETerm minutes' minutesTy <- translateNode minutes
    case minutesTy of
      TInt     -> pure $ ETerm (IntArithOp Mul (1000000 * 60) minutes') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul (1000000 * 60) minutes') TDecimal
      _        -> throwError $ BadTimeType astNode

  AST_NFun _node "time" [AST_Lit (LString timeLit)]
    | Just timeLit'
      <- parseTime defaultTimeLocale Pact.simpleISO8601 (T.unpack timeLit)
    -> pure $ ETerm (lit (mkTime timeLit')) TTime

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
              _    -> throwError $ MalformedComparison fn args
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
              _     -> throwError $ MalformedLogicalOp fn args
          _ -> throwError $ MalformedLogicalOp fn args

        mkArith :: TranslateM ETerm
        mkArith = case args of
          [a, b] -> do
            ETerm a' tyA <- translateNode a
            ETerm b' tyB <- translateNode b
            if
              | fn `Set.member` Set.fromList ["+", "-", "*", "/", "^"]
                -> let opFromName = \case
                         ("+" :: Text) -> Add
                         "-"           -> Sub
                         "*"           -> Mul
                         "/"           -> Div
                         "^"           -> Pow
                         "log"         -> Log
                         _             -> error "impossible"
                 in case (tyA, tyB) of
                   (TInt, TInt)         -> pure $
                     ETerm (IntArithOp (opFromName fn) a' b') TInt
                   (TDecimal, TDecimal) -> pure $
                     ETerm (DecArithOp (opFromName fn) a' b') TDecimal
                   (TInt, TDecimal)     -> pure $
                     ETerm (IntDecArithOp (opFromName fn) a' b') TDecimal
                   (TDecimal, TInt)     -> pure $
                     ETerm (DecIntArithOp (opFromName fn) a' b') TDecimal
                   _ -> throwError $ MalformedArithOp fn args
              | otherwise -> case (tyA, tyB, fn) of
                (TDecimal, TInt, "round")   -> pure $
                  ETerm (RoundingLikeOp2 Round a' b') TDecimal
                (TDecimal, TInt, "ceiling") -> pure $
                  ETerm (RoundingLikeOp2 Ceiling a' b') TDecimal
                (TDecimal, TInt, "floor")   -> pure $
                  ETerm (RoundingLikeOp2 Floor a' b') TDecimal
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
    | name `elem` ["insert", "update", "write"] -> do
    ETerm row' TStr <- translateNode row
    EObject obj' _schema <- translateNode obj
    pure $ ETerm (Write (TableName (T.unpack tn)) row' obj') TStr

  AST_If _ cond tBranch fBranch -> do
    ETerm cond' TBool <- translateNode cond
    ETerm a ta <- translateNode tBranch
    ETerm b tb <- translateNode fBranch
    case typeEq ta tb of
      Just Refl -> pure $ ETerm (IfThenElse cond' a b) ta
      _         -> throwError (BranchesDifferentTypes (EType ta) (EType tb))

  AST_NFun _node "pact-version" [] -> pure $ ETerm PactVersion TStr

  AST_WithRead _ table key bindings schemaNode body -> do
    schema <- translateSchema schemaNode
    ETerm key' TStr <- translateNode key
    let readT = EObject (Read (TableName (T.unpack table)) schema key') schema
    translateBinding bindings schema body readT

  AST_Bind _ objectA bindings schemaNode body -> do
    schema <- translateSchema schemaNode
    objectT <- translateNode objectA
    translateBinding bindings schema body objectT

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
    schema <- translateSchema node
    pure (EObject (Read (TableName (T.unpack table)) schema key') schema)

  -- Note: this won't match if the columns are not a list literal
  AST_ReadCols node table key columns -> do
    ETerm key' TStr <- translateNode key
    schema <- translateSchema node
    columns' <- for columns $ \case
      AST_Lit (LString col) -> pure col
      bad                   -> throwError (NonStaticColumns bad)

    pure (EObject
      (ReadCols (TableName (T.unpack table)) schema key' columns')
      schema)

  AST_At node colName obj -> do
    EObject obj' schema <- translateNode obj
    ETerm colName' TStr <- translateNode colName
    ty <- translateType node
    pure $ case ty of
      EType ty'         -> ETerm   (At schema colName' obj' ty) ty'
      EObjectTy schema' -> EObject (At schema colName' obj' ty) schema'

  AST_Obj node kvs -> do
    kvs' <- for kvs $ \(k, v) -> do
      k' <- case k of
        AST_Lit (LString t) -> pure $ T.unpack t
        -- TODO: support non-const keys
        _                   -> throwError $ NonConstKey k
      ty <- translateType $ v ^. aNode
      v' <- translateNode v
      pure (k', (ty, v'))
    schema <- translateSchema node
    pure $ EObject (LiteralObject $ Map.fromList kvs') schema

  --
  -- TODO: more cases.
  --

  ast -> throwError $ UnexpectedNode ast

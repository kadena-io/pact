{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MonadFailDesugaring        #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Pact.Analyze.Translate where

import           Control.Lens               (at, view, (<&>), (?~), (^.),
                                             (^?))
import           Control.Monad.Except       (Except, MonadError, throwError)
import           Control.Monad.Fail         (MonadFail (fail))
import           Control.Monad.Gen          (GenT, MonadGen(gen))
import           Control.Monad.Reader       (MonadReader (local), ReaderT)
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
import           Pact.Types.Lang            (Literal (..), PrimType (..),
                                             Type (..))
import qualified Pact.Types.Lang            as Pact
import           Pact.Types.Typecheck       (AST, Named (Named), Node, aId,
                                             aNode, aTy, tiName, _aTy)
import qualified Pact.Types.Typecheck       as Pact
import           System.Locale              (defaultTimeLocale)

import           Pact.Analyze.Parse
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
  | MonadFailure String
  | NonStaticColumns (AST Node)
  | BadNegationType (AST Node)
  | BadTimeType (AST Node)
  | NonConstKey (AST Node)
  | FailedVarLookup Text
  | NoPacts (AST Node)
  | NoLists (AST Node)
  | NoKeys (AST Node)
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
  -- Uncomment for debugging
  -- UnexpectedNode ast -> "Unexpected node in translation: " <> tShow ast
  UnexpectedNode _ast -> "Analysis doesn't support this construct yet"
  MissingConcreteType ty -> "The typechecker should always produce a concrete type, but we found " <> tShow ty
  MonadFailure str -> "Translation failure: " <> T.pack str
  NonStaticColumns col -> "When reading only certain columns we require all columns to be concrete in order to do analysis. We found " <> tShow col
  BadNegationType node -> "Invalid: negation of a non-integer / decimal: " <> tShow node
  BadTimeType node -> "Invalid: days / hours / minutes applied to non-integer / decimal: " <> tShow node
  NonConstKey k -> "Pact can currently only analyze constant keys in objects. Found " <> tShow k
  FailedVarLookup varName -> "Failed to look up a variable (" <> varName <> "). This likely means the variable wasn't properly bound."
  NoPacts _node -> "Analysis of pacts is not yet supported"
  NoLists _node -> "Analysis of lists is not yet supported"
  NoKeys _node  -> "`keys` is not yet supported"
  UnhandledType node ty -> "Found a type we don't know how to translate yet: " <> tShow ty <> " at node: " <> tShow node
  where
    tShow :: Show a => a -> Text
    tShow = T.pack . show

mkTranslateEnv :: [Arg] -> Map Node (Text, UniqueId)
mkTranslateEnv = foldl'
  (\m (nm, uid, node) -> Map.insert node (nm, uid) m)
  Map.empty

newtype TranslateM a
  = TranslateM { unTranslateM :: ReaderT (Map Node (Text, UniqueId)) (GenT UniqueId (Except TranslateFailure)) a }
  deriving (Functor, Applicative, Monad,
    MonadReader (Map Node (Text, UniqueId)), MonadError TranslateFailure, MonadGen UniqueId)

instance MonadFail TranslateM where
  fail s = throwError (MonadFailure s)

genUid :: Node -> Text -> (UniqueId -> TranslateM a) -> TranslateM a
genUid varNode varName action = do
  uid <- gen
  local (at varNode ?~ (varName, uid)) (action uid)

-- Map.union is left-biased. The more explicit name makes this extra clear.
unionPreferring :: Ord k => Map k v -> Map k v -> Map k v
unionPreferring = Map.union

translateType' :: Pact.Type Pact.UserType -> Maybe EType
translateType' = \case
  TyUser (Pact.Schema _ _ fields _) ->
    fmap (EObjectTy . Schema) $ sequence $ Map.fromList $ fields <&>
      \(Pact.Arg name ty _info) -> (name, translateType' ty)

  -- TODO(joel): understand the difference between the TyUser and TySchema cases
  TySchema _ ty'   -> translateType' ty'

  TyPrim TyBool    -> Just $ EType TBool
  TyPrim TyDecimal -> Just $ EType TDecimal
  TyPrim TyInteger -> Just $ EType TInt
  TyPrim TyString  -> Just $ EType TStr
  TyPrim TyTime    -> Just $ EType TTime
  TyPrim TyKeySet  -> Just $ EType TKeySet

  -- Pretend any and var are the same -- we can't analyze either of them.
  TyAny            -> Just $ EType TAny
  TyVar _          -> Just $ EType TAny

  --
  -- TODO: handle these:
  --
  TyPrim TyValue   -> Nothing
  TyList _         -> Nothing
  TyFun _          -> Nothing

translateType :: Node -> TranslateM EType
translateType node = case _aTy node of
  (translateType' -> Just ety) -> pure ety
  ty                           -> throwError $ UnhandledType node ty

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
  (bindings :: [(String, EType, (Node, Text, UniqueId))]) <- for bindingsA $
    \(Named _ varNode _, colAst) -> do
      let varName = varNode ^. aId.tiName
      varType <- translateType varNode
      uid     <- gen
      case colAst of
        AST_StringLit colName ->
          pure (T.unpack colName, varType, (varNode, varName, uid))
        _ ->
          throwError $ NonStringLitInBinding colAst

  bindingId <- gen
  let freshVar = Var "binding" bindingId

  let translateLet :: Term a -> Term a
      translateLet innerBody = Let "binding" bindingId rhsT $
        -- NOTE: *left* fold for proper shadowing/overlapping name semantics:
        foldl'
          (\body (colName, varType, (_varNode, varName, uid)) ->
            let colTerm = lit colName
            in Let varName uid
              (case varType of
                 EType ty ->
                   ETerm   (At schema colTerm freshVar varType) ty
                 EObjectTy sch ->
                   EObject (At schema colTerm freshVar varType) sch)
              body)
          innerBody
          bindings

      nodeToNameUid = Map.fromList $
        (\(_, _, (node, name, uid)) -> (node, (name, uid))) <$> bindings

  mapETerm translateLet <$>
    local (unionPreferring nodeToNameUid) (translateBody bodyA)

translateNode :: AST Node -> TranslateM ETerm
translateNode astNode = case astNode of
  AST_Let _ [] body -> translateBody body

  AST_Let node ((Named _ varNode _, rhsNode):bindingsRest) body -> do
    rhsETerm <- translateNode rhsNode
    let varName = varNode ^. aId.tiName
    genUid varNode varName $ \uid -> do
      --
      -- TODO: do we only want to allow subsequent bindings to reference
      --       earlier ones if we know it's let* rather than let? or has this
      --       been enforced by earlier stages for us?
      --
      -- XXX allow objects here
      body' <- translateNode $ AST_Let node bindingsRest body
      pure $ case body' of
        ETerm   bodyTm bodyTy -> ETerm   (Let varName uid rhsETerm bodyTm) bodyTy
        EObject bodyTm bodyTy -> EObject (Let varName uid rhsETerm bodyTm) bodyTy

  AST_InlinedApp body -> translateBody body

  AST_Var node -> do
    Just (varName, uid) <- view (at node)
    ty      <- translateType node
    pure $ case ty of
      EType ty'        -> ETerm (Var varName uid) ty'
      EObjectTy schema -> EObject (Var varName uid) schema

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
    Just (name, uid)     <- view (at node)
    EType ty <- translateType node
    case ty of
      TInt     -> pure (ETerm (IntUnaryArithOp Negate (Var name uid)) TInt)
      TDecimal -> pure (ETerm (DecUnaryArithOp Negate (Var name uid)) TDecimal)
      _        -> throwError $ BadNegationType astNode

  AST_Enforce _ cond -> do
    ETerm condTerm TBool <- translateNode cond
    pure $ ETerm (Enforce condTerm) TBool

  AST_Format formatStr vars -> do
    ETerm formatStr' TStr <- translateNode formatStr
    vars' <- for vars translateNode
    pure $ ETerm (Format formatStr' vars') TStr

  AST_FormatTime formatStr time -> do
    ETerm formatStr' TStr <- translateNode formatStr
    ETerm time' TTime     <- translateNode time
    pure $ ETerm (FormatTime formatStr' time') TStr

  AST_ParseTime formatStr timeStr -> do
    ETerm formatStr' TStr <- translateNode formatStr
    ETerm timeStr' TStr   <- translateNode timeStr
    pure $ ETerm (ParseTime (Just formatStr') timeStr') TTime

  AST_Time timeStr -> do
    ETerm timeStr' TStr <- translateNode timeStr
    pure $ ETerm (ParseTime Nothing timeStr') TTime

  AST_Hash val -> do
    val' <- translateNode val
    pure $ ETerm (Hash val') TStr

  AST_ReadKeyset nameA -> do
    ETerm nameT TStr <- translateNode nameA
    return $ ETerm (ReadKeySet nameT) TKeySet

  AST_ReadDecimal nameA -> do
    ETerm nameT TStr <- translateNode nameA
    return $ ETerm (ReadDecimal nameT) TDecimal

  AST_ReadInteger _ -> throwError $ UnexpectedNode astNode
  AST_ReadMsg _     -> throwError $ UnexpectedNode astNode

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

  AST_EnforceOne enforces -> do
    tms <- for enforces $ \enforce -> do
      ETerm enforce' TBool <- translateNode enforce
      pure enforce'
    return $ ETerm (EnforceOne tms) TBool

  AST_Days days -> do
    ETerm days' daysTy <- translateNode days
    case daysTy of
      TInt     -> pure $ ETerm (IntArithOp Mul (60 * 60 * 24) days') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul (60 * 60 * 24) days') TDecimal
      _        -> throwError $ BadTimeType astNode

  AST_Hours hours -> do
    ETerm hours' hoursTy <- translateNode hours
    case hoursTy of
      TInt     -> pure $ ETerm (IntArithOp Mul (60 * 60) hours') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul (60 * 60) hours') TDecimal
      _        -> throwError $ BadTimeType astNode

  AST_Minutes minutes -> do
    ETerm minutes' minutesTy <- translateNode minutes
    case minutesTy of
      TInt     -> pure $ ETerm (IntArithOp Mul 60 minutes') TInt
      TDecimal -> pure $ ETerm (DecArithOp Mul 60 minutes') TDecimal
      _        -> throwError $ BadTimeType astNode

  AST_NFun _node "time" [AST_Lit (LString timeLit)]
    | Just timeLit'
      <- parseTime defaultTimeLocale Pact.simpleISO8601 (T.unpack timeLit)
    -> pure $ ETerm (lit (mkTime timeLit')) TTime

  AST_NFun_Basic "mod" [a, b] ->  do
    ETerm a' TInt <- translateNode a
    ETerm b' TInt <- translateNode b
    pure (ETerm (ModOp a' b') TInt)

  AST_NFun_Basic (textToComparisonOp -> Just op) [a, b] -> do
    ETerm a' ta <- translateNode a
    ETerm b' tb <- translateNode b
    case typeEq ta tb of
      Just Refl -> pure $ ETerm (Comparison op a' b') TBool
      _         -> throwError (TypeMismatch (EType ta) (EType tb))

  AST_NFun_Basic fn@(textToComparisonOp -> Just _) args
    -> throwError $ MalformedComparison fn args

  -- logical: not, and, or

  AST_NFun_Basic "not" [a] -> do
    ETerm a' TBool <- translateNode a
    pure $ ETerm (Logical NotOp [a']) TBool

  AST_NFun_Basic fn args@[a, b]
    | fn == "and" || fn == "or" -> do
      ETerm a' tyA <- translateNode a
      ETerm b' tyB <- translateNode b
      case (tyA, tyB) of
        (TBool, TBool) -> case fn of
          "and" -> pure $ ETerm (Logical AndOp [a', b']) TBool
          "or"  -> pure $ ETerm (Logical OrOp [a', b']) TBool
          _     -> error "impossible"
        _ -> throwError $ MalformedLogicalOp fn args

  AST_NFun_Basic fn@(textToLogicalOp -> Just _) args
    -> throwError $ MalformedLogicalOp fn args

  -- arithmetic

  AST_NFun_Basic fn args@[a, b]
    | fn `Set.member` Set.fromList ["round", "ceiling", "floor"] -> do
      ETerm a' tyA <- translateNode a
      ETerm b' tyB <- translateNode b
      case (tyA, tyB, fn) of
        (TDecimal, TInt, "round")   -> pure $
          ETerm (RoundingLikeOp2 Round a' b') TDecimal
        (TDecimal, TInt, "ceiling") -> pure $
          ETerm (RoundingLikeOp2 Ceiling a' b') TDecimal
        (TDecimal, TInt, "floor")   -> pure $
          ETerm (RoundingLikeOp2 Floor a' b') TDecimal
        _ -> throwError $ MalformedArithOp fn args

  AST_NFun_Basic fn [a]
    | fn `Set.member` Set.fromList
      ["-", "sqrt", "ln", "exp", "abs", "round", "ceiling", "floor"]
    -> do
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
        _ -> throwError $ MalformedArithOp fn [a]

  AST_NFun_Basic "+" [a, b] -> do
    ETerm a' tyA <- translateNode a
    ETerm b' tyB <- translateNode b
    case (tyA, tyB) of
      (TStr, TStr)         -> pure $ ETerm (Concat a' b') TStr
      (TInt, TInt)         -> pure $ ETerm (IntArithOp Add a' b') TInt
      (TDecimal, TDecimal) -> pure $ ETerm (DecArithOp Add a' b') TDecimal
      (TInt, TDecimal)     -> pure $ ETerm (IntDecArithOp Add a' b') TDecimal
      (TDecimal, TInt)     -> pure $ ETerm (DecIntArithOp Add a' b') TDecimal
      -- TODO(joel): handle other forms of plus! (object merging)
      _ -> throwError $ MalformedArithOp "+" [a, b]

  AST_NFun_Basic fn args@[a, b]
    | fn `Set.member` Set.fromList ["-", "*", "/", "^"] -> do
      ETerm a' tyA <- translateNode a
      ETerm b' tyB <- translateNode b
      let Just op = textToArithOp fn
      case (tyA, tyB) of
        (TInt, TInt)         -> pure $
          ETerm (IntArithOp op a' b') TInt
        (TDecimal, TDecimal) -> pure $
          ETerm (DecArithOp op a' b') TDecimal
        (TInt, TDecimal)     -> pure $
          ETerm (IntDecArithOp op a' b') TDecimal
        (TDecimal, TInt)     -> pure $
          ETerm (DecIntArithOp op a' b') TDecimal
        _ -> throwError $ MalformedArithOp fn args

  AST_NFun_Basic fn@(textToArithOp -> Just _) args
    -> throwError $ MalformedArithOp fn args

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
        AST_Lit (LString t) -> pure t
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

  AST_Step                -> throwError $ NoPacts astNode
  AST_NFun _ "pact-id" [] -> throwError $ NoPacts astNode

  AST_NFun _ f _
    | f `Set.member` Set.fromList
      ["map", "make-list", "filter", "reverse", "sort", "take", "fold"]
    -> throwError $ NoLists astNode

  AST_NFun _ "keys" [_] -> throwError $ NoKeys astNode

  _ -> throwError $ UnexpectedNode astNode

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      :  Pact.Typechecker
-- Copyright   :  (C) 2017 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Stylized static typechecker for Pact, operates on
-- "loaded" modules only, where all references have been resolved.
-- Upon success, produces a fully-inlined 'AST' with concrete types
-- for each node.
--
-- Typechecking follows a Hindley-Milner approach using binary
-- substitutions, but does not have need for polytypes since all
-- types are expected to resolve to a concrete type or a native
-- function.
--
-- Pact's support for overloaded native functions is one reason
-- a more traditional HM approach is not used.
--
-- TODO: handle lazy functions better (map, filter etc).
--

module Pact.Typechecker where

import Bound.Scope
import Control.Arrow hiding ((<+>))
import Control.Lens hiding (List,Fold)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.Bitraversable (bimapM)
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Set (Set, isSubsetOf)
import qualified Data.Set as S
import qualified Data.Set as Set
import Data.String
import qualified Data.Vector as V
import Data.Text (Text, unpack, pack)


import Pact.Types.Native
import Pact.Types.Pretty
import qualified Pact.Types.Runtime as Term
import Pact.Types.Runtime hiding (App,ModRef,appInfo,Object,Step)
import Pact.Types.Typecheck

die :: MonadThrow m => Info -> String -> m a
die i s = throwM $ CheckerException i s

die' :: MonadThrow m => TcId -> String -> m a
die' i = die (_tiInfo i)

die'' :: MonadThrow m => AST Node -> String -> m a
die'' = die' . _aId . _aNode



debug :: String -> TC ()
debug s = use tcDebug >>= \d -> debug' d s

debug' :: MonadIO m => Bool -> String -> m ()
debug' d s = when d (liftIO $ putStrLn s)


freshId :: Info -> Text -> TC TcId
freshId i n = TcId i n <$> state (_tcSupply &&& over tcSupply succ)



-- | Walk the AST, performing function both before and after descent into child elements.
walkAST :: Monad m => Visitor m n -> AST n -> m (AST n)
walkAST f t@Prim {} = f Pre t >>= f Post
walkAST f t@Var {} = f Pre t >>= f Post
walkAST f t@Table {} = f Pre t >>= f Post
walkAST f t@ModRef {} = f Pre t >>= f Post
walkAST f t@Object {} = do
  a <- f Pre t
  t' <- Object (_aNode a) <$>
         mapM (walkAST f) (_aObject a)
  f Post t'
walkAST f t@List {} = do
  a <- f Pre t
  t' <- List (_aNode a) <$> mapM (walkAST f) (_aList a)
  f Post t'
walkAST f t@Binding {} = do
  a <- f Pre t
  t' <- Binding (_aNode a) <$>
        forM (_aBindings a) (\(k,v) -> (k,) <$> walkAST f v) <*>
        mapM (walkAST f) (_aBody a) <*> pure (_aBindType a)
  f Post t'
walkAST f t@App {} = do
  a <- f Pre t
  t' <- App (_aNode a) <$>
        (case _aAppFun a of
           fun@FNative {..} -> case _fSpecial of
             Nothing -> return fun
             Just (fs,SBinding bod) -> do
               bod' <- walkAST f bod
               return (set fSpecial (Just (fs,SBinding bod')) fun)
             _ -> return fun
           fun@FDefun {..} -> do
             db <- mapM (walkAST f) _fBody
             return $ set fBody db fun
        ) <*>
        mapM (walkAST f) (_aAppArgs a)
  f Post t'
walkAST f t@Step {} = do
  a <- f Pre t
  t' <- Step (_aNode a)
    <$> traverse (walkAST f) (_aEntity a)
    <*> walkAST f (_aExec a)
    <*> traverse (walkAST f) (_aRollback a)
    <*> pure (_aYieldResume a)
    <*> pure (_aModel a)
  f Post t'
walkAST f t@Dynamic {} = f Pre t >>= f Post

isConcreteTy :: Type n -> Bool
isConcreteTy ty = not (isAnyTy ty || isVarTy ty)


data RoleTys = RoleTys
  { _rtCandArgTy :: Type UserType
  , _rtAST :: AST Node
  , _rtTyVar :: TypeVar UserType
  , _rtResolvedTy :: Type UserType
  }
instance Show RoleTys where
  show (RoleTys a b c d) =
    "RoleTys { candArgTy=" ++ show a ++ ", AST=" ++ show (_aNode b) ++
    ", tyVar=" ++ show c ++ ", resolvedTy=" ++ show d ++ "}"
instance Pretty RoleTys where
  pretty (RoleTys a b c d) =
    "RoleTys { candArgTy=" <> pretty a <> ", AST=" <> pretty (_aNode b) <>
    ", tyVar=" <> pretty c <> ", resolvedTy=" <> pretty d <> "}"
makeLenses ''RoleTys



-- | Run through all overloads attempting to find all-concrete type solution for each.
solveOverloads :: TC ()
solveOverloads = do

  overs <- use tcOverloads >>=
           traverse (traverse (\i -> (i,) . fst <$> lookupAst "solveOverloads" (_aId (_aNode i))))
  oids <- use tcOverloadOrder

  let runSolve os = fmap M.fromList $ forM oids $ \oid -> case M.lookup oid os of
        Nothing -> die def $ "Internal error, unknown overload id: " ++ show oid
        Just o@Overload {..} -> fmap (oid,) $ case _oSolved of
          Just {} -> return o
          Nothing -> (\s -> set oSolved s o) <$> foldM (tryFunType o) Nothing _oTypes

      rptSolve os = runSolve os >>= \os' -> if os' == os then return os' else rptSolve os'

  done <- rptSolve overs
  tcOverloads .= fmap (fmap fst) done

  forM_ (M.toList done) $ \(i,o) -> case _oSolved o of
      Nothing -> do
        debug $ "Unable to solve overload: " ++ show (i,o)
        addFailure i $ "Unable to solve overloaded function: " ++ unpack (_oFunName o)
      Just {} -> return ()


-- | Attempt to solve an overload with one of its candidate types.
tryFunType :: Overload (AST Node,TypeVar UserType) -> Maybe (FunType UserType) -> FunType UserType ->
               TC (Maybe (FunType UserType))
tryFunType _ r@Just {} _ = return r
tryFunType Overload {..} _ cand@(FunType candArgs candRetTy) = do

  -- see if var role unifies with with fun role. If so, return singleton list of var information
  -- indexed by fun role.
  let tryRole :: VarRole -> Type UserType ->
                 TC (Maybe (VarRole,RoleTys))
      tryRole rol candArgTy = case M.lookup rol _oRoles of
        Nothing -> return Nothing
        -- resolve typevar at role
        Just (roleAST,roleTyVar) -> use tcVarToTypes >>= \m -> case M.lookup roleTyVar m of
          Nothing -> die' (_aId (_aNode roleAST)) $ "Bad var in overload solver: " ++ showPretty roleTyVar
          Just ty -> do
            roleResolvedTy <- resolveTy ty
            debug $ "tryFunType: unify role: role=" ++ showPretty rol ++ ", candTy=" ++ showPretty candArgTy ++
              ", roleTy=" ++ showPretty roleResolvedTy
            case unifyTypes candArgTy roleResolvedTy of
              Nothing -> return Nothing
              Just _ -> return $ Just (rol,RoleTys candArgTy roleAST roleTyVar roleResolvedTy)

  -- try arg and return roles
  argRoles <- forM (zip candArgs [0..]) $ \(Arg _ candArgTy _,ai) -> tryRole (ArgVar ai) candArgTy
  allRoles <- (:argRoles) <$> tryRole RetVar candRetTy
  case sequence allRoles of
    Nothing -> do
      debug $ "tryFunType: failed: " ++ showPretty cand ++ ": roles=" ++ showPretty allRoles
      return Nothing
    Just ars -> do

      debug $ "tryFunType: cand: " ++ showPretty cand ++ ", roles=" ++ showPretty allRoles

      let byRole = handleSpecialOverload _oSpecial $ M.fromList ars
          byFunType = M.fromListWith (++) $ map (\(RoleTys fty i tv ty) -> (fty,[(_aId (_aNode i),tv,ty)])) $ M.elems byRole

      debug $ "tryFunType: trying " ++ showPretty cand ++ " with " ++ showPretty (M.toList byFunType)

      let solvedM = sequence $ (`map` M.toList byFunType) $ \(fty,tvTys) ->
            let tys = foldl1 unifyM $ (Just fty:) $ map (Just . view _3) tvTys
                unifyM (Just a) (Just b) = either id id <$> unifyTypes a b
                unifyM _ _ = Nothing
            in case tys of
              Nothing -> Nothing
              Just uty -> Just (fty,(uty,map (view _1 &&& view _2) tvTys))

          allConcrete = all isConcreteTy . map (fst . snd)

      case solvedM of
        Nothing -> return Nothing
        Just solved
          | allConcrete solved -> do

              debug $ "Solved overload with " ++ showPretty cand ++ ": " ++ showPretty solved
              forM_ solved $ \(fty,(uty,tvs)) -> forM_ tvs $ \tv -> do
                debug $ "Adjusting type for solution: " ++ showPretty (tv,fty,uty)
                uncurry assocTy tv fty
                uncurry assocTy tv uty
              return $ Just cand

          | otherwise -> do
              debug $ "tryFunType: not all concrete: " ++ show solved
              return Nothing


handleSpecialOverload :: Maybe OverloadSpecial ->
                         M.Map VarRole RoleTys ->
                         M.Map VarRole RoleTys
handleSpecialOverload (Just OAt) m =
  case (M.lookup (ArgVar 0) m,M.lookup (ArgVar 1) m,M.lookup RetVar m) of
    (Just keyArg,Just srcArg,Just ret) -> case (roleTy keyArg,view rtAST keyArg,roleTy srcArg) of
      (TyPrim TyString,Prim _ (PrimLit (LString k)),TySchema TyObject (TyUser u) _) -> case findSchemaField k u of
        Nothing -> m
        Just t -> unifyRet (roleTy ret) t
      (TyPrim TyInteger,_,TyList t) -> unifyRet (roleTy ret) t
      _ -> m
    _ -> m
  where
    roleTy = view rtResolvedTy
    unifyRet ret ty = case unifyTypes ret ty of
                        Nothing -> m
                        Just e -> set (at RetVar . _Just . rtResolvedTy) (either id id e) m
handleSpecialOverload _ m = m


findSchemaField :: Text -> UserType -> Maybe (Type UserType)
findSchemaField fname (UTSchema Schema{..}) =
      foldl' (\r Arg {..} -> mplus (if _aName == fname then Just _aType else Nothing) r) Nothing _schFields
findSchemaField _ UTModSpec{} = Nothing

asPrimString :: AST Node -> TC Text
asPrimString (Prim _ (PrimLit (LString s))) = return s
asPrimString t = die (_tiInfo (_aId (_aNode t))) $ "Expected literal string: " ++ show t

-- | Visitor to inspect Objects and Bindings to validate schemas,
-- "pushing down" the field types to associate with field ASTs.
-- Also handles validating lists, as this seems like the right phase for that too.
applySchemas :: Visitor TC Node
applySchemas Pre ast = case ast of

  (Object n (ObjectMap ps)) -> findSchema n $ \sch partial -> do
    debug $ "applySchemas [object]: " ++ showPretty (n,sch,partial)
    pmap <- forM ps $ \v -> do
      vt' <- lookupAndResolveTy (_aNode v)
      return (v,_aId (_aNode v),vt')
    validateSchema sch partial pmap >>= \pm -> forM_ pm $ \pkeys -> case partial of
      FullSchema -> addFailure (_aId n) "Invalid partial schema found"
      _ -> do
        debug $ "Specializing schema ty for sublist: " ++ show pkeys
        assocAstTy n $ TySchema TyObject (TyUser sch) (PartialSchema pkeys)
    return ast

  (Binding _ bs _ (AstBindSchema n)) -> findSchema n $ \sch partial -> do
    debug $ "applySchemas [binding]: " ++ showPretty (n,sch)
    pmapM <- forM bs $ \(Named _ node ni,bv) -> case bv of
      Prim _ (PrimLit (LString bn)) -> do
        vt' <- lookupAndResolveTy node
        return $ Just (FieldKey bn,(Var node,ni,vt'))
      _ -> addFailure ni ("Found non-string key in schema binding: " ++ showPretty bv) >> return Nothing
    case sequence pmapM of
      Just pmap -> validateSchema sch partial (M.fromList pmap) >>= \pm ->
        forM_ pm $ \pkeys -> case partial of
          PartialSchema{} -> do
            debug $ "Specializing schema ty for sublist: " ++ show pkeys
            assocAstTy n $ TySchema TyObject (TyUser sch) (PartialSchema pkeys)
          _ -> pure ()
      Nothing -> return () -- error already tracked above
    return ast

  (List n ls) -> lookupAndResolveTy n >>= \ltym -> case ltym of
    (TyList TyAny) -> do
      debug $ "Skipping TC on specified heterogenous list for node: " ++ show n
      return ast
    (TyList lty) -> do
      debug ("validateList: " ++ show lty)
      validateList n lty ls
      return ast
    _ -> return ast

  _ -> return ast

  where
    -- Given map of object text key to (value,key id,value ty) ...
    -- (returns partial keys list if subset of specified)
    validateSchema :: UserType -> SchemaPartial
                   -> M.Map FieldKey (AST Node, TcId, Type UserType) -> TC (Maybe (Set Text))
    validateSchema (UTSchema sch) partial pmap = do
      -- smap: lookup from field name to ty
      let smap = filterPartial partial $ M.fromList $ (`map` _schFields sch) $ \(Arg an aty _) -> (an,aty)
          filterPartial FullSchema = id
          filterPartial AnySubschema = id
          filterPartial (PartialSchema ks) = (`M.restrictKeys` ks)
      -- over each object field ...
      pks <- fmap (Set.fromList . concat) $ forM (M.toList pmap) $ \(FieldKey k,(v,ki,vty)) -> case M.lookup k smap of
        -- validate field exists ...
        Nothing -> do
          addFailure ki $ "Invalid field in schema object: " ++ showPretty k ++ ", expected " ++ showPretty (M.keys smap)
          return []
        -- unify field value ty ...
        Just aty -> do
          case unifyTypes aty vty of
            Nothing -> addFailure (_aId (_aNode v)) $ "Unable to unify field type: " ++ showPretty (k,aty,(vty,v))
            -- associate unified ty with value node.
            Just u -> assocAstTy (_aNode v) (either id id u)
          return [k]
      return $ if Set.size pks < M.size smap then Just pks else Nothing
    validateSchema (UTModSpec (ModSpec mn)) _ _ = die def
      $ "Invalid mod ref in schema position: "
      <> showPretty mn

    lookupAndResolveTy a = resolveTy =<< (snd <$> lookupAst "lookupTy" (_aId a))

    -- Resolve schema ty if possible and act on it, otherwise skip.
    findSchema :: Node -> (UserType -> SchemaPartial -> TC (AST Node)) -> TC (AST Node)
    findSchema n act = do
      ty <- lookupAndResolveTy n
      case ty of
        (TySchema _ (TyUser sch) sp) -> act sch sp
        _ -> return ast

    validateList :: Node -> Type UserType -> [AST Node] -> TC ()
    validateList n lty ls = forM_ ls $ \a -> do
        aty <- lookupAndResolveTy $ _aNode a
        case unifyTypes lty aty of
          Nothing -> addFailure (_aId (_aNode a)) $ "Unable to unify list member type: " ++ showPretty (lty,aty,a)
          Just (Left uty) -> assocAstTy (_aNode a) uty
          Just (Right uty) -> assocAstTy n (TyList uty)


applySchemas Post a = return a




-- | Visitor to process Apps of native funs.
-- For non-overloads, associate fun tys with app args and result.
-- Overloads are simply tracked to app args and result.
-- Special form support includes:
--  1. for 'map', associate lambda result type for arg AST
--  2. for bindings, associate binding AST with fun return ty,
--     associate inner binding schema type with funty last arg ty
processNatives :: Visitor TC Node
processNatives Pre a@(App i FNative {..} argASTs) = do
  case _fTypes of
    -- single funtype
    orgFunType@FunType {} :| [] -> do

      let mangledFunType = mangleFunType (_aId i) orgFunType
          ftargs = _ftArgs mangledFunType

      debug $ "Mangled funtype: " ++ showPretty orgFunType ++ " -> " ++ showPretty mangledFunType

      case _fSpecial of
        Nothing -> do
          let !n = length ftargs
              !m = length argASTs
          -- check arg arity so we don't accidentally trim any zipped args
          unless (n == m) $ addFailure (_aId $ _aNode a)
            $ "native function supplied "
            <> (if n < m then "too few" else "too many")
            <> " args: "
            <> showPretty argASTs

        Just{} -> return ()

      -- zip funtype 'Arg's with AST args, and assoc each.
      args <- (\f -> zipWithM f (_ftArgs mangledFunType) argASTs) $ \(Arg _ argTy _) argAST -> case (argTy,argAST) of
        (TyFun lambdaType,partialAST@App{}) -> do
          debug $ "associating partial AST app with lambda return type: " ++ show lambdaType
          assocAstTy (_aNode argAST) $ _ftReturn lambdaType
          debug "associating partial AST args with lambda arg types"
          let lamArgTys = _ftArgs lambdaType
              partialArgASTs = _aAppArgs partialAST
              saturated = length partialArgASTs - length lamArgTys
          when (saturated < 0) $
            die' (_aId (_aNode argAST)) $ "Invalid/unsaturated partial application: " ++ show argTy
          (\f -> zipWithM_ f lamArgTys (drop saturated partialArgASTs)) $ \(Arg _ lamArgTy _) partialArgAST ->
            assocAstTy (_aNode partialArgAST) lamArgTy
          return (argAST,argTy)
        (TyFun{},_) -> die' (_aId (_aNode argAST)) $ "App required: " ++ show argTy
        _ -> do
          assocAstTy (_aNode argAST) argTy
          return (argAST,argTy)

      -- assoc return type
      assocAstTy i $ _ftReturn mangledFunType
      -- perform extra assocs for special forms
      case _fSpecial of
        Nothing -> return ()
        Just spec -> case (spec,args) of
          -- bindings
          ((_,SBinding b),_) -> case b of
            (Binding bn _ _ (AstBindSchema sn)) -> do
              -- assoc binding with app return
              assocAstTy bn $ _ftReturn mangledFunType
              -- assoc schema with last ft arg
              assocAstTy sn (_aType (last (toList $ _ftArgs mangledFunType)))
            (List _ln ll) -> do -- TODO, for with-capability
              -- assoc app return with last of body
              assocAstTy (_aNode $ last ll) $ _ftReturn mangledFunType
            sb -> die _fInfo $ "Invalid special form, expected binding: " ++ show sb
          -- TODO the following is dodgy, schema may not be resolved.
          ((Where,_),[(field,_),(partialAst,_),(_,TySchema TyObject uty _)]) -> asPrimString field >>= \fld -> case uty of
            TyUser u -> case findSchemaField fld u of
              Nothing -> return ()
              Just fty -> assocTyWithAppArg fty partialAst 0
            _ -> return ()
          _ -> return ()
    -- multiple funtypes
    fts -> do
      let fts' = fmap (mangleFunType (_aId i)) fts
          argOvers = mconcat $ zipWith (\n aa -> M.singleton (ArgVar n) aa) [0..] argASTs
          ospec | _fName == "at" = Just OAt
                | _fName == asString Select = Just OSelect
                | otherwise = Nothing
          oload = Overload _fName (argOvers <> M.singleton RetVar a) fts' Nothing ospec
      tcOverloads %= M.insert (_aId i) oload
      tcOverloadOrder %= (_aId i:)
  return a
processNatives _ a = return a



assocTyWithAppArg :: Type UserType -> AST Node -> Int -> TC ()
assocTyWithAppArg tl t@(App _ _ as) offset = debug ("assocTyWithAppArg: " ++ show (tl,offset,t)) >> case as ^? ix (length as - offset - 1 ) of
  Just a -> assocAstTy (_aNode a) tl
  Nothing -> return ()
assocTyWithAppArg _ _ _ = return ()

-- | Inlines function call arguments into a defun, returning the new body. We
-- introduce a synthetic 'Binding' to let-bind all arguments before inlining to
-- achieve call-by-value semantics, and the correct behavior for effectful
-- argument expressions.
inlineAppArgs
  :: Node             -- ^ function application 'Node'
  -> FunType UserType -- ^ type of the function being applied
  -> [Named Node]     -- ^ arglist of the defun
  -> [AST Node]       -- ^ body of the defun
  -> [AST Node]       -- ^ arguments being passed into the function
  -> TC [AST Node]    -- ^ updated, inlined body
inlineAppArgs appNode defunType defunArgs defunBody args = do
  let FunType argTys retTy = mangleFunType (_aId appNode) defunType

  assocAstTy appNode retTy

  let appInfo  = _tiInfo $ _aId appNode
      bindType = AstBindInlinedCallArgs :: AstBindType Node
  letId <- freshId appInfo (pack $ show bindType)
  letNode <- trackIdNode letId
  assocAstTy letNode retTy

  letBinders :: [Named Node] <- forM (zip args argTys) $
    \(arg, Arg nm argTy info) -> do
      let uniqueName = tShow letId `pfx` nm
      varId <- freshId info uniqueName
      let varTy = mangleType varId argTy
      varNode <- trackNode varTy varId
      assocAST varId arg
      pure $ Named nm varNode varId

  let letBindings :: [(Named Node, AST Node)]
      letBindings = zip letBinders args

      vars :: [AST Node]
      vars = Var . _nnNamed <$> letBinders

      subs :: [(TcId, AST Node)]
      subs = zip (_aId . _nnNamed <$> defunArgs) vars

      substitute :: (TcId, AST Node) -> AST Node -> TC (AST Node)
      substitute sub = walkAST (substAppDefun (Just sub))

  body' <- forM defunBody $ \bodyExpr -> foldM (flip substitute) bodyExpr subs
  return [Binding letNode letBindings body' bindType]

-- | Visitor to process Apps of user defuns.
-- We want to a) replace AST nodes with the app arg ASTs,
-- b) associate the AST nodes to check and track their related types
substAppDefun :: Maybe (TcId, AST Node) -> Visitor TC Node
substAppDefun (Just (defArg,appAst)) Pre t@Var {..}
  | defArg == _aId _aNode = assocAST defArg appAst >> return appAst
  | otherwise = return t
substAppDefun Nothing Post (App appNode fun args) = do -- Post, to allow AST subs first
    fun' <- case fun of
      FNative {} ->
        return fun -- noop
      FDefun {_fType,_fArgs,_fBody} -> do
        body' <- inlineAppArgs appNode _fType _fArgs _fBody args
        return $ set fBody body' fun
    return (App appNode fun' args)
substAppDefun _ _ t = return t

-- | Track AST as a TypeVar pointing to a Types. If the provided node type is already a var use that,
-- otherwise make a new var based on the TcId.
trackAST :: Node -> TC ()
trackAST (Node i t) = do
  debug $ "trackAST: " ++ showPretty (i,t)
  maybe (return ()) (const (die' i $ "trackAST: ast already tracked: " ++ show (i,t)))
    =<< (M.lookup i <$> use tcAstToVar)
  let v = case t of
        (TyVar tv) -> tv
        _ -> TypeVar (fromString (show i)) []
  tcAstToVar %= M.insert i v
  maybe (return ()) (const (die' i $ "trackAST: var already tracked: " ++ show (i,t)))
    =<< (M.lookup v <$> use tcVarToTypes)
  tcVarToTypes %= M.insert v t


addFailure :: TcId -> String -> TC ()
addFailure i s = do
  debug $ "Failure: " ++ show (i,s)
  tcFailures %= S.insert (Failure i s)

-- | Lookup both type var and Types for AST node.
lookupAst :: String -> TcId -> TC (TypeVar UserType,Type UserType)
lookupAst msg i = do
  v <- lookupAstVar msg i
  (v,) <$> lookupTypes msg i v

-- | Lookup type var for AST node.
lookupAstVar :: String -> TcId -> TC (TypeVar UserType)
lookupAstVar msg i = maybe (die' i $ msg ++ ": ast not already tracked: " ++ show i) return =<<
       (M.lookup i <$> use tcAstToVar)

-- | Lookup Types for type var. TcId is for logging only.
lookupTypes :: String -> TcId -> TypeVar UserType -> TC (Type UserType)
lookupTypes msg i v =
  maybe (die' i $ msg ++ ":  var not already tracked: " ++ show v) return =<<
        (M.lookup v <$> use tcVarToTypes)

-- | Do substitution between a non-AST type (types from natives, overloads, schemas)
-- and an AST type.
assocAstTy :: Node -> Type UserType -> TC ()
assocAstTy (Node ai _) ty = do
  av <- lookupAstVar "assocAstTy" ai
  assocTy ai av ty


-- | associate/substitute a tracked type with a provided type.
-- The fact that one of these implies storage and the other
-- is "just a type" is problematic, creating much cruft in here.
assocTy :: TcId -> TypeVar UserType -> Type UserType -> TC ()
assocTy ai av ty = do
  aty <- resolveTy =<< lookupTypes "assocTy" ai av
  debug $ "assocTy: " ++ showPretty (av,aty) ++ " <=> " ++ showPretty ty
  unifyTypes' ai aty ty $ \r -> case r of
    Left _same -> do
      -- AST type is most specialized. If assoc ty is a var, update it to point
      -- at ast type.
      assocParams ai ty aty
      case ty of
        TyVar tv -> do
          tvtysm <- M.lookup tv <$> use tcVarToTypes
          case tvtysm of
            Nothing -> do
              -- RH is tyvar with no tracking; track as LH ty.
              debug $ "assocTy: " ++ showPretty aty ++ " => " ++ showPretty tv
              tcVarToTypes %= M.insert tv aty
            Just tvtys ->
              -- RH is tyvar tracking type. Unify and update tracked type.
              unifyTypes' ai aty tvtys $ \r' ->
                (tcVarToTypes . at tv . _Just) .= either id id r'
        TyList TyAny -> do
          debug $ "assocTy: specified heterogenous list, " ++ showPretty aty ++ " <= " ++ showPretty ty
          tcVarToTypes . at av . _Just .= ty
        _ -> debug $ "assocTy: noop: " ++ showPretty (aty,ty)
    Right u -> do
      -- Associated ty is most specialized, simply update record for AST type.
      debug $ "assocTy: " ++ showPretty (av,aty) ++ " <= " ++ showPretty u
      tcVarToTypes . at av . _Just .= u
      assocParams ai u aty
      -- if old type was var, make entry and adjust it
      case aty of
        TyVar atyv | u /= aty -> do
                       debug $ "assocTy: tracking/updating type variable " ++
                         showPretty atyv ++ " <= " ++ showPretty u
                       alterTypes atyv u (const u)
        _ -> return ()


-- | Set, or update, a tracked 'Types' value.
alterTypes :: TypeVar UserType -> Type UserType -> (Type UserType -> Type UserType) -> TC ()
alterTypes v newVal upd = tcVarToTypes %= M.alter (Just . maybe newVal upd) v


-- | Investigate types "inside of" schemas and lists to see if they can associate.
assocParams :: TcId -> Type UserType -> Type UserType -> TC ()
assocParams i x y = case (x,y) of
  _ | x == y -> return ()
  (TySchema _ a _,TySchema _ b _) -> assoc a b -- partial not dealt with here
  (TyList a,TyList b) -> assoc a b
  _ -> return ()
  where
    createMaybe v = alterTypes v (TyVar v) id
    assoc (TyVar a) b | b /= TyAny = createMaybe a >> assocTy i a b
    assoc a (TyVar b) | a /= TyAny = createMaybe b >> assocTy i b a
    assoc _ _ = return ()

-- | Associate an id with another AST node, substituting to most specialized.
-- Not as thorough as `assocTy`, should probably use it.
assocAST :: TcId -> AST Node -> TC ()
assocAST ai b = assocNode ai (_aNode b)

assocNode :: TcId -> Node -> TC ()
assocNode ai bn = do
  let bi = _aId bn
  (av,aty) <- lookupAst "assocAST" ai
  (bv,bty) <- lookupAst "assocAST" bi
  let doSub si sv sty fi fv fty = do
        debug $ "assocAST: " ++ showPretty (si,sv,sty) ++ " => " ++ showPretty (fi,fv,fty)
        -- reassign any references to old var to new
        tcAstToVar %= fmap (\v -> if v == fv then sv else v)
        -- cleanup old var
        unless (sv == fv) $ tcVarToTypes %= M.delete fv
  case unifyTypes aty bty of
    Nothing -> addFailure bi $ "assocAST: cannot unify: " ++ showPretty (aty,bty)
    Just (Left _) -> doSub ai av aty bi bv bty
    Just (Right _) -> doSub bi bv bty ai av aty

-- | Unify two types and note failure.
unifyTypes' :: TcId -> Type UserType -> Type UserType -> (Either (Type UserType) (Type UserType) -> TC ()) -> TC ()
unifyTypes' i a b act = case unifyTypes a b of
  Just r -> act r
  Nothing -> do
    addFailure i $ "Cannot unify: " ++ showPretty (a,b)
    return ()

-- | Unify two types, indicating which of the types was more specialized with the Either result.
unifyTypes :: Type UserType -> Type UserType -> Maybe (Either (Type UserType) (Type UserType))
unifyTypes l r = case (l,r) of
  _ | l == r -> Just (Right r)
  (TyAny,_) -> Just (Right r)
  (_,TyAny) -> Just (Left l)
  (TyPrim (TyGuard gl), TyPrim (TyGuard gr)) -> case (gl,gr) of
    (Just _, Nothing) -> Just (Left l)
    (Nothing, Just _) -> Just (Right r)
    _ -> Just (Right r) -- equality already covered above, and Nothing/Nothing ok
  (TyVar v,s) -> unifyVar Left Right v s
  (s,TyVar v) -> unifyVar Right Left v s
  (TyList a,TyList b) -> unifyParam Just a b
  (TySchema sa a spa,TySchema sb b spb) | sa == sb -> unifyParam (specializePartial spa spb) a b
  (TyModule ms, TyModule ms') -> case (ms, ms') of
    (Just a, Just b) -> do
      a' <- traverse toModuleName a
      b' <- traverse toModuleName b
      if testElems a' b' then return $ Left l
      else if testElems b' a' then return $ Right r
      else Nothing
    _ -> Nothing
  _ -> Nothing
  where
    toModuleName (UTModSpec (ModSpec mn)) = return mn
    toModuleName _ = Nothing

    testElems as bs = all (`elem` bs) as
    -- | Unifies param and uses bias to return parent, with additional test/modifier f
    -- TODO why not specialize param too?
    unifyParam
      :: (Type UserType -> Maybe (Type UserType))
      -> Type UserType -> Type UserType -> Maybe (Either (Type UserType) (Type UserType))
    unifyParam f a b = bimapM (const (f l)) (const (f r)) =<< unifyTypes a b
    specializePartial spa spb u =
      let setPartial p = Just $ set tySchemaPartial p u
      in case (spa,spb) of
        (FullSchema,FullSchema) -> Just u
        (FullSchema,_) -> setPartial spb
        (_,FullSchema) -> setPartial spa
        (PartialSchema as,PartialSchema bs)
          | as == bs           -> Just u
          | bs `isSubsetOf` as -> setPartial spb
          | as `isSubsetOf` bs -> setPartial spa
          | otherwise          -> Nothing
        (PartialSchema {}, AnySubschema) -> setPartial spa
        (AnySubschema, PartialSchema {}) -> setPartial spb
        (AnySubschema, AnySubschema) -> Just u
    unifyVar vWrap sWrap v s =
      let useS = sWrap s
      in case (v,s) of
        (SchemaVar {},TyUser {}) -> Just useS
        (SchemaVar {},TyVar SchemaVar {}) -> Just useS
        (SchemaVar {},_) -> Nothing
        (TypeVar {},TyVar SchemaVar {}) -> Nothing
        (TypeVar {},TyUser {}) -> Nothing
        (TypeVar _ ac,TyVar sv@(TypeVar _ bc)) -> case unifyConstraints ac bc of
          Just (Left uc)  -> Just . vWrap . TyVar $ set tvConstraint uc v
          Just (Right uc) -> Just . sWrap . TyVar $ set tvConstraint uc sv
          Nothing -> Nothing
        (TypeVar _ ac,_) | checkConstraints s ac -> Just useS
        _ -> Nothing

-- | check for unification of (non-var) type against constraints
checkConstraints :: Type UserType -> [Type UserType] -> Bool
checkConstraints _ [] = True
checkConstraints ty constrs = foldl' check False constrs where
  check r con = r || isJust (unifyTypes ty con)


-- | Attempt to unify identifying which input "won", or specialize the first type.
unifyConstraints :: [Type UserType] -> [Type UserType] -> Maybe (Either [Type UserType] [Type UserType])
unifyConstraints ac bc
  | null ac = Just $ Right bc
  | null bc = Just $ Left ac
  | all (`elem` ac) bc = Just $ Right bc
  | all (`elem` bc) ac = Just $ Left ac
  | null intersection = Nothing
  | otherwise = Just $ Left intersection
  where intersection = ac `intersect` bc

-- | Instantiate a Bound scope as AST nodes or references.
scopeToBody :: Info -> [AST Node] -> Scope Int Term (Either Ref (AST Node)) -> TC [AST Node]
scopeToBody i args bod = do
  bt <- instantiate (return . Right) <$> traverseScope (bindArgs i args) return bod
  case bt of
    TList ts _ _ -> mapM toAST (V.toList ts)
    _ -> die i "Malformed def body"

pfx :: Text -> Text -> Text
pfx s = ((s <> "_") <>)

-- | Make a type variable to track a TcId.
idTyVar :: TcId -> Type n
idTyVar i = mkTyVar (TypeVarName $ pack $ show i) []

-- | Make/track a Node using an "idTyVar", ie a variable for itself.
trackIdNode :: TcId -> TC Node
trackIdNode i = trackNode (idTyVar i) i

mangle :: TcId -> Type n -> Type n
mangle i = over (tyVar.tvName.typeVarName) (pfx (pack $ show i))

-- | Mangle a type variable.
mangleType :: TcId -> Type UserType -> Type UserType
mangleType f t@TyVar {} = mangle f t
mangleType f t@TyList {} = over tyListType (mangleType f) t
mangleType f t@TySchema {} = over tySchemaType (mangle f) t
mangleType f t@TyFun {} = over tyFunType (mangleFunType f) t
mangleType _ t = t

mangleFunType :: TcId -> FunType UserType -> FunType UserType
mangleFunType f = over ftReturn (mangleType f) .
                  over (ftArgs.traverse.aType) (mangleType f)


-- | Build Defuns and natives from Terms.
toFun :: Term (Either Ref (AST Node)) -> TC (Fun Node)
toFun (TVar (Left (Direct TNative {..})) _) = do
  ft' <- traverse (traverse toUserType') (fmap (fmap (fmap Right)) _tFunTypes)
  return $ FNative _tInfo (asString _tNativeName) ft' Nothing -- we deal with special form in App
toFun (TVar (Left (Ref r)) _) = toFun (fmap Left r)
toFun (TVar Right {} i) = die i "Value in fun position"
toFun (TLam (Lam name ty body _) i) = do
  TcId _ _ newIx <- freshId i "%anonlam"
  let mn = ModuleName ("%anonlam_" <> pack (show newIx)) Nothing
  withScopeBodyToFun name mn ty body Defun i
toFun (TDef td i) = resolveDyn td
  where
    resolveDyn Def{..} = use tcDynEnv >>= \de -> case M.lookup _dModule de of
      Nothing ->
        withScopeBodyToFun (asString _dDefName) _dModule _dFunType _dDefBody _dDefType i
      Just ModuleData{..} -> case HM.lookup (asString _dDefName) _mdRefMap of
        Just r -> toFun (fmap Left $ TVar r i)
        Nothing -> die i $ "Dynamic substitution failed, module does not have member: " ++
          show (moduleDefName _mdModule) ++ "::" ++ show _dDefName
toFun (TDynamic ref mem i) = case (ref, mem) of
  (TVar (Right Var{}) _, TVar (Left (Ref r)) _) -> toFun $ Left <$> r
  _ -> die i "Malformed mod ref"
toFun t = die (_tInfo t) "Non-var in fun position"

withScopeBodyToFun
  :: Show n
  => Text
  -> ModuleName
  -> FunType (Term (Either Ref n))
  -> Scope Int Term (Either Ref (AST Node))
  -> DefType
  -> Info
  -> TC (Fun Node)
withScopeBodyToFun fnname modname funTy body deftype info = do
  args <- forM (_ftArgs funTy) $ \(Arg n t ai) -> do
    an <- freshId ai $ pfx fnname n
    t' <- mangleType an <$> traverse toUserType t
    Named n <$> trackNode t' an <*> pure an
  tcs <- scopeToBody info (map (Var . _nnNamed) args) body
  funType <- traverse toUserType funTy
  funId <- freshId info fnname
  void $ trackNode (_ftReturn funType) funId
  unless (null tcs) $ assocAST funId (last tcs)
  return $ FDefun info modname fnname deftype funType args tcs funId

assocStepYieldReturns :: TopLevel Node -> [AST Node] -> TC ()
assocStepYieldReturns (TopFun (FDefun _ _ _ Defpact _ _ _ _) _) steps =
  void $ toStepYRs >>= foldM go (Nothing,0::Int)
  where
    lastStep = pred $ length steps
    toStepYRs = forM steps $ \step -> case step of
      Step{..} -> case (_aYieldResume, _aRollback) of

        -- check that a cross-chain yield and rollback do not occur
        -- in the same step, otherwise build the tuple
        (Just y, Just{}) ->
          if _yrCrossChain y
          then die'' step "Illegal rollback with yield"
          else return (_aNode, _aYieldResume)
        _ -> return (_aNode, _aYieldResume)
      _ -> die'' step "Non-step in defpact"
    yrMay l yr = preview (_Just . l . _Just) yr
    go :: (Maybe (YieldResume Node),Int) -> (Node, Maybe (YieldResume Node)) -> TC (Maybe (YieldResume Node),Int)
    go (prev,idx) (n,curr) = do
      case (yrMay yrYield prev, yrMay yrYield curr, yrMay yrResume curr) of
        -- resume in first step
        (_,_,Just{}) | idx == 0 -> die' (_aId n) "Illegal resume in first step"
        -- yield in last step
        (_,Just{},_) | idx == lastStep -> die' (_aId n) "Illegal yield in last step"
        -- yield in previous step, no resume in current step
        (Just{},_,Nothing) -> die' (_aId n) "Missing resume after yield"
        -- resume in current step, no yield in last step
        (Nothing,_,Just {}) -> die' (_aId n) "Missing yield before resume"
        -- yield in previous step, resume in current step: associate
        (Just y,_,Just r) -> assocYRSchemas y r
        -- just yield in current: noop
        (Nothing,Just {},Nothing) -> return ()
        -- nothing: noop
        (Nothing,Nothing,Nothing) -> return ()
      return (curr,succ idx)
    lookupSchemaTy n = (resolveTy . snd) =<< lookupAst "assocStepYieldReturns" (_aId n)
    assocYRSchemas a b = do
      a' <- lookupSchemaTy a
      b' <- lookupSchemaTy b
      debug $ "assocYRSchemas: " ++ showPretty ((a,a'),(b,b'))
      assocParams (_aId a) a' b'

assocStepYieldReturns _ _ = return ()



notEmpty :: MonadThrow m => Info -> String -> [a] -> m [a]
notEmpty i msg [] = die i msg
notEmpty _ _ as = return as


-- | Build ASTs from terms.
toAST :: Term (Either Ref (AST Node)) -> TC (AST Node)
toAST TNative {..} = die _tInfo "Native in value position"
toAST TDef {..} = die _tInfo "Def in value position"
toAST TSchema {..} = die _tInfo "User type in value position"
toAST TLam{..} = die _tInfo "lam in value position"
toAST TModRef{..} = do
  tcid <- freshId _tInfo $ renderCompactText _tModRef

  let ty = TyModule
        $ fmap (UTModSpec . ModSpec)
        <$> _modRefSpec _tModRef

  n <- trackNode ty tcid
  return $ ModRef n (_modRefName _tModRef) (_modRefSpec _tModRef)
toAST (TVar v i) = case v of -- value position only, TApp has its own resolver
  (Left (Ref r)) -> toAST (fmap Left r)
  (Left (Direct t)) ->
    case t of
      TLiteral {..} ->
        -- Handle references to pre-evaluated constants:
        trackPrim _tInfo (litToPrim _tLiteral) (PrimLit _tLiteral)
      TConst{..} -> case _tModule of
        -- if modulename is nothing, it's a builtin
        Nothing -> toAST $ return $ Left (Direct $ constTerm _tConstVal)
        _ -> die i $ "Non-native constant value in native context: " ++ show t
      TGuard{..} -> do
        g <- traverse (toAST . return . Left . Direct) _tGuard
        trackPrim _tInfo (TyGuard $ Just $ guardTypeOf _tGuard) (PrimGuard g)
      _ -> die i $ "Native in value context: " <> show t
  (Right t) -> return t

toAST (TApp Term.App{..} _) = do

  fun <- toFun _appFun
  i <- freshId _appInfo $ _fName fun
  n <- trackIdNode i
  args <- mapM toAST _appArgs
  let mkApp fun' args' = return $ App n fun' args'

  case fun of

    FDefun {..} -> do
      if null _fBody
      then assocNode _fRetId n -- TODO: is this necessary?
      else assocAST i (last _fBody)

      mkApp fun args

    FNative {} -> do

      let special = isSpecialForm (NativeDefName $ _fName fun)

      -- Overloaded special forms: eagerly specialize overload based on arg count

      let argCount = length args
          selectOverloadOnArgCount sf =
            case NE.filter ((== argCount) . length . _ftArgs) (_fTypes fun) of
              ft@[_] -> return $ set fTypes (NE.fromList ft) fun
              _ -> die _appInfo $ show sf ++
                   " arg count mismatch, expected: " ++ show (_fTypes fun)

      fun' <- case (special,length (_fTypes fun) > 1) of
        (Just sf,True) -> selectOverloadOnArgCount sf
        _ -> return fun

      -- detect partial app args: for funtype args, add phantom arg to partial app
      args' <- if NE.length (_fTypes fun') > 1 then return args else do
        let funType = NE.head (_fTypes fun')
        (\f -> zipWithM f (_ftArgs funType) args) $ \(Arg _ argTy _) argAST ->
          case (argTy,argAST) of
            (TyFun lambdaTy,App{}) ->
              (\f -> foldM f argAST (_ftArgs lambdaTy)) $ \argAST' (Arg lamArgName _ _) -> do
                freshArg <- trackIdNode =<<
                  freshId (_tiInfo (_aId (_aNode argAST')))
                  (_fName (_aAppFun argAST) <> "_" <> lamArgName <> "_p")
                debug $ "Adding fresh arg to partial application: " ++ show freshArg
                return $ over aAppArgs (++ [Var freshArg]) argAST'
            (TyFun t,a) -> die'' argAST $ "App required for funtype argument: " ++ show t ++ ", found: " ++ show a
            _ -> return argAST

      -- other special forms: bindings, yield/resume
      case special of

        Nothing -> mkApp fun' args'

        Just sf -> do

          let specialBind = do
                initArgs <- init <$> notEmpty _appInfo "Invalid binding invocation" args'
                -- bind forms have binding in last position, move into SBinding
                mkApp (set fSpecial (Just (sf,SBinding (last args'))) fun') initArgs

              setOrAssocYR :: Lens' (YieldResume Node) (Maybe Node) -> Node -> TC ()
              setOrAssocYR yrLens target = do
                use tcYieldResume >>= \yrm -> case yrm of
                  Nothing -> tcYieldResume .= Just (set yrLens (Just $ target) def)
                  Just yr -> case view yrLens yr of
                    Nothing -> tcYieldResume .= Just (set yrLens (Just $ target) yr)
                    Just prevYr -> assocNode (_aId prevYr) target

          case sf of
            Bind -> specialBind
            WithRead -> specialBind
            WithDefaultRead -> specialBind
            WithCapability -> specialBind
            YieldSF -> do
              app' <- mkApp fun' args'
              setOrAssocYR yrYield (_aNode app')
              tcYieldResume . _Just . yrCrossChain .= (argCount >= 2)
              return app'
            Resume -> do
              app' <- specialBind
              case head args' of -- 'specialBind' ensures non-empty args
                (Binding _ _ _ (AstBindSchema sty)) ->
                  setOrAssocYR yrResume sty
                a -> die'' a "Expected binding"
              return app'
            _ -> mkApp fun' args'

toAST TBinding {..} = do
  bi <- freshId _tInfo $ case _tBindType of
    BindLet -> "BindLet"
    BindSchema _ -> "BindSchema"
  bn <- trackIdNode bi
  bs <- forM _tBindPairs $ \(BindPair (Arg n t ai) v) -> do
    aid <- freshId ai (pfx (pack $ show bi) n)
    t' <- mangleType aid <$> traverse toUserType t
    an <- trackNode t' aid
    v' <- toAST v
    case _tBindType of
      BindLet -> do
        assocAST aid v'
        return (Named n an aid,v')
      BindSchema _ -> do
        _fieldName <- asPrimString v'
        return (Named n an aid,v')
  bb <- scopeToBody _tInfo (map (Var . _nnNamed . fst) bs) _tBindBody
  assocAST bi (last bb)
  bt <- case _tBindType of
    BindLet -> return AstBindLet
    BindSchema sty -> do
      sty' <- mangleType bi <$> traverse toUserType sty
      sn <- trackNode sty' =<< freshId _tInfo (pack $ show bi ++ "schema")
      return $ AstBindSchema sn
  return $ Binding bn bs bb bt

toAST TList {..} = do
  ty <- TyList <$> traverse toUserType _tListType
  List <$> (trackNode ty =<< freshId _tInfo "list") <*> mapM toAST (V.toList _tList)
toAST (TObject Term.Object {..} _) = do
  debug $ "TObject: " ++ show _oObjectType
  ty <- TySchema TyObject <$> traverse toUserType _oObjectType <*> pure FullSchema
  Object <$> (trackNode ty =<< freshId _oInfo "object")
    <*> mapM toAST _oObject
toAST TConst {..} = toAST $ constTerm _tConstVal -- TODO(stuart): typecheck here
toAST TGuard {..} = do
  g <- traverse toAST _tGuard
  trackPrim _tInfo (TyGuard $ Just $ guardTypeOf _tGuard) (PrimGuard g)
toAST TLiteral {..} = trackPrim _tInfo (litToPrim _tLiteral) (PrimLit _tLiteral)
toAST TTable {..} = do
  debug $ "TTable: " ++ show _tTableType
  ty <- TySchema TyTable <$> traverse toUserType _tTableType <*> pure FullSchema
  Table
    <$> (trackNode ty =<< freshId _tInfo (asString _tModuleName <> "." <> asString _tTableName))
    <*> pure _tTableName
toAST TModule {..} = die _tInfo "Modules not supported"
toAST TUse {..} = die _tInfo "Use not supported"
toAST (TStep Term.Step {..} (Meta _doc model) _) = do
  ent <- forM _sEntity $ \e -> do
    e' <- toAST e
    assocAstTy (_aNode e') $ TyPrim TyString
    return e'
  si <- freshId _sInfo "step"
  sn <- trackIdNode si
  tcYieldResume .= Nothing
  ex <- toAST _sExec
  assocAST si ex
  yr <- state (_tcYieldResume &&& set tcYieldResume Nothing)
  Step sn ent ex <$> traverse toAST _sRollback <*> pure yr <*> pure model
toAST TDynamic {..} = do -- only comes up for consts, functions are in 'toFun'
  r <- toAST _tDynModRef
  m <- toFun _tDynMember
  n <- trackIdNode =<< freshId _tInfo (renderCompactText r)
  return $ Dynamic n r m

trackPrim :: Info -> PrimType -> PrimValue (AST Node) -> TC (AST Node)
trackPrim inf pty v = do
  let ty :: Type UserType = TyPrim pty
  Prim <$> (trackNode ty =<< freshId inf (pack $ showPretty ty) ) <*> pure v

trackNode :: Type UserType -> TcId -> TC Node
trackNode ty i = trackAST node >> return node
  where node = Node i ty

-- | Main type transform, expecting that vars can only refer to a user type.
toUserType :: forall n . Show n => Term (Either Ref n) -> TC UserType
toUserType t = case t of
  (TVar (Left r) _) -> derefUT r
  _ -> die (_tInfo t) $ "toUserType: expected var of user type ref: " ++ show t
  where
    derefUT (Ref r) = toUserType' (fmap Left r :: Term (Either Ref n))
    derefUT (Direct d) = toUserType' (fmap Right d)

toUserType' :: Show n => Term (Either Ref n) -> TC UserType
toUserType' TSchema {..} = fmap UTSchema $ Schema _tSchemaName _tModule
  <$> mapM (traverse toUserType) _tFields
  <*> pure _tInfo
toUserType' TModRef {..} = case _modRefSpec _tModRef of
  Nothing -> return $ UTModSpec (ModSpec $ _modRefName _tModRef)
  Just{} -> die _tInfo "toUserType': expected interface modref"
toUserType' t = die (_tInfo t) $ "toUserType': expected user type: " ++ show t

bindArgs :: Info -> [a] -> Int -> TC a
bindArgs i args b = case args ^? ix b of
  Nothing -> die i $ "Missing arg: " ++ show b ++ ", " ++ show (length args) ++ " provided"
  Just a -> return a

-- Temporary data type we're using to define 'Abbrev' for this 'Either'.
newtype AbbrevNode = AbbrevNode (Either Ref (AST Node))

instance Pretty AbbrevNode where
  pretty (AbbrevNode node) = either pretty pretty node

-- | Convert a top-level Term to a TopLevel.
mkTop :: Term (Either Ref (AST Node)) -> TC (TopLevel Node)
mkTop t@TDef {..} = do
  debug $ "===== Fun: " ++ abbrevStr (AbbrevNode <$> t)
  TopFun <$> toFun t <*> pure (_dMeta _tDef)
mkTop t@TConst {..} = do
  debug $ "===== Const: " ++ abbrevStr (AbbrevNode <$> t)
  TopConst _tInfo (asString _tModule <> "." <> _aName _tConstArg) <$>
    traverse toUserType (_aType _tConstArg) <*>
    toAST (constTerm _tConstVal) <*> pure (_mDocs _tMeta)
mkTop t@TTable {..} = do
  debug $ "===== Table: " ++ abbrevStr (AbbrevNode <$> t)
  TopTable _tInfo (asString _tModuleName <> "." <> asString _tTableName) <$>
    traverse toUserType _tTableType <*> pure _tMeta
mkTop t@TSchema {..} = do
  debug $ "===== Schema: " ++ abbrevStr (AbbrevNode <$> t)
  TopUserType _tInfo <$> toUserType' t <*> pure (_mDocs _tMeta)
mkTop t = die (_tInfo t) $ "Invalid top-level term: " ++ abbrevStr (AbbrevNode <$> t)


-- | Recursively compute the "leaf type" where
-- vars are resolved to non-vars, if possible;
-- schemas/lists are specialized.
-- Recursive references are detected and error out.
resolveTy :: Type UserType -> TC (Type UserType)
resolveTy rt = do
  v2Ty <- use tcVarToTypes
  let resolv :: Type UserType -> State [Type UserType] (Maybe (Type UserType))
      resolv tv@(TyVar v) =
        case M.lookup v v2Ty of
          Just t | t /= tv -> go t
                 | otherwise -> return (Just tv)
          Nothing -> return (Just tv)
      resolv (TySchema s st p) = fmap (\t -> TySchema s t p) <$> go st
      resolv (TyList l) = fmap TyList <$> go l
      resolv t = return (Just t)
      go t = do
        ts <- get
        if t `elem` ts then
          return Nothing
          else modify (t:) >> resolv t
  case runState (go rt) [] of
    (Just r,_) -> return r
    (Nothing,rs) -> die def $ "Error: recursive type vars: " ++ show rs




-- | Is this type a variable, or does it have any parameterized variables
isUnresolvedTy :: Type n -> Bool
isUnresolvedTy TyVar {} = True
isUnresolvedTy (TySchema _ v _) = isUnresolvedTy v
isUnresolvedTy (TyList l) = isUnresolvedTy l
isUnresolvedTy _ = False -- TODO fun types

-- | A successful result has the '_tcAstToVar' map populated with "resolved types", ie concrete non-var types.
resolveAllTypes :: TC (M.Map TcId (Type UserType))
resolveAllTypes = do
  ast2Ty <- use tcAstToVar >>= \a2v -> (`M.traverseWithKey` a2v) $ \i tv -> do
    tysm <- M.lookup tv <$> use tcVarToTypes
    case tysm of
      Nothing -> die def $ "resolveAllTypes: untracked type var: " ++ show (i,tv)
      Just tys -> resolveTy tys
  let unresolved = M.filter isUnresolvedTy ast2Ty
  if M.null unresolved then debug "Successfully resolved all types"
    else forM_ (M.toList unresolved) $ \(i,v) ->
      addFailure i $ "Unable to resolve type" ++
        (case v of
           (TyVar (TypeVar _ [])) -> ""
           (TyVar (TypeVar _ as)) -> " " ++ showPretty as
           _ -> " (" ++ show v ++ ")")
  return ast2Ty

_debugState :: TC ()
_debugState = liftIO . putDoc . pretty =<< get

showFails :: TC ()
showFails = do
  fails <- use tcFailures
  unless (S.null fails) $ liftIO $ putDoc (prettyFails fails <> hardline)

-- | unsafe lens for using `typecheckBody` with const
singLens :: Iso' a [a]
singLens = iso pure head

-- | Typecheck a top-level production.
typecheck :: TopLevel Node -> TC (TopLevel Node)
typecheck f@(TopFun FDefun {} _) = typecheckBody f (tlFun . fBody)
typecheck c@TopConst {..} = do
  assocAstTy (_aNode _tlConstVal) _tlType
  typecheckBody c (tlConstVal . singLens)
typecheck tl = return tl


-- | Workhorse function. Perform AST substitutions, associate types, solve overloads,
-- enforce schemas, resolve all type variables, populate back into AST.
typecheckBody :: TopLevel Node -> Traversal' (TopLevel Node) [AST Node] -> TC (TopLevel Node)
typecheckBody tl bodyLens = do
  let body = view bodyLens tl
  debug "--------------------------------\n Substitute defuns:\n"
  appSub <- mapM (walkAST $ substAppDefun Nothing) body
  debug "--------------------------------\nSubstitute natives:\n"
  nativesProc <- mapM (walkAST processNatives) appSub
  debug "--------------------------------\nAssoc Yield/Resume:\n"
  assocStepYieldReturns tl nativesProc
  debug "--------------------------------\nApply Schemas:\n"
  schEnforced <- mapM (walkAST applySchemas) nativesProc
  debug "--------------------------------\nSolve Overloads:\n"
  solveOverloads
  debug "--------------------------------\nResolve types:\n"
  ast2Ty <- resolveAllTypes
  fails <- use tcFailures
  dbg <- use tcDebug
  when (dbg && not (S.null fails)) $ liftIO $ putDoc (prettyFails fails <> hardline)
  let tl' = set bodyLens schEnforced tl
  forM tl' $ \n@(Node i _) -> case M.lookup i ast2Ty of
    Nothing -> die' i $ "Failed to find tracked AST for node: " ++ show n
    Just ty -> return (Node i ty)

-- | Typecheck a single module production.
typecheckTopLevel :: Ref -> TC (TopLevel Node)
typecheckTopLevel (Ref r) = do
  tl <- mkTop (fmap Left r)
  tl' <- typecheck tl
  debug $ "\n===== Done: " ++ abbrevStr r
  return tl'
typecheckTopLevel (Direct d) = die (_tInfo d) $ "Unexpected direct ref: " ++ abbrevStr d

-- | Typecheck all productions in a module.
typecheckModule :: Bool -> DynEnv -> ModuleData Ref -> IO ([TopLevel Node],[Failure])
typecheckModule dbg dynEnv (ModuleData (MDModule m) rm) = do
  debug' dbg $ "Typechecking module " ++ show (_mName m)
  let tc ((tls,fails),sup) r = do
        (tl,TcState {..}) <- runTCState (mkTcState sup dbg dynEnv) (typecheckTopLevel r)
        return ((tl:tls,fails ++ toList _tcFailures),succ _tcSupply)
  fst <$> foldM tc (([],[]),0) (HM.elems rm)
typecheckModule _ _ (ModuleData MDInterface{} _) = return mempty

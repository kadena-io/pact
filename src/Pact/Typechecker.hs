{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Monad.Catch
import Control.Lens hiding (pre,List,Fold)
import Bound.Scope
import Safe hiding (at)
import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import Control.Arrow hiding ((<+>))
import Data.Foldable
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>),(<>))
import Data.String


import Pact.Types.Typecheck
import Pact.Types.Runtime
import Pact.Native.Internal

die :: MonadThrow m => Info -> String -> m a
die i s = throwM $ CheckerException i s

die' :: MonadThrow m => TcId -> String -> m a
die' i = die (_tiInfo i)



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
walkAST f t@Object {} = do
  Object {..} <- f Pre t
  t' <- Object _aNode <$>
         forM _aObject (\(k,v) -> (,) <$> walkAST f k <*> walkAST f v)
  f Post t'
walkAST f t@List {} = do
  List {..} <- f Pre t
  t' <- List _aNode <$> mapM (walkAST f) _aList
  f Post t'
walkAST f t@Binding {} = do
  Binding {..} <- f Pre t
  t' <- Binding _aNode <$>
        forM _aBindings (\(k,v) -> (k,) <$> walkAST f v) <*>
        mapM (walkAST f) _aBody <*> pure _aBindType
  f Post t'
walkAST f t@App {} = do
  App {..} <- f Pre t
  t' <- App _aNode <$>
        (case _aAppFun of
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
        mapM (walkAST f) _aAppArgs
  f Post t'
walkAST f t@Step {} = do
  Step {..} <- f Pre t
  t' <- Step _aNode <$> walkAST f _aEntity <*> walkAST f _aExec <*> traverse (walkAST f) _aRollback
  f Post t'

isConcreteTy :: Type n -> Bool
isConcreteTy ty = not (isAnyTy ty || isVarTy ty)

-- | Run through all overloads attempting to find all-concrete type solution for each.
solveOverloads :: TC ()
solveOverloads = do

  overs <- use tcOverloads >>=
           traverse (traverse (\i -> ((i,) . fst) <$> lookupAst "solveOverloads" (_aId (_aNode i))))

  let runSolve os = forM os $ \(o@Overload {..}) -> case _oSolved of
        Just {} -> return o
        Nothing -> (\s -> set oSolved s o) <$> foldM (tryFunType o) Nothing _oTypes

      rptSolve os = runSolve os >>= \os' -> if os' == os then return os' else rptSolve os'

  done <- rptSolve overs
  tcOverloads .= fmap (fmap fst) done

  forM_ (M.toList done) $ \(i,o) -> case _oSolved o of
      Nothing -> do
        debug $ "Unable to solve overload: " ++ show (i,o)
        addFailure i $ "Unable to solve overloaded function: " ++ show o
      Just {} -> return ()


newtype RoleTys = RoleTys (Type UserType,AST Node,TypeVar UserType,Type UserType)
instance Show RoleTys where show (RoleTys (a,b,c,d)) = show (a,_aId (_aNode b),c,d)
_roleTys :: Lens' RoleTys (Type UserType,AST Node,TypeVar UserType,Type UserType)
_roleTys f (RoleTys rt) = RoleTys <$> f rt


-- | Attempt to solve an overload with one of its candidate types.
tryFunType :: Overload (AST Node,TypeVar UserType) -> Maybe (FunType UserType) -> FunType UserType ->
               TC (Maybe (FunType UserType))
tryFunType _ r@Just {} _ = return r
tryFunType Overload {..} _ f@(FunType as rt) = do

  -- see if var role unifies with with fun role. If so, return singleton list of var information
  -- indexed by fun role.
  let tryRole :: VarRole -> Type UserType ->
                 TC (Maybe (VarRole,RoleTys))
      tryRole rol fty = case M.lookup rol _oRoles of
        Nothing -> return Nothing
        Just (i,tv) -> use tcVarToTypes >>= \m -> case M.lookup tv m of
          Nothing -> die' (_aId (_aNode i)) $ "Bad var in overload solver: " ++ show tv
          Just ty -> case unifyTypes fty ty of
            Nothing -> return Nothing
            Just _ -> return $ Just (rol,RoleTys (fty,i,tv,ty)) -- (Just (fty,[(i,tv,ty)]))

  -- try arg and return roles
  argRoles <- forM (zip as [0..]) $ \(Arg _ fty _,ai) -> tryRole (ArgVar ai) fty
  allRoles <- (:argRoles) <$> tryRole RetVar rt
  case sequence allRoles of
    Nothing -> do
      debug $ "tryFunType: failed: " ++ show f ++ ": " ++ show allRoles
      return Nothing
    Just ars -> do
      let byRole = handleSpecialOverload _oSpecial $ M.fromList ars
          byFunType = M.fromListWith (++) $ map (\(RoleTys (fty,i,tv,ty)) -> (fty,[(_aId (_aNode i),tv,ty)])) $ M.elems byRole

      debug $ "tryFunType: trying " ++ show (f,byFunType)

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

              debug $ "Solved overload with " ++ show f ++ ": " ++ show solved
              forM_ solved $ \(fty,(uty,tvs)) -> forM_ tvs $ \tv -> do
                debug $ "Adjusting type for solution: " ++ show (tv,fty,uty)
                uncurry assocTy tv fty
                uncurry assocTy tv uty
              return $ Just f

          | otherwise -> do
              debug $ "tryFunType: not all concrete: " ++ show solved
              return Nothing


handleSpecialOverload :: Maybe OverloadSpecial ->
                         M.Map VarRole RoleTys ->
                         M.Map VarRole RoleTys
handleSpecialOverload Nothing m = m
handleSpecialOverload (Just OAt) m =
  case (M.lookup (ArgVar 0) m,M.lookup (ArgVar 1) m,M.lookup RetVar m) of
    (Just keyArg,Just srcArg,Just ret) -> case (roleTy keyArg,view (_roleTys . _2) keyArg,roleTy srcArg) of
      (TyPrim TyString,Prim _ (PrimLit (LString k)),TySchema TyObject (TyUser u)) -> case findField k u of
        Nothing -> m
        Just t -> unifyRet (roleTy ret) t
      (TyPrim TyInteger,_,TyList t) -> unifyRet (roleTy ret) t
      _ -> m
    _ -> m
  where
    findField fname Schema {..} =
      foldl (\r Arg {..} -> mplus (if _aName == fname then Just _aType else Nothing) r) Nothing _utFields
    roleTy = view (_roleTys . _4)
    unifyRet ret ty = case unifyTypes ret ty of
                        Nothing -> m
                        Just e -> set (at RetVar . _Just . _roleTys . _4) (either id id e) m




asPrimString :: AST Node -> TC Text
asPrimString (Prim _ (PrimLit (LString s))) = return s
asPrimString t = die (_tiInfo (_aId (_aNode t))) $ "Expected literal string: " ++ show t

-- | Visitor to inspect Objects and Bindings to validate schemas,
-- "pushing down" the field types to associate with field ASTs.
applySchemas :: Visitor TC Node
applySchemas Pre ast = case ast of
  (Object n ps) -> findSchema n $ \sch -> do
    debug $ "applySchemas: " ++ show (n,sch)
    pmap <- M.fromList <$> forM ps
      (\(k,v) -> (,) <$> asPrimString k <*> ((v,_aId (_aNode k),) <$> lookupTy (_aNode v)))
    validateSchema sch pmap
    return ast
  (Binding _ bs _ (BindSchema n)) -> findSchema n $ \sch -> do
    debug $ "applySchemas: " ++ show (n,sch)
    pmap <- M.fromList <$> forM bs
      (\(Named bn _ ni,v) -> (bn,) <$> ((v,ni,) <$> lookupTy (_aNode v)))
    validateSchema sch pmap
    return ast
  _ -> return ast
  where
    validateSchema sch pmap = do
      let smap = M.fromList <$> (`map` _utFields sch) $ \(Arg an aty _) -> (an,aty)
      forM_ (M.toList pmap) $ \(k,(v,ki,vty)) -> case M.lookup k smap of
        Nothing -> addFailure ki $ "Invalid field in schema object: " ++ show k
        Just aty -> case unifyTypes aty vty of
          Nothing -> addFailure (_aId (_aNode v)) $ "Unable to unify field type: " ++ show (k,aty,vty,v)
          Just u -> assocAstTy (_aNode v) (either id id u)
    lookupTy a = resolveTy =<< (snd <$> lookupAst "lookupTy" (_aId a))
    findSchema n act = do
      ty <- lookupTy n
      case ty of
        (TySchema _ (TyUser sch)) -> act sch
        _ -> return ast
applySchemas Post a = return a

-- | Visitor to process Apps of native funs.
-- For non-overloads, associate fun tys with app args and result.
-- Overloads are simply tracked to app args and result.
-- Special form support includes:
--  1. for 'map', associate lambda result type for arg AST
--  2. for bindings, associate binding AST with fun return ty,
--     associate inner binding schema type with funty last arg ty
processNatives :: Visitor TC Node
processNatives Pre a@(App i FNative {..} as) = do
  case _fTypes of
    -- single funtype
    ft@FunType {} :| [] -> do
      let adjustForSpecial = case _fSpecial of
            Just (Map,_) -> replaceFunTyWithReturnTy 0
            Just (Filter,_) -> replaceFunTyWithReturnTy 0
            Just (Fold,_) -> replaceFunTyWithReturnTy 0
            Just (Compose,_) -> replaceFunTyWithReturnTy 0 . replaceFunTyWithReturnTy 1
            _ -> id
          replaceFunTyWithReturnTy argIx = over (ftArgs . ix argIx . aType)
              (\ty -> case ty of
                  TyFun (FunType _ r) -> r -- replace map funty with just return val
                  _ -> ty)
          FunType {..} = mangleFunType (_aId i) $ adjustForSpecial ft
      zipWithM_ (\(Arg _ t _) aa -> assocAstTy (_aNode aa) t) _ftArgs as
      assocAstTy i _ftReturn
      -- the following assumes that special forms are never overloaded!
      case _fSpecial of
        -- with-read et al have a single Binding body, associate this with return type
        Just (_,SBinding b) -> case b of
          (Binding bn _ _ (BindSchema sn)) -> do
            assocAstTy bn _ftReturn
            -- assoc schema with last ft arg
            assocAstTy sn (_aType (last (toList _ftArgs)))
          sb -> die _fInfo $ "Invalid special form, expected binding: " ++ show sb
        _ -> return () -- todo partials etc
    -- multiple funtypes
    fts -> do
      let fts' = fmap (mangleFunType (_aId i)) fts
          argOvers = mconcat $ zipWith (\n aa -> M.singleton (ArgVar n) aa) [0..] as
          ospec = if _fName == "at" then Just OAt else Nothing
          oload = Overload (argOvers <> M.singleton RetVar a) fts' Nothing ospec
      tcOverloads %= M.insert (_aId i) oload
  return a
processNatives _ a = return a

-- | Visitor to process Apps of user defuns.
-- We want to a) replace AST nodes with the app arg ASTs,
-- b) associate the AST nodes to check and track their related types
substAppDefun :: Maybe (TcId, AST Node) -> Visitor TC Node
substAppDefun (Just (defArg,appAst)) Pre t@Var {..}
  | defArg == _aId _aNode = assocAST defArg appAst >> return appAst
  | otherwise = return t
substAppDefun Nothing Post (App appNode appFun appArgs) = do -- Post, to allow AST subs first
    af <- case appFun of
      FNative {} -> return appFun -- noop
      FDefun {..} -> do
        -- assemble substitutions, and also associate fun ty with app args
        let mangledFty = mangleFunType (_aId appNode) _fType
        subArgs <- forM (zip3 _fArgs appArgs (_ftArgs mangledFty)) $ \(fa,aa,_ft) -> -- do
          -- assocAstTy (_aNode aa) (_aType ft) -- this was causing recursive var refs, bailing for now
          return (_aId (_nnNamed fa),aa)
        -- associate fun return ty with app
        assocAstTy appNode (_ftReturn mangledFty)
        let subDefArg b fa = walkAST (substAppDefun (Just fa)) b
        fb' <- forM _fBody $ \bAst -> foldM subDefArg bAst subArgs
        return $ set fBody fb' appFun
    return (App appNode af appArgs)
substAppDefun _ _ t = return t

-- | Track AST as a TypeVar pointing to a Types. If the provided node type is already a var use that,
-- otherwise make a new var based on the TcId.
trackAST :: Node -> TC ()
trackAST (Node i t) = do
  debug $ "trackAST: " ++ show (i,t)
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
  aty <- lookupTypes "assocTy" ai av
  debug $ "assocTy: " ++ show (av,aty) ++ " <=> " ++ show ty
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
              debug $ "assocTy: " ++ show aty ++ " => " ++ show tv
              tcVarToTypes %= M.insert tv aty
            Just tvtys ->
              -- RH is tyvar tracking type. Unify and update tracked type.
              unifyTypes' ai aty tvtys $ \r' ->
                (tcVarToTypes . at tv . _Just) .= either id id r'
        _ -> debug $ "assocTy: noop: " ++ show (aty,ty)
    Right u -> do
      -- Associated ty is most specialized, simply update record for AST type.
      debug $ "assocTy: " ++ show (av,aty) ++ " <= " ++ show u
      tcVarToTypes . at av . _Just .= u
      assocParams ai u aty
      -- if old type was var, make entry and adjust it
      case aty of
        TyVar atyv | u /= aty -> do
                       debug $ "assocTy: tracking/updating type variable " ++ show atyv ++ " <= " ++ show u
                       alterTypes atyv u (const u)
        _ -> return ()


-- | Set, or update, a tracked 'Types' value.
alterTypes :: TypeVar UserType -> Type UserType -> (Type UserType -> Type UserType) -> TC ()
alterTypes v newVal upd = tcVarToTypes %= M.alter (Just . maybe newVal upd) v


-- | Investigate types "inside of" schemas and lists to see if they can associate.
assocParams :: TcId -> Type UserType -> Type UserType -> TC ()
assocParams i x y = case (x,y) of
  _ | x == y -> return ()
  (TySchema _ a,TySchema _ b) -> assoc a b
  (TyList a,TyList b) -> assoc a b
  _ -> return ()
  where
    createMaybe v = alterTypes v (TyVar v) id
    assoc (TyVar a) b | b /= TyAny = createMaybe a >> assocTy i a b
    assoc a (TyVar b) | a /= TyAny = createMaybe b >> assocTy i b a
    assoc _ _ = return ()

-- | Substitute AST types to most specialized.
-- Not as thorough as `assocTy`, should probably use it.
assocAST :: TcId -> AST Node -> TC ()
assocAST ai b = do
  let bi = _aId (_aNode b)
  (av,aty) <- lookupAst "assocAST" ai
  (bv,bty) <- lookupAst "assocAST" bi
  let doSub si sv sty fi fv fty = do
        debug $ "assocAST: " ++ show (si,sv,sty) ++ " => " ++ show (fi,fv,fty)
        -- reassign any references to old var to new
        tcAstToVar %= fmap (\v -> if v == fv then sv else v)
        -- cleanup old var
        unless (sv == fv) $ tcVarToTypes %= M.delete fv
  case unifyTypes aty bty of
    Nothing -> addFailure bi $ "assocAST: cannot unify: " ++ show (aty,bty)
    Just (Left _) -> doSub ai av aty bi bv bty
    Just (Right _) -> doSub bi bv bty ai av aty

-- | Unify two types and note failure.
unifyTypes' :: (Show n,Eq n) => TcId -> Type n -> Type n -> (Either (Type n) (Type n) -> TC ()) -> TC ()
unifyTypes' i a b act = case unifyTypes a b of
  Just r -> act r
  Nothing -> do
    addFailure i $ "Cannot unify: " ++ show (a,b)
    return ()

-- | Unify two types, indicating which of the types was more specialized with the Either result.
unifyTypes :: Eq n => Type n -> Type n -> Maybe (Either (Type n) (Type n))
unifyTypes l r = case (l,r) of
  _ | l == r -> Just (Right r)
  (TyAny,_) -> Just (Right r)
  (_,TyAny) -> Just (Left l)
  (TyVar v,s) -> unifyVar Left Right v s
  (s,TyVar v) -> unifyVar Right Left v s
  (TyList a,TyList b) -> unifyParam a b
  (TySchema sa a,TySchema sb b) | sa == sb -> unifyParam a b
  _ -> Nothing
  where
    unifyParam a b = fmap (either (const (Left l)) (const (Right r))) (unifyTypes a b)
    unifyVar vc sc v s =
      let vWins = Just (vc (TyVar v))
          sWins = Just (sc s)
      in case (v,s) of
        (SchemaVar {},TyUser {}) -> sWins
        (SchemaVar {},TyVar SchemaVar {}) -> sWins
        (SchemaVar {},_) -> Nothing
        (TypeVar {},TyVar SchemaVar {}) -> Nothing
        (TypeVar {},TyUser {}) -> Nothing
        (TypeVar _ ac,TyVar (TypeVar _ bc)) | null ac -> sWins
                                            | null bc -> vWins
                                            | all (`elem` ac) bc -> sWins
                                            | all (`elem` bc) ac -> vWins
                                            | otherwise -> Nothing
        (TypeVar _ ac,_) | null ac || s `elem` ac -> sWins
        _ -> Nothing

-- | Instantiate a Bound scope as AST nodes or references.
scopeToBody :: Info -> [AST Node] -> Scope Int Term (Either Ref (AST Node)) -> TC [AST Node]
scopeToBody i args bod = do
  bt <- instantiate (return . Right) <$> traverseScope (bindArgs i args) return bod
  case bt of
    (TList ts@(_:_) _ _) -> mapM toAST ts -- verifies non-empty body.
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
  ft' <- traverse (traverse toUserType') _tFunTypes
  return $ FNative _tInfo (asString _tNativeName) ft' Nothing -- we deal with special form in App
toFun (TVar (Left (Ref r)) _) = toFun (fmap Left r)
toFun (TVar Right {} i) = die i "Value in fun position"
toFun TDef {..} = do -- TODO currently creating new vars every time, is this ideal?
  let fn = asString _tModule <> "." <> asString _tDefName
  args <- forM (_ftArgs _tFunType) $ \(Arg n t ai) -> do
    an <- freshId ai $ pfx fn n
    t' <- mangleType an <$> traverse toUserType t
    Named n <$> trackNode t' an <*> pure an
  tcs <- scopeToBody _tInfo (map (\ai -> Var (_nnNamed ai)) args) _tDefBody
  ft' <- traverse toUserType _tFunType
  return $ FDefun _tInfo fn ft' args tcs _tDocs
toFun t = die (_tInfo t) "Non-var in fun position"


notEmpty :: MonadThrow m => Info -> String -> [a] -> m [a]
notEmpty i msg [] = die i msg
notEmpty _ _ as = return as



-- | Build ASTs from terms.
toAST :: Term (Either Ref (AST Node)) -> TC (AST Node)
toAST TNative {..} = die _tInfo "Native in value position"
toAST TDef {..} = die _tInfo "Def in value position"
toAST TSchema {..} = die _tInfo "User type in value position"
toAST (TVar v i) = case v of -- value position only, TApp has its own resolver
  (Left (Ref r)) -> toAST (fmap Left r)
  (Left Direct {}) -> die i "Native in value context"
  (Right t) -> return t
toAST TApp {..} = do
  fun <- toFun _tAppFun
  i <- freshId _tInfo $
       "app" <> (case fun of FDefun {} -> "D"; _ -> "N") <>  _fName fun
  n <- trackIdNode i
  as <- mapM toAST _tAppArgs
  (as',fun') <- case fun of
    FDefun {..} -> assocAST i (last _fBody) >> return (as,fun) -- non-empty verified in 'scopeToBody'
    FNative {..} -> case isSpecialForm (NativeDefName _fName) of
      Nothing -> return (as,fun)
      Just sf -> do
        let specialBind = (,) <$> notEmpty _tInfo "Expected >1 arg" (init as)
                          <*> pure (set fSpecial (Just (sf,SBinding (last as))) fun)
            specialOther s as' = return (as',set fSpecial (Just (sf,s)) fun)
        case sf of
          Bind -> specialBind
          WithRead -> specialBind
          WithDefaultRead -> specialBind
          Map -> case as of
            [] -> do
              addFailure i "map with no arguments encountered"
              specialOther SPartial as
            _ -> do
              -- create Var AST and append to arg list of partial apply.
              mpa <- trackIdNode =<< freshId (_tiInfo (_aId (_aNode (head as)))) "map-partial-apply"
              specialOther SPartial (over (ix 0 . aAppArgs) (++ [Var mpa]) as)
          Filter -> case as of
            [] -> do
              addFailure i "filter with no arguments encountered"
              specialOther SPartial as
            _ -> do
              -- create Var AST and append to arg list of partial apply.
              mpa <- trackIdNode =<< freshId (_tiInfo (_aId (_aNode (head as)))) "filter-partial-apply"
              specialOther SPartial (over (ix 0 . aAppArgs) (++ [Var mpa]) as)
          _ -> specialOther SPartial as

  return $ App n fun' as'

toAST TBinding {..} = do
  bi <- freshId _tInfo (pack $ show _tBindType)
  bn <- trackIdNode bi
  bs <- forM _tBindPairs $ \(Arg n t ai,v) -> do
    aid <- freshId ai (pfx (pack $ show bi) n)
    t' <- mangleType aid <$> traverse toUserType t
    an <- trackNode t' aid
    v' <- toAST v
    case _tBindType of
      BindLet -> do
        assocAST aid v'
        return (Named n an aid,v')
      BindSchema _ -> do
        fieldName <- asPrimString v'
        return (Named fieldName an aid,Var an)
  bb <- scopeToBody _tInfo (map ((\ai -> Var (_nnNamed ai)).fst) bs) _tBindBody
  bt <- case _tBindType of
    BindLet -> assocAST bi (last bb) >> return BindLet
    BindSchema sty -> do
      assocAST bi (last bb)
      sty' <- mangleType bi <$> traverse toUserType sty
      sn <- trackNode sty' =<< freshId _tInfo (pack $ show bi ++ "schema")
      return $ BindSchema sn
  return $ Binding bn bs bb bt

toAST TList {..} = do
  ty <- TyList <$> traverse toUserType _tListType
  List <$> (trackNode ty =<< freshId _tInfo "list") <*> mapM toAST _tList
toAST TObject {..} = do
  debug $ "TObject: " ++ show _tObjectType
  ty <- TySchema TyObject <$> traverse toUserType _tObjectType
  Object <$> (trackNode ty =<< freshId _tInfo "object")
    <*> mapM (\(k,v) -> (,) <$> toAST k <*> toAST v) _tObject
toAST TConst {..} = toAST _tConstVal -- TODO typecheck here
toAST TKeySet {..} = trackPrim _tInfo TyKeySet (PrimKeySet _tKeySet)
toAST TValue {..} = trackPrim _tInfo TyValue (PrimValue _tValue)
toAST TLiteral {..} = trackPrim _tInfo (litToPrim _tLiteral) (PrimLit _tLiteral)
toAST TTable {..} = do
  debug $ "TTable: " ++ show _tTableType
  ty <- TySchema TyTable <$> traverse toUserType _tTableType
  Table <$> (trackNode ty =<< freshId _tInfo (asString _tModule <> "." <> asString _tTableName))
toAST TModule {..} = die _tInfo "Modules not supported"
toAST TUse {..} = die _tInfo "Use not supported"
toAST TStep {..} = do
  ent <- toAST _tStepEntity
  assocAstTy (_aNode ent) $ TyPrim TyString
  si <- freshId _tInfo "step"
  sn <- trackIdNode si
  ex <- toAST _tStepExec
  assocAST si ex
  Step sn ent ex <$> traverse toAST _tStepRollback

trackPrim :: Info -> PrimType -> PrimValue -> TC (AST Node)
trackPrim inf pty v = do
  let ty :: Type UserType = TyPrim pty
  Prim <$> (trackNode ty =<< freshId inf (pack $ show ty) ) <*> pure v

trackNode :: Type UserType -> TcId -> TC Node
trackNode ty i = trackAST node >> return node
  where node = Node i ty

-- | Main type transform, expecting that vars can only refer to a user type.
toUserType :: Term (Either Ref (AST Node)) -> TC UserType
toUserType t = case t of
  (TVar (Left r) _) -> derefUT r
  _ -> die (_tInfo t) $ "toUserType: expected user type: " ++ show t
  where
    derefUT (Ref r) = toUserType' r
    derefUT Direct {} = die (_tInfo t) $ "toUserType: unexpected direct ref: " ++ show t

toUserType' :: Show n => Term n -> TC UserType
toUserType' TSchema {..} = Schema _tSchemaName _tModule <$> mapM (traverse toUserType') _tFields <*> pure _tInfo
toUserType' t = die (_tInfo t) $ "toUserType: expected user type: " ++ show t

bindArgs :: Info -> [a] -> Int -> TC a
bindArgs i args b =
  case args `atMay` b of
    Nothing -> die i $ "Missing arg: " ++ show b ++ ", " ++ show (length args) ++ " provided"
    Just a -> return a

-- | Convert a top-level Term to a TopLevel.
mkTop :: Term (Either Ref (AST Node)) -> TC (TopLevel Node)
mkTop t@TDef {} = do
  debug $ "===== Fun: " ++ abbrev t
  TopFun <$> toFun t
mkTop t@TConst {..} = do
  debug $ "===== Const: " ++ abbrev t
  TopConst _tInfo (asString _tModule <> "." <> _aName _tConstArg) <$>
    traverse toUserType (_aType _tConstArg) <*>
    toAST _tConstVal
mkTop t@TTable {..} = do
  debug $ "===== Table: " ++ abbrev t
  TopTable _tInfo (asString _tModule <> "." <> asString _tTableName) <$>
    traverse toUserType _tTableType
mkTop t@TSchema {..} = do
  debug $ "===== Schema: " ++ abbrev t
  TopUserType _tInfo <$> toUserType' t
mkTop t = die (_tInfo t) $ "Invalid top-level term: " ++ abbrev t


-- | Recursively find the "leaf type" for a type which could be a var,
-- or have a parameterized type in it.
resolveTy :: Type UserType -> TC (Type UserType)
resolveTy rt = do
  v2Ty <- use tcVarToTypes
  let resolv tv@(TyVar v) =
        case M.lookup v v2Ty of
          Just t | t /= tv -> go t
                 | otherwise -> return (Just tv)
          Nothing -> return (Just tv)
      resolv (TySchema s st) = fmap (TySchema s) <$> go st
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
isUnresolvedTy (TySchema _ v) = isUnresolvedTy v
isUnresolvedTy (TyList l) = isUnresolvedTy l
isUnresolvedTy _ = False -- TODO fun types

prettyMap :: (t -> Doc) -> (t1 -> Doc) -> M.Map t t1 -> Doc
prettyMap prettyK prettyV = vsep . map (\(k,v) -> prettyK k <> colon <+> prettyV v) . M.toList

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
           (TyVar (TypeVar _ as)) -> " " ++ show as
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
typecheck f@(TopFun FDefun {}) = typecheckBody (f,tlFun . fBody)
typecheck c@TopConst {..} = do
  assocAstTy (_aNode _tlConstVal) _tlType
  typecheckBody (c,tlConstVal . singLens)
typecheck tl = return tl


-- | Workhorse function. Perform AST substitutions, associate types, solve overloads,
-- enforce schemas, resolve all type variables, populate back into AST.
typecheckBody :: (TopLevel Node,Traversal' (TopLevel Node) [AST Node]) -> TC (TopLevel Node)
typecheckBody (tl,bodyLens) = do
  let body = view bodyLens tl
  debug "Substitute defuns"
  appSub <- mapM (walkAST $ substAppDefun Nothing) body
  debug "Substitute natives"
  nativesProc <- mapM (walkAST processNatives) appSub
  debug "Apply Schemas"
  schEnforced <- mapM (walkAST applySchemas) nativesProc
  debug "Solve Overloads"
  solveOverloads
  debug "Resolve types"
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
  debug $ "===== Done: " ++ abbrev r
  return tl'
typecheckTopLevel (Direct d) = die (_tInfo d) $ "Unexpected direct ref: " ++ abbrev d

-- | Typecheck all productions in a module.
typecheckModule :: Bool -> ModuleData -> IO ([TopLevel Node],[Failure])
typecheckModule dbg (Module {..},refs) = do
  debug' dbg $ "Typechecking module " ++ show _mName
  let tc ((tls,fails),sup) r = do
        (tl,TcState {..}) <- runTC sup dbg (typecheckTopLevel r)
        return ((tl:tls,fails ++ toList _tcFailures),succ _tcSupply)
  fst <$> foldM tc (([],[]),0) (HM.elems refs)

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Typecheck where

import Pact.Repl
import Pact.Types
import Pact.Native.Internal
import Control.Monad.Catch
import Control.Lens hiding (pre,List)
import Bound.Scope
import Safe hiding (at)
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty (..))
import Control.Arrow hiding ((<+>))
import Data.Aeson hiding (Object, (.=))
import Data.Foldable
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>))
import Data.String
import Data.Maybe
import Data.List
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data CheckerException = CheckerException Info String deriving (Eq,Ord)

instance Exception CheckerException
instance Show CheckerException where show (CheckerException i s) = renderInfo i ++ ": " ++ s

die :: MonadThrow m => Info -> String -> m a
die i s = throwM $ CheckerException i s

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn


data VarRole = ArgVar Int | RetVar deriving (Eq,Show,Ord)

data VarType = Spec { _vtType :: Type } |
               Overload { _vtRole :: VarRole, _vtOverApp :: TcId }
               deriving (Eq,Ord)

type TypeSet = S.Set VarType
newtype TypeSetId = TypeSetId String
  deriving (Eq,Ord,IsString,AsString)
instance Show TypeSetId where show (TypeSetId i) = show i


instance Show VarType where
  show (Spec t) = show t
  show (Overload r ts) =
    show ts ++ "?" ++ (case r of ArgVar i -> show i; RetVar -> "r")

instance Pretty VarType where pretty = string . show

data Pivot a = Pivot {
  _pRevMap :: M.Map a (Either TypeSet TypeSetId),
  _pVarMap :: M.Map VarType TypeSetId,
  _pSetMap :: M.Map TypeSetId TypeSet
  } deriving (Eq,Show)
instance Default (Pivot a) where def = Pivot def def def
instance Pretty (Pivot a) where
  pretty Pivot {..} =
    string "Pivot:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pretty k  <> colon <+> sshow v) $ M.toList _pVarMap) <$$>
    string "Sets:" <$$>
    indent 2 (vsep $ map (\(k,v) -> sshow k  <+> colon <$$>
                         indent 4 (hsep (map (string . show) (toList v)))) $ M.toList _pSetMap)


data TcState = TcState {
  _tcSupply :: Int,
  _tcVars :: M.Map TcId TypeSet,
  _tcOverloads :: M.Map TcId FunTypes,
  _tcPivot :: Pivot TcId,
  _tcFailures :: S.Set CheckerException
  } deriving (Eq,Show)

infixr 5 <$$>
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = (PP.<$>)

sshow :: Show a => a -> Doc
sshow = text . show

for :: [a] -> (a -> b) -> [b]
for = flip map

instance Default TcState where def = TcState 0 def def def def
instance Pretty TcState where
  pretty TcState {..} = string "Vars:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <+> colon <+> string (show v)) $ M.toList $ M.map S.toList _tcVars) <$$>
    string "Overloads:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <> string "?" <+> colon <+>
                           align (vsep (map (string . show) (toList v)))) $ M.toList _tcOverloads) <$$>
    pretty _tcPivot <$$>
    string "Failures:" <$$> indent 2 (vsep $ map (string.show) (toList _tcFailures))
    <> hardline


newtype TC a = TC { unTC :: StateT TcState IO a }
  deriving (Functor,Applicative,Monad,MonadState TcState,MonadIO,MonadThrow,MonadCatch)


data TcId = TcId {
  _tiInfo :: Info,
  _tiName :: String,
  _tiId :: Int
  }

instance Eq TcId where
  a == b = _tiId a == _tiId b && _tiName a == _tiName b
instance Ord TcId where
  a <= b = _tiId a < _tiId b || (_tiId a == _tiId b && _tiName a <= _tiName b)
-- show instance is important, used as variable name
instance Show TcId where show TcId {..} = _tiName ++ show _tiId
instance Pretty TcId where pretty = string . show

makeLenses ''TcState
makeLenses ''VarType

makeLenses ''Pivot



freshId :: Info -> String -> TC TcId
freshId i n = TcId i n <$> state (_tcSupply &&& over tcSupply succ)

data LitValue =
  LVLit Literal |
  LVKeySet PactKeySet |
  LVValue Value
  deriving (Eq,Show)
instance Pretty LitValue where
  pretty (LVLit l) = text (show l)
  pretty (LVKeySet k) = text (show k)
  pretty (LVValue v) = text (show v)



data Fun t =
  FNative {
    _fInfo :: Info,
    _fName :: String,
    _fTypes :: FunTypes,
    _fSpecial :: Maybe (SpecialForm,[AST t])
    } |
  FDefun {
    _fInfo :: Info,
    _fName :: String,
    _fType :: FunType,
    _fArgs :: [t],
    _fBody :: [AST t] }
  deriving (Eq,Functor,Foldable,Show)

instance Pretty t => Pretty (Fun t) where
  pretty FNative {..} = text (show _fName) <$$>
    indent 4 (vsep (map (text.show) (toList _fTypes))) <>
      (case _fSpecial of
         Nothing -> mempty
         Just (_,bod) -> mempty <$$> indent 2 (vsep (map pretty bod)))
  pretty FDefun {..} = text (show _fName) <$$>
    indent 4 (text (show _fType)) <$$>
    indent 4 (text "Args:") <+> hsep (map pretty _fArgs) <$$>
    indent 2 (vsep (map pretty _fBody))



data AST t =
  App {
  _aId :: TcId,
  _aAppFun :: Fun t,
  _aAppArgs :: [AST t]
  } |
  Binding {
  _aId :: TcId,
  _aBindings :: [(t,AST t)],
  _aBody :: [AST t],
  _aBindCtx :: BindCtx
  } |
  List {
  _aId :: TcId,
  _aList :: [AST t],
  _aListType :: Maybe Type
  } |
  Object {
  _aId :: TcId,
  _aObject :: [(AST t,AST t)],
  _aUserType :: Maybe TypeName
  } |
  Lit {
  _aId :: TcId,
  _aLitType :: Type,
  _aLitValue :: LitValue
  } |
  Var {
  _aId :: TcId,
  _aVar :: t }
  deriving (Eq,Functor,Foldable,Show)

instance Pretty t => Pretty (AST t) where
  pretty Lit {..} = pretty _aLitType <> equals <> pretty _aLitValue
  pretty Var {..} = pretty _aVar
  pretty Object {..} =
    "{" <$$>
    indent 2 (vsep (map (\(k,v) -> pretty k <> text ":" <$$> indent 4 (pretty v)) _aObject)) <$$> "}"
  pretty List {..} = list (map pretty _aList)
  pretty Binding {..} =
    pretty _aId <$$>
    indent 2 (vsep (map (\(k,v) -> pretty k <> text ":=" <$$> indent 4 (pretty v)) _aBindings)) <$$>
    indent 4 (vsep (map pretty _aBody))
  pretty App {..} =
    pretty _aId <$$>
    pretty _aAppFun <$$>
    indent 2 (vsep (map pretty _aAppArgs))



makeLenses ''AST
makeLenses ''Fun

runTC :: TC a -> IO (a, TcState)
runTC a = runStateT (unTC a) def

data Visit = Pre | Post deriving (Eq,Show)
type Visitor m n = Visit -> AST n -> m (AST n)

-- | Walk the AST, performing function both before and after descent into child elements.
walkAST :: Monad m => Visitor m n -> AST n -> m (AST n)
walkAST f t@Lit {} = f Pre t >>= f Post
walkAST f t@Var {} = f Pre t >>= f Post
walkAST f t@Object {} = do
  Object {..} <- f Pre t
  t' <- Object _aId <$>
         forM _aObject (\(k,v) -> (,) <$> walkAST f k <*> walkAST f v) <*>
         pure _aUserType
  f Post t'
walkAST f t@List {} = do
  List {..} <- f Pre t
  t' <- List _aId <$> mapM (walkAST f) _aList <*> pure _aListType
  f Post t'
walkAST f t@Binding {} = do
  Binding {..} <- f Pre t
  t' <- Binding _aId <$>
        forM _aBindings (\(k,v) -> (k,) <$> walkAST f v) <*>
        mapM (walkAST f) _aBody <*> pure _aBindCtx
  f Post t'
walkAST f t@App {} = do
  App {..} <- f Pre t
  t' <- App _aId <$>
        (case _aAppFun of
           fun@FNative {..} -> case _fSpecial of
             Nothing -> return fun
             Just (fs,bod) -> do
               bod' <- mapM (walkAST f) bod
               return (set fSpecial (Just (fs,bod')) fun)
           fun@FDefun {..} -> do
             db <- mapM (walkAST f) _fBody
             return $ set fBody db fun
        ) <*>
        mapM (walkAST f) _aAppArgs
  f Post t'

isOverload :: VarType -> Bool
isOverload Overload {} = True
isOverload _ = False

isConcrete :: VarType -> Bool
isConcrete (Spec ty) = case ty of
  TyVar {} -> False
  TyRest -> False
  TyFun {} -> False
  _ -> True
isConcrete _ = False

isRestOrUnconstrained :: VarType -> Bool
isRestOrUnconstrained (Spec ty) = case ty of
  TyVar _ [] -> True
  TyRest -> True
  _ -> False
isRestOrUnconstrained _ = False

isTyVar :: VarType -> Bool
isTyVar (Spec TyVar {}) = True
isTyVar _ = False

pivot :: TC ()
pivot = do
  m <- use tcVars
  tcPivot .= pivot' m

pivot' :: Ord a => M.Map a TypeSet -> Pivot a
pivot' m = mkPivot m $ rpt initPivot
  where
    initPivot = execState (rinse m) M.empty
    lather = execState (get >>= rinse)
    rinse :: M.Map a TypeSet -> State (M.Map VarType TypeSet) ()
    rinse p =
      forM_ (M.elems p) $ \vts ->
        forM_ vts $ \vt ->
          when (isTyVar vt || isOverload vt) $ modify $ M.insertWith S.union vt vts
    rpt p = let p' = lather p in if p' == p then p else rpt p'

mkPivot :: forall a . Ord a => M.Map a TypeSet -> M.Map VarType TypeSet -> Pivot a
mkPivot org m =
  let lkps = concatMap mk $ nub $ M.elems m
      mk v = let tid = TypeSetId (show v)
             in [((tid,v),(v,tid),map (,tid) (toList v))]
      tsMap = M.fromList $ map (view _1) lkps
      revLkp = M.fromList $ map (view _2) lkps
      aLkp = M.fromList $ concatMap (view _3) lkps
      vMap = M.map (revLkp M.!) m
      lkpVars :: TypeSet -> Maybe TypeSetId
      lkpVars s = case catMaybes $ S.toList $ S.map (`M.lookup` aLkp) s of
        [] -> Nothing
        (tid:_) -> Just tid
      revMap :: M.Map a (Either TypeSet TypeSetId)
      revMap = M.map (\s -> maybe (Left s) Right $ lkpVars s) org
  in Pivot revMap vMap tsMap

unPivot :: MonadThrow m => Pivot a -> m (M.Map a TypeSet)
unPivot Pivot {..} = forM _pRevMap $ \v -> case v of
  Left s -> return s
  Right tid -> case M.lookup tid _pSetMap of
    Nothing -> die def $ "Bad pivot, '" ++ show tid ++ "' not found: " ++ show _pSetMap
    Just s -> return s

failEx :: a -> TC a -> TC a
failEx a = handle (\(e :: CheckerException) -> do
                      tcFailures %= S.insert e
                      debug $ "Failure: " ++ show e
                      return a)

modifying' :: MonadState s m => Lens' s a -> (a -> m a) -> m ()
modifying' l f = use l >>= f >>= assign l

-- | Eliminate/reduce sets and accumulate errors.
eliminate :: TC ()
eliminate = modifying' (tcPivot . pSetMap) $ mapM (typecheckSet def)

-- | Typecheck and monadically accumulate errors
typecheckSet :: Info -> TypeSet -> TC TypeSet
typecheckSet inf vset = failEx vset $ typecheckSet' inf vset

-- | Eliminate/reduce sets and throw errors.
eliminate' :: MonadThrow m => Pivot a -> m (Pivot a)
eliminate' p = do
  tsets' <- forM (_pSetMap p) $ typecheckSet' def
  return $ set pSetMap tsets' p

_testElim :: MonadThrow m => m (Pivot a)
_testElim = eliminate' (Pivot def def (M.fromList [("foo",S.fromList [Spec $ TyVar "a" [TyInteger,TyDecimal],Spec TyDecimal])]))

_testElim2 :: MonadThrow m => m (Pivot a)
_testElim2 = eliminate' (Pivot def def (M.fromList [("foo",S.fromList [Spec $ TyVar "a" [TyInteger,TyDecimal],Spec TyString])]))



-- | Typecheck and throw errors.
typecheckSet' :: MonadThrow m => Info -> TypeSet -> m TypeSet
typecheckSet' inf vset = do
  let (_rocs,v1) = S.partition isRestOrUnconstrained vset
      (concs,v2) = S.partition isConcrete v1
  conc <- case toList concs of
    [] -> return Nothing
    [c] -> return $ Just c
    _cs -> die inf $ "Multiple concrete types in set:" ++ show vset
  let (tvs,rest) = S.partition isTyVar v2
      constraintsHaveType c t = case t of
        (Spec (TyVar _ es)) -> c `elem` es
        _ -> False
  case conc of
    Just c@(Spec concTy) -> do
      unless (all (constraintsHaveType concTy) tvs) $
        die inf $ "Constraints incompatible with concrete type: " ++ show vset
      return $ S.insert c rest -- constraints good with concrete, so we can get rid of them
    Just c -> die inf $ "Internal error, expected concrete type: " ++ show c
    Nothing ->
      if S.null tvs then return rest -- no conc, no tvs, just leftovers
      else do
        let inter = S.toList $ foldr1 S.intersection $ map S.fromList $ mapMaybe (firstOf (vtType.tvConstraint)) (toList tvs)
        case inter of
          [] -> die inf $ "Incommensurate constraints in set: " ++ show vset
          _ -> do
            let uname = foldr1 (\a b -> a ++ "_U_" ++ b) $ mapMaybe (firstOf (vtType.tvId)) (toList tvs)
            return $ S.insert (Spec (TyVar uname inter)) rest


data SolverEdge = SolverEdge {
  _seTypeSet :: TypeSetId,
  _seVarRole :: VarRole,
  _seOverload :: TcId
  } deriving (Eq,Show,Ord)

data SolverState = SolverState {
  _funMap :: M.Map TcId (FunTypes,M.Map VarRole Type,Maybe FunType),
  _tsetMap :: M.Map TypeSetId (TypeSet,Maybe Type)
} deriving (Eq,Show)
makeLenses ''SolverState
instance Pretty SolverState where
  pretty SolverState {..} =
    text "Funs:" <$$>
    indent 2 (vsep $ for (M.toList _funMap) $ \(k,(fts,mems,mft)) ->
                 text (show k) <> colon <+> align (text (show mems) <$$>
                                                   text (show mft) <$$>
                                                   indent 2 (vsep (map (text . show) $ toList fts))))
    <$$>
    text "Typesets:" <$$>
    indent 2 (vsep $ for (M.toList _tsetMap) $ \(k,(_,mt)) ->
                 text (show k) <> colon <+> text (show mt))



data SolverEnv = SolverEnv {
  _graph :: M.Map (Either TypeSetId TcId) [SolverEdge]
  }
makeLenses ''SolverEnv

type Solver = StateT SolverState (ReaderT SolverEnv IO)

runSolver :: SolverState -> SolverEnv -> Solver a -> IO (a, SolverState)
runSolver s e a = runReaderT (runStateT a s) e

solveOverloads :: TC ()
solveOverloads = do
  tss <- M.toList <$> use (tcPivot . pSetMap)
  (oMap :: M.Map TcId (FunTypes,M.Map VarRole Type,Maybe FunType)) <-
    M.map (,def,Nothing) <$> use tcOverloads
  (stuff :: [((TypeSetId,(TypeSet,Maybe Type)),[SolverEdge])]) <-
    fmap catMaybes $ forM tss $ \(tid,ts) -> do
    let es = (`map` toList ts) $ \v -> case v of
               Overload r i -> Right (SolverEdge tid r i)
               Spec t | isConcrete v -> Left (Just t)
               _ -> Left Nothing
    concrete <- case (`mapMaybe` es) (either id (const Nothing)) of
      [] -> return Nothing
      [a] -> return (Just a)
      _ -> die def $ "Cannot solve: more than one concrete type in set: " ++ show ts
    let ses = mapMaybe (either (const Nothing) Just) es
    if null ses then return Nothing else
      return $ Just ((tid,(ts,concrete)),mapMaybe (either (const Nothing) Just) es)
  let tsMap :: M.Map TypeSetId (TypeSet,Maybe Type)
      tsMap = M.fromList $ map fst stuff
      initState = SolverState oMap tsMap
      concretes :: [TypeSetId]
      concretes = (`mapMaybe` stuff) $ \((tid,(_,t)),_) -> tid <$ t
      edges :: [SolverEdge]
      edges = concatMap snd stuff
      edgeMap :: M.Map (Either TypeSetId TcId) [SolverEdge]
      edgeMap = M.fromListWith (++) $ (`concatMap` edges) $ \s@(SolverEdge t _ i) -> [(Left t,[s]),(Right i,[s])]

  (_,ss@SolverState {..}) <- liftIO $ runSolver initState (SolverEnv edgeMap) (solve concretes)
  liftIO $ putDoc $ pretty ss <$$> hardline
  let resolved = concatMap (\(tid,(_,ty)) -> maybe [] ((:[]) . (tid,)) ty) $ M.toList _tsetMap
  forM_ resolved $ \(tid,ty) -> tcPivot . pSetMap %= M.insert tid (S.singleton (Spec ty))

solve :: [TypeSetId] -> Solver ()
solve initTypes = run initTypes (0 :: Int) where
  run ts i = do
    ovs <- doTypesets ts
    unless (null ovs) $ do
      ts' <- doFuns ovs
      unless (null ts') $ run ts' (succ i)

doFuns :: [TcId] -> Solver [TypeSetId]
doFuns ovs = fmap (nub . concat) $ forM ovs $ \ov -> do
  fr <- M.lookup ov <$> use funMap
  er <- M.lookup (Right ov) <$> view graph
  case (fr,er) of
    (Just (fTys,mems,Nothing),Just es) -> do
      ftym <- foldM (\r f -> maybe (fmap (f,) <$> tryFunType f mems) (return . Just) r) Nothing fTys
      case ftym of
        Nothing -> return []
        Just (fty,mems') -> do
          funMap %= M.adjust (set _3 (Just fty) . set _2 mems') ov
          let newmems = M.difference mems' mems
              findEdge vr = filter ((== vr) . _seVarRole) es
          fmap concat $ forM (M.toList newmems) $ \(vr,ty) -> case findEdge vr of
            [SolverEdge ts _ _] -> do
              tsetMap %= M.adjust (set _2 (Just ty)) ts
              return [ts]
            _ -> return [] -- need more error reporting
    _ -> return []

doTypesets :: [TypeSetId] -> Solver [TcId]
doTypesets cs = fmap (nub . concat) $ forM cs $ \c -> do
  cr <- M.lookup c <$> use tsetMap
  er <- M.lookup (Left c) <$> view graph
  case (cr,er) of
    (Just (_, Just ct),Just es) -> fmap concat $ forM es $ \(SolverEdge _ r ov) -> do
      fm <- M.lookup ov <$> use funMap
      case fm of
        Nothing -> return []
        Just (_,_,Just _) -> return []
        _ -> do
          funMap %= M.adjust (over _2 (M.insert r ct)) ov
          return [ov]
    (_,_) -> return []

tryFunType :: (MonadIO m,MonadCatch m) => FunType -> M.Map VarRole Type -> m (Maybe (M.Map VarRole Type))
tryFunType (FunType as rt) vMap = do
  let ars = zipWith (\(Arg _ t _) i -> (ArgVar i,t)) as [0..]
      fMap = M.fromList $ (RetVar,rt):ars
      toSets = M.map (S.singleton . Spec)
      setsMap = M.unionWith S.union (toSets vMap) (toSets fMap)
      piv = pivot' setsMap
  handle (\(_ :: CheckerException) -> return Nothing) $ do
    elim <- eliminate' piv
    remapped <- unPivot elim
    let justConcs = (`map` M.toList remapped) $ \(vr,s) -> ((vr,) <$> asConcreteSingleton s)
    case sequence justConcs of
      Nothing -> return Nothing
      Just ps -> return (Just (M.fromList ps))

asConcrete :: VarType -> Maybe Type
asConcrete s | isConcrete s = Just $ _vtType s
asConcrete _ = Nothing

asConcreteSingleton :: TypeSet -> Maybe Type
asConcreteSingleton s | S.size s == 1 = asConcrete (head (toList s))
asConcreteSingleton _ = Nothing

_ftaaa :: FunType
_ftaaa = let a = TyVar "a" [TyInteger,TyDecimal]
         in FunType [Arg "x" a def,Arg "y" a def] a
_ftabD :: FunType
_ftabD = let v n = TyVar n [TyInteger,TyDecimal]
         in FunType [Arg "x" (v "a") def,Arg "y" (v "b") def] TyDecimal

{-

λ> tryFunType _ftaaa (M.fromList [(ArgVar 0,TyDecimal),(ArgVar 1,TyInteger)])
Nothing
λ> tryFunType _ftaaa (M.fromList [(ArgVar 0,TyDecimal),(ArgVar 1,TyDecimal)])
Just (fromList [(ArgVar 0,decimal),(ArgVar 1,decimal),(RetVar,decimal)])
λ> tryFunType _ftabD (M.fromList [(ArgVar 0,TyDecimal),(ArgVar 1,TyInteger)])
Just (fromList [(ArgVar 0,decimal),(ArgVar 1,integer),(RetVar,decimal)])
λ> tryFunType _ftabD (M.fromList [(ArgVar 0,TyDecimal)])
Nothing
λ>

-}


-- | Native funs get processed on their own walk.
-- 'assocAST' associates the app arg's ID with the fun ty.
processNatives :: Visitor TC TcId
processNatives Pre a@(App i FNative {..} as) = do
  case _fTypes of
    -- single funtype
    ft@FunType {} :| [] -> do
      let FunType {..} = mangleFunType i ft
      zipWithM_ (\(Arg _ t _) aa -> assocTy (_aId aa) (Spec t)) _ftArgs as
      assocTy i (Spec _ftReturn)
      -- the following assumes that special forms are never overloaded!
      case _fSpecial of
        -- with-read et al have a single Binding body, associate this with return type
        Just (_,[Binding {..}]) -> assocTy _aId (Spec _ftReturn)
        -- WithKeyset is a body form
        Just (WithKeyset,bod) -> notEmpty (_tiInfo i) "Expected non-empty body" bod >>= (assocAST i . last)
        _ -> return ()
    -- multiple funtypes
    fts -> do
      let fts' = fmap (mangleFunType i) fts
      tcOverloads %= M.insert i fts'
      zipWithM_ (\ai aa -> assocTy (_aId aa) (Overload (ArgVar ai) i)) [0..] as -- this assoc's the funty with the app ty.
      assocTy i (Overload RetVar i)
  return a
processNatives _ a = return a

-- | Walk to substitute app args into vars for FDefuns
-- 'assocAST' associates the defun's arg with the app arg type.
substAppDefun :: Maybe (TcId, AST TcId) -> Visitor TC TcId
substAppDefun sub Pre t@Var {..} = case sub of
    Nothing -> return t
    Just (defArg,appAst)
      | defArg == _aVar -> assocAST defArg appAst >> return appAst
      | otherwise -> return t
substAppDefun _ Post App {..} = do -- Post, to allow args to get substituted out first.
    af <- case _aAppFun of
      f@FNative {} -> return f
      f@FDefun {..} -> do
        fb' <- forM _fBody $ \bAst ->
          foldM (\b fa -> walkAST (substAppDefun (Just fa)) b) bAst (zip _fArgs _aAppArgs) -- this zip might need a typecheck
        return $ set fBody fb' f
    return (App _aId af _aAppArgs)
substAppDefun _ _ t = return t

lookupIdTys :: TcId -> TC TypeSet
lookupIdTys i = (fromMaybe S.empty . M.lookup i) <$> use tcVars



tcToTy :: AST TcId -> TC TypeSet
tcToTy Lit {..} = return $ S.singleton $ Spec _aLitType
tcToTy Var {..} = lookupIdTys _aVar
tcToTy Object {..} = return $ S.singleton $ Spec $ TyObject _aUserType
tcToTy List {..} = return $ S.singleton $ Spec $ TyList _aListType
tcToTy App {..} = lookupIdTys _aId
tcToTy Binding {..} = lookupIdTys _aId

-- | Track type to id
trackVar :: TcId -> VarType -> TC ()
trackVar i t = do
  old <- M.lookup i <$> use tcVars
  case old of
    Nothing -> return ()
    Just tys -> die (_tiInfo i) $ "Internal error: type already tracked: " ++ show (i,t,tys)
  tcVars %= M.insert i (S.singleton t)

-- | Track type to id with typechecking
assocTy :: TcId -> VarType -> TC ()
assocTy i ty = assocTys i (S.singleton ty)

-- | Track ast type to id with typechecking
assocAST :: TcId -> AST TcId -> TC ()
assocAST i a = tcToTy a >>= assocTys i

-- | Track types to id with typechecking
-- TODO figure out better error messages. The type being added
-- is at least in App case the specified type to the arg type,
-- meaning the already-tracked type is the unexpected one.
assocTys :: TcId -> TypeSet -> TC ()
assocTys i tys = do
  tys' <- S.union tys <$> lookupIdTys i
  void $ typecheckSet (_tiInfo i) tys'
  tcVars %= M.insert i tys'

scopeToBody :: Info -> [AST TcId] -> Scope Int Term (Either Ref (AST TcId)) -> TC [AST TcId]
scopeToBody i args bod = do
  bt <- instantiate (return . Right) <$> traverseScope (bindArgs i args) return bod
  case bt of
    (TList ts@(_:_) _ _) -> mapM toAST ts -- verifies non-empty body.
    _ -> die i "Malformed def body"

pfx :: String -> String -> String
pfx s = ((s ++ "_") ++)

idTyVar :: TcId -> Type
idTyVar i = TyVar (show i) []

mangleType :: TcId -> Type -> Type
mangleType f t@TyVar {} = over tvId (pfx (show f)) t
mangleType f t@TyList {} = over (tlType . _Just) (mangleType f) t
mangleType f t@TyFun {} = over tfType (mangleFunType f) t
mangleType _ t = t

mangleFunType :: TcId -> FunType -> FunType
mangleFunType f = over ftReturn (mangleType f) .
                  over (ftArgs.traverse.aType) (mangleType f)

toFun :: Term (Either Ref (AST TcId)) -> TC (Fun TcId)
toFun (TVar (Left (Direct (TNative DefData {..} _ i))) _) =
  return $ FNative i _dName _dType ((,[]) <$> isSpecialForm _dName)
toFun (TVar (Left (Ref r)) _) = toFun (fmap Left r)
toFun (TVar Right {} i) = die i "Value in fun position"
toFun (TDef DefData {..} bod _ i) = do -- TODO currently creating new vars every time, is this ideal?
  ty <- case _dType of
    t :| [] -> return t
    _ -> die i "Multiple def types not allowed"
  let fn = maybe _dName ((++ ('.':_dName)) . asString) _dModule
  args <- forM (_ftArgs ty) $ \(Arg n t ai) -> do
    an <- freshId ai $ pfx fn n
    let t' = mangleType an t
    trackVar an (Spec t')
    return an
  tcs <- scopeToBody i (map (\ai -> Var ai ai) args) bod
  return $ FDefun i fn ty args tcs
toFun t = die (_tInfo t) "Non-var in fun position"

notEmpty :: MonadThrow m => Info -> String -> [a] -> m [a]
notEmpty i msg [] = die i msg
notEmpty _ _ as = return as

toAST :: Term (Either Ref (AST TcId)) -> TC (AST TcId)
toAST TNative {..} = die _tInfo "Native in value position"
toAST TDef {..} = die _tInfo "Def in value position"
toAST (TVar v i) = case v of -- value position only, TApp has its own resolver
  (Left (Ref r)) -> toAST (fmap Left r)
  (Left Direct {}) -> die i "Native in value context"
  (Right t) -> return t
toAST TApp {..} = do
  fun <- toFun _tAppFun
  i <- freshId _tInfo $
       "app" ++ (case fun of FDefun {} -> "D"; _ -> "N") ++  _fName fun
  trackVar i (Spec $ idTyVar i)
  as <- mapM toAST _tAppArgs
  (as',fun') <- case fun of
    FDefun {..} -> assocAST i (last _fBody) >> return (as,fun) -- non-empty verified in 'scopeToBody'
    FNative {..} -> case _fSpecial of
      Nothing -> return (as,fun)
      Just (f@WithKeyset,_) -> return (take 1 as,set fSpecial (Just (f,drop 1 as)) fun)
      Just (f,_) -> (,) <$> notEmpty _tInfo "Expected >1 arg" (init as)
                    <*> pure (set fSpecial (Just (f,[last as])) fun)
  return $ App i fun' as'

toAST TBinding {..} = do
  bi <- freshId _tInfo (case _tBindCtx of BindLet -> "let"; BindKV -> "bind")
  trackVar bi $ Spec $ idTyVar bi
  bs <- forM _tBindPairs $ \(Arg n t ai,v) -> do
    an <- freshId ai (pfx (show bi) n)
    let t' = mangleType an t
    trackVar an $ Spec t'
    case _tBindCtx of
      BindLet -> do
        v' <- toAST v
        assocAST an v'
        return (an,v')
      BindKV -> return (an,Var an an) -- KV bind punts and simply creates a var
  bb <- scopeToBody _tInfo (map ((\ai -> Var ai ai).fst) bs) _tBindBody
  case _tBindCtx of
    BindLet -> assocAST bi (last bb)
    BindKV -> return () -- TODO check it out
  return $ Binding bi bs bb _tBindCtx

toAST TList {..} = List <$> freshId _tInfo "list" <*> mapM toAST _tList <*> pure _tListType
toAST TObject {..} = Object <$> freshId _tInfo "object" <*>
                       mapM (\(k,v) -> (,) <$> toAST k <*> toAST v) _tObject <*> pure _tUserType
toAST TConst {..} = toAST _tConstVal -- TODO typecheck here
toAST TKeySet {..} = freshId _tInfo "keyset" >>= \i -> return $ Lit i TyKeySet (LVKeySet _tKeySet)
toAST TValue {..} = freshId _tInfo "value" >>= \i -> return $ Lit i TyValue (LVValue _tValue)
toAST TLiteral {..} = do
  let ty = l2ty _tLiteral
  i <- freshId _tInfo (show ty)
  trackVar i (Spec ty)
  return $ Lit i ty (LVLit _tLiteral)
toAST TModule {..} = die _tInfo "Modules not supported"
toAST TUse {..} = die _tInfo "Use not supported"
toAST TStep {..} = die _tInfo "TODO steps/pacts"

l2ty :: Literal -> Type
l2ty LInteger {} = TyInteger
l2ty LDecimal {} = TyDecimal
l2ty LString {} = TyString
l2ty LBool {} = TyBool
l2ty LTime {} = TyTime

bindArgs :: Info -> [a] -> Int -> TC a
bindArgs i args b =
  case args `atMay` b of
    Nothing -> die i $ "Missing arg: " ++ show b ++ ", " ++ show (length args) ++ " provided"
    Just a -> return a


infer :: Term Ref -> TC (Fun TcId)
infer t@TDef {..} = toFun (fmap Left t)
infer t = die (_tInfo t) "Non-def"


allVarsCheck :: TC (Either (M.Map TcId TypeSet) (M.Map TcId Type))
allVarsCheck = do
  vs <- use tcVars
  let vs' = (`map` M.toList vs) $ \(tid,s) -> case asConcreteSingleton s of
        Nothing -> Left (tid,s)
        Just ty -> Right (tid,ty)
  case sequence vs' of
    Left _ -> return (Left $ M.fromList $ concatMap (either pure (const [])) vs')
    Right ps -> return (Right (M.fromList ps))


substFun :: Fun TcId -> TC (Either (String,Fun TcId) (Fun (TcId,Type)))
substFun f@FNative {} = return $ Right $ fmap (const (TcId def "" 0,TyRest)) f
substFun f@FDefun {..} = do
  debug "Substitution"
  b' <- mapM (walkAST processNatives) =<< mapM (walkAST $ substAppDefun Nothing) _fBody
  debug "Pivot"
  pivot
  debug "Eliminate"
  eliminate
  debug "Solve overloads"
  failEx () solveOverloads
  use tcPivot >>= unPivot >>= assign tcVars
  result <- allVarsCheck
  let f' = set fBody b' f
  -- TODO need to bail on any errors accumulated up to here
  case result of
    Left _ -> return $ Left ("Failed to unify all types",f')
    Right vs -> return $ Right $ fmap (\v -> (v,vs M.! v)) f'

_loadFun :: FilePath -> ModuleName -> String -> IO (Term Ref)
_loadFun fp mn fn = do
  (r,s) <- execScript' (Script fp) fp
  either (die def) (const (return ())) r
  let (Just (Just (Ref d))) = firstOf (rEnv . eeRefStore . rsModules . at mn . _Just . at fn) s
  return d

_infer :: FilePath -> ModuleName -> String -> IO (Either (String,Fun TcId) (Fun (TcId,Type)), TcState)
_infer fp mn fn = _loadFun fp mn fn >>= \d -> runTC (infer d >>= substFun)

-- _pretty =<< _inferIssue
_inferIssue :: IO (Either (String,Fun TcId) (Fun (TcId,Type)), TcState)
_inferIssue = _infer "examples/cp/cp.repl" "cp" "issue"

-- _pretty =<< _inferTransfer
_inferTransfer :: IO (Either (String,Fun TcId) (Fun (TcId,Type)), TcState)
_inferTransfer = _infer "examples/accounts/accounts.repl" "accounts" "transfer"

-- prettify output of '_infer' runs
_pretty :: (Either (String,Fun TcId) (Fun (TcId,Type)), TcState) -> IO ()
_pretty (Left (m,f),tc) = putDoc (pretty f <> hardline <> hardline <> pretty tc <$$> text m <> hardline)
_pretty (Right f,tc) = putDoc (pretty f <> hardline <> hardline <> pretty tc)

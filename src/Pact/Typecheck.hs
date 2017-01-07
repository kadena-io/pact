{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>),(<>))
import Data.String
import Data.Maybe
import Data.List
import Data.Either
import Data.Monoid
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data CheckerException = CheckerException Info String deriving (Eq,Ord)

instance Exception CheckerException
instance Show CheckerException where show (CheckerException i s) = renderInfo i ++ ": " ++ s

die :: MonadThrow m => Info -> String -> m a
die i s = throwM $ CheckerException i s

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn


sing :: a -> S.Set a
sing = S.singleton

data UserType = UserType {
  _utName :: TypeName,
  _utModule :: ModuleName,
  _utFields :: [Arg UserType],
  _utInfo :: Info
  } deriving (Eq,Ord)
instance Show UserType where
  show UserType {..} = "{" ++ asString _utModule ++ "." ++ asString _utName ++ " " ++ show _utFields ++ "}"


data VarRole = ArgVar Int | RetVar deriving (Eq,Show,Ord)


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

die' :: MonadThrow m => TcId -> String -> m a
die' i = die (_tiInfo i)

data Overloaded = Overloaded { _oRole :: VarRole, _oOverApp :: TcId }
 deriving (Eq,Ord)
instance Show Overloaded where
  show (Overloaded r ts) = show ts ++ "?" ++ (case r of ArgVar i -> show i; RetVar -> "r")

data VarType = Spec { _vtSpec :: Type UserType } |
               Overload { _vtOverload :: Overloaded }
             deriving (Eq,Ord)


instance Show VarType where
  show (Spec t) = show t
  show (Overload o) = show o


instance Pretty VarType where pretty = string . show

walkVarType :: (Monoid s, Monad m) => VarType -> (VarType -> m s) -> m s
walkVarType v f = do
  s <- f v
  s' <- case v of
          Spec (TyList tv) -> walkVarType (Spec tv) f
          Spec (TySchema _ tv) -> walkVarType (Spec tv) f
          Spec (TyFun FunType {..}) -> do
            as <- mapM ((`walkVarType` f) . Spec . _aType) _ftArgs
            r <- walkVarType (Spec _ftReturn) f
            return (mconcat as <> r)
          _ -> return mempty
  return (s <> s')

walkVarType' :: Monoid s => VarType -> (VarType -> s) -> s
walkVarType' v f = runIdentity (walkVarType v (return . f))

freeVarType :: VarType -> S.Set (TypeVar UserType)
freeVarType v = walkVarType' v $ \vt -> case vt of
  Spec (TyVar n) -> sing n
  _ -> mempty

freeType :: Type n -> S.Set (TypeVar n)
freeType (TyVar v) = sing v
freeType (TyList l) = freeType l
freeType (TySchema _ o) = freeType o
freeType (TyFun FunType {..}) = mconcat (map (freeType . _aType) _ftArgs) <> freeType _ftReturn
freeType _ = mempty



makeLenses ''VarType




type TypeSet = S.Set VarType
newtype TypeSetId = TypeSetId String
  deriving (Eq,Ord,IsString,AsString)
instance Show TypeSetId where show (TypeSetId i) = i



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


data Types o = Types {
  _tsPlain :: Type UserType,
  _tsOverloads :: [o]
  } deriving (Eq,Ord)
instance Show o => Show (Types o) where
  show (Types p []) = show p
  show (Types p os) = show p ++ " " ++ show os


data TcState = TcState {
  _tcSupply :: Int,
  _tcVars :: M.Map TcId TypeSet,
  _tcOverloads :: M.Map TcId (FunTypes UserType),
  _tcPivot :: Pivot TcId,
  _tcFailures :: S.Set CheckerException,
  _tcAstToVar :: M.Map TcId (Type UserType),
  _tcVarToTypes :: M.Map (TypeVar UserType) (Types Overloaded)
  } deriving (Eq,Show)

infixr 5 <$$>
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = (PP.<$>)

sshow :: Show a => a -> Doc
sshow = text . show

for :: [a] -> (a -> b) -> [b]
for = flip map

instance Default TcState where def = TcState 0 def def def def def def
instance Pretty TcState where
  pretty TcState {..} = "Vars:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <+> colon <+> string (show v)) $ M.toList $ M.map S.toList _tcVars) <$$>
    "Overloads:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <> string "?" <+> colon <+>
                           align (vsep (map (string . show) (toList v)))) $ M.toList _tcOverloads) <$$>
    pretty _tcPivot <$$>
    "AstToVar:" <$$>
    indent 2 (vsep (map (\(k,v) -> pretty k <> colon <+> sshow v) (M.toList _tcAstToVar))) <$$>
    "VarToTypes:" <$$>
    indent 2 (vsep $ map (\(k,v) -> sshow k <> colon <+> sshow v) $ M.toList $ _tcVarToTypes) <$$>
    "Failures:" <$$>
    indent 2 (vsep $ map (string.show) (toList _tcFailures))
    <> hardline


newtype TC a = TC { unTC :: StateT TcState IO a }
  deriving (Functor,Applicative,Monad,MonadState TcState,MonadIO,MonadThrow,MonadCatch)



makeLenses ''TcState
makeLenses ''Pivot
makeLenses ''Types


freshId :: Info -> String -> TC TcId
freshId i n = TcId i n <$> state (_tcSupply &&& over tcSupply succ)

fresh :: TC VarType
fresh = do
  i <- state (_tcSupply &&& over tcSupply succ)
  return $ Spec $ undefined -- TyVar ("v" ++ show i) []

data PrimValue =
  PrimLit Literal |
  PrimKeySet PactKeySet |
  PrimValue Value
  deriving (Eq,Show)
instance Pretty PrimValue where
  pretty (PrimLit l) = text (show l)
  pretty (PrimKeySet k) = text (show k)
  pretty (PrimValue v) = text (show v)




data Fun t =
  FNative {
    _fInfo :: Info,
    _fName :: String,
    _fTypes :: FunTypes UserType,
    _fSpecial :: Maybe (SpecialForm,[AST t])
    } |
  FDefun {
    _fInfo :: Info,
    _fName :: String,
    _fType :: FunType UserType,
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
  _aListType :: Type UserType
  } |
  Object {
  _aId :: TcId,
  _aObject :: [(AST t,AST t)],
  _aUserType :: Type UserType
  } |
  Prim {
  _aId :: TcId,
  _aPrimType :: PrimType,
  _aPrimValue :: PrimValue
  } |
  Var {
  _aId :: TcId,
  _aVar :: t
  } |
  Table {
  _aId :: TcId,
  _aUserType :: Type UserType
  }

  deriving (Eq,Functor,Foldable,Show)

instance Pretty t => Pretty (AST t) where
  pretty Prim {..} = sshow _aPrimType <> equals <> pretty _aPrimValue
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
  pretty Table {..} = text "table" <> colon <> sshow _aUserType



makeLenses ''AST
makeLenses ''Fun

runTC :: TC a -> IO (a, TcState)
runTC a = runStateT (unTC a) def


data Visit = Pre | Post deriving (Eq,Show)
type Visitor m n = Visit -> AST n -> m (AST n)

-- | Walk the AST, performing function both before and after descent into child elements.
walkAST :: Monad m => Visitor m n -> AST n -> m (AST n)
walkAST f t@Prim {} = f Pre t >>= f Post
walkAST f t@Var {} = f Pre t >>= f Post
walkAST f t@Table {} = f Pre t >>= f Post
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
isConcrete (Spec ty) = not (isAnyTy ty || isVarTy ty)
isConcrete _ = False

isUnconstrained :: VarType -> Bool
isUnconstrained (Spec ty) = isUnconstrainedTy ty
isUnconstrained _ = False

isTyVarType :: VarType -> Bool
isTyVarType (Spec v) = isVarTy v
isTyVarType _ = False

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
        forM_ vts $ \vt' -> walkVarType vt' $ \vt -> do
          let vts' = S.insert vt vts
          when (isTyVarType vt || isOverload vt) $
            modify $ M.insertWith S.union vt vts'
    rpt p = let p' = lather p in if p' == p then p else rpt p'

mkPivot :: forall a . Ord a => M.Map a TypeSet -> M.Map VarType TypeSet -> Pivot a
mkPivot org m =
  let lkps = concatMap mk $ nub $ M.elems m
      mk v = let tid = TypeSetId (show (toList v))
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
_testElim =
  eliminate' (Pivot def def (M.fromList
                             [("foo",S.fromList
                                [Spec $ mkTyVar "a" [TyPrim TyInteger,TyPrim TyDecimal],Spec (TyPrim TyDecimal)])]))

_testElim2 :: MonadThrow m => m (Pivot a)
_testElim2 =
  eliminate' (Pivot def def (M.fromList
                              [("foo",S.fromList
                                 [Spec $ mkTyVar "a" [TyPrim TyInteger,TyPrim TyDecimal],Spec (TyPrim TyString)])]))

{-
set typechecking:

-}


-- | Typecheck and throw errors.
typecheckSet' :: MonadThrow m => Info -> TypeSet -> m TypeSet
typecheckSet' inf vset = do
  let (_rocs,v1) = S.partition isUnconstrained vset
      (concs,v2) = S.partition isConcrete v1
  conc <- case toList concs of
    [] -> return Nothing
    [c] -> return $ Just c
    _cs -> die inf $ "Multiple concrete types in set:" ++ show vset
  let (tvs,rest) = S.partition isTyVarType v2
      constraintsHaveType c t = case t of
        (Spec (TyVar (TypeVar _ es))) -> c `elem` es
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
        let inter = S.toList $ foldr1 S.intersection $ map S.fromList $ mapMaybe (firstOf (vtSpec.tyVar.tvConstraint)) (toList tvs)
        case inter of
          [] -> die inf $ "Incommensurate constraints in set: " ++ show vset
          _ -> do
            let uname = foldr1 (\a b -> (TypeVarName $ show a ++ "_U_" ++ show b)) $ -- TODO fix for real VarName
                  mapMaybe (firstOf (vtSpec.tyVar.tvName)) (toList tvs)
            return $ S.insert (Spec (TyVar (TypeVar uname inter))) rest


data SolverEdge = SolverEdge {
  _seTypeSet :: TypeSetId,
  _seVarRole :: VarRole,
  _seOverload :: TcId
  } deriving (Eq,Show,Ord)

data SolverState = SolverState {
  _funMap :: M.Map TcId (FunTypes UserType,M.Map VarRole (Type UserType),Maybe (FunType UserType)),
  _tsetMap :: M.Map TypeSetId (TypeSet,Maybe (Type UserType))
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
  (oMap :: M.Map TcId (FunTypes UserType,M.Map VarRole (Type UserType),Maybe (FunType UserType))) <-
    M.map (,def,Nothing) <$> use tcOverloads
  (stuff :: [((TypeSetId,(TypeSet,Maybe (Type UserType))),[SolverEdge])]) <-
   fmap catMaybes $ forM tss $ \(tid,ts) -> do
    let es = (`map` toList ts) $ \v -> case v of
               Overload (Overloaded r i) -> Right (SolverEdge tid r i)
               Spec t | isConcrete v -> Left (Just t)
               _ -> Left Nothing
    concrete <- case (`mapMaybe` es) (either id (const Nothing)) of
      [] -> return Nothing
      [a] -> return (Just a)
      _ -> return Nothing {- TODO: die def $ "Cannot solve: more than one concrete type in set: " ++ show ts -}
    let ses = mapMaybe (either (const Nothing) Just) es
    if null ses then return Nothing else
      return $ Just ((tid,(ts,concrete)),mapMaybe (either (const Nothing) Just) es)
  let tsMap :: M.Map TypeSetId (TypeSet,Maybe (Type UserType))
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

tryFunType :: (MonadIO m,MonadCatch m) => FunType UserType -> M.Map VarRole (Type UserType) ->
              m (Maybe (M.Map VarRole (Type UserType)))
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

asConcrete :: VarType -> Maybe (Type UserType)
asConcrete s | isConcrete s = Just $ _vtSpec s
asConcrete _ = Nothing

asConcreteSingleton :: TypeSet -> Maybe (Type UserType)
asConcreteSingleton s | S.size s == 1 = asConcrete (head (toList s))
asConcreteSingleton _ = Nothing

_ftaaa :: FunType UserType
_ftaaa = let a = mkTyVar "a" [TyPrim TyInteger,TyPrim TyDecimal]
         in FunType [Arg "x" a def,Arg "y" a def] a
_ftabD :: FunType UserType
_ftabD = let v n = mkTyVar n [TyPrim TyInteger,TyPrim TyDecimal]
         in FunType [Arg "x" (v "a") def,Arg "y" (v "b") def] (TyPrim TyDecimal)

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
      zipWithM_ (\(Arg _ t _) aa -> assocTy (_aId aa) (Spec t) {- >> assocAST (_aId aa) aa -}) _ftArgs as
      assocTy i (Spec _ftReturn)
      -- the following assumes that special forms are never overloaded!
      case _fSpecial of
        -- with-read et al have a single Binding body, associate this with return type
        Just (_,[Binding {..}]) -> assocTy _aId (Spec _ftReturn)
        _ -> return ()
    -- multiple funtypes
    fts -> do
      let fts' = fmap (mangleFunType i) fts
      tcOverloads %= M.insert i fts'
      zipWithM_ (\ai aa -> assocTy (_aId aa) (Overload (Overloaded (ArgVar ai) i))) [0..] as -- this assoc's the funty with the app ty.
      assocTy i (Overload (Overloaded RetVar i))
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

astToTy :: AST n -> Maybe (Type UserType)
astToTy Prim {..} = Just $ TyPrim _aPrimType
astToTy Var {} = Nothing
astToTy Object {..} = Just $ TySchema TyObject _aUserType
astToTy List {..} = Just $ TyList _aListType
astToTy App {} = Nothing
astToTy Binding {} = Nothing
astToTy Table {..} = Just $ TySchema TyTable _aUserType

tcToTy :: AST TcId -> TC TypeSet
tcToTy Prim {..} = return $ sing $ Spec $ TyPrim _aPrimType
tcToTy Var {..} = lookupIdTys _aVar
tcToTy Object {..} = return $ sing $ Spec $ TySchema TyObject _aUserType
tcToTy List {..} = return $ sing $ Spec $ TyList _aListType
tcToTy App {..} = lookupIdTys _aId
tcToTy Binding {..} = lookupIdTys _aId
tcToTy Table {..} = return $ sing $ Spec $ TySchema TyTable _aUserType

-- | Track type to id
trackAST :: TcId -> Type UserType -> TC ()
trackAST i t = do
  old <- M.lookup i <$> use tcVars
  case old of
    Nothing -> return ()
    Just tys -> die (_tiInfo i) $ "Internal error: type already tracked: " ++ show (i,t,tys)
  tcVars %= M.insert i (sing $ Spec t)
  trackAST' i t

trackAST' :: TcId -> Type UserType -> TC ()
trackAST' i t = do
  debug $ "trackAST: " ++ show (i,t)
  maybe (return ()) (const (die' i $ "trackAST: ast already tracked: " ++ show (i,t)))
    =<< (M.lookup i <$> use tcAstToVar)
  tcAstToVar %= M.insert i t
  case t of
    TyVar v -> do
      maybe (return ()) (const (die' i $ "trackAST: var already tracked: " ++ show (i,t)))
        =<< (M.lookup v <$> use tcVarToTypes)
      tcVarToTypes %= M.insert v (Types t [])
    _ -> return ()

getParamTypes :: VarType -> [Type UserType]
getParamTypes t = walkVarType' t $ \pt -> case pt of
  Spec s | pt /= t && s /= TyAny -> [s]
  _ -> []

addFailure :: TcId -> String -> TC ()
addFailure i s = do
  debug $ "Failure: " ++ show (i,s)
  tcFailures %= S.insert (CheckerException (_tiInfo i) s)

lookupAst :: String -> TcId -> TC (Type UserType)
lookupAst msg i = maybe (die' i $ msg ++ ": ast not already tracked: " ++ show i) return =<<
                  (M.lookup i <$> use tcAstToVar)

-- | Track type to id with typechecking
assocTy :: TcId -> VarType -> TC ()
assocTy ai vt = do
  assocTys ai (sing vt) -- old way
  aty <- lookupAst "assocTy" ai
  (avm,atysm) <- case aty of
    TyVar tv -> (Just tv,) . M.lookup tv <$> use tcVarToTypes
    _ -> return (Nothing,Nothing)
  debug $ "assocTy: " ++ show (ai,aty,vt)
  case vt of
    Overload o -> case (avm,atysm) of
      (Just v,Just tys) -> do
        debug ("assocTy: associating " ++ show o ++ " with " ++ show (ai,v,tys))
        tcVarToTypes %= M.insert v (over tsOverloads (o:) tys)
      _ -> die' ai $ "assocTy: cannot track overload, not a var or not tracked: " ++ show (ai,aty,atysm,vt)
    Spec ty -> case unifyTypes aty ty of
      Nothing -> addFailure ai $ "assocTy: cannot unify: " ++ show (ai,aty,ty)
      Just (Left _same) -> do
        debug ("assocTy: noop: " ++ show (ai,aty,ty))
        assocParams aty ty
      (Just (Right u)) -> do
        debug $ "assocTy: substituting " ++ show u ++ " for " ++ show (ai,ty)
        assocParams aty ty
        case (avm,atysm) of
          (Nothing,Nothing) -> do
            tcAstToVar %= M.insert ai u
            updateTyVar u u
          (Just v,Just tys) ->
            tcVarToTypes %= M.insert v (set tsPlain u tys)
          _ -> die' ai $ "assocTy: var not tracked: " ++ show (ai,avm,atysm)

updateTyVar :: Type UserType -> Type UserType -> TC ()
updateTyVar (TyVar uv) u = do
  debug $ "updateTyVar: " ++ show (uv,u)
  tcVarToTypes %= M.alter (maybe (Just (Types u [])) (Just . set tsPlain u)) uv
updateTyVar _ _ = return ()

assocParams :: Type UserType -> Type UserType -> TC ()
assocParams x y = case (x,y) of
  _ | x == y -> return ()
  (TySchema _ a,TySchema _ b) -> assoc a b
  (TyList a,TyList b) -> assoc a b
  _ -> return ()
  where
    assoc a@(TyVar {}) b = updateTyVar a b
    assoc a b@(TyVar {}) = updateTyVar b a
    assoc _ _ = return ()

-- | Track ast type to id with typechecking
assocAST :: TcId -> AST TcId -> TC ()
assocAST ai b = do
  tcToTy b >>= assocTys ai -- old way
  let bi = _aId b
  aty <- lookupAst "assocAST" ai
  bty <- lookupAst "assocAST" bi
  let doSub si sty fi fty = do
        debug $ "assocAST: substituting " ++ show (si,sty) ++ " for " ++ show (fi,fty)
        tcAstToVar %= M.insert fi sty
        case fty of
          TyVar fv -> tcVarToTypes %= M.delete fv
          _ -> return ()
  case unifyTypes aty bty of
    Nothing -> addFailure bi $ "assocAST: cannot unify: " ++ show (aty,bty)
    Just (Left _) -> doSub ai aty bi bty
    Just (Right _) -> doSub bi bty ai aty



unifyVarTypes :: VarType -> VarType -> Maybe (Either VarType VarType)
unifyVarTypes l r = case (l,r) of
  _ | l == r -> Just (Right r)
  (Spec a,Spec b) -> fmap (either (Left . Spec) (Right . Spec)) (unifyTypes a b)
  _ -> Nothing

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

idTyVar :: TcId -> Type n
idTyVar i = mkTyVar (show i) []

mangle :: TcId -> Type n -> Type n
mangle i = over (tyVar.tvName.typeVarName) (pfx (show i))

mangleType :: TcId -> Type UserType -> Type UserType
mangleType f t@TyVar {} = mangle f t
mangleType f t@TyList {} = over ttListType (mangleType f) t
mangleType f t@TySchema {} = over ttSchemaType (mangle f) t
mangleType f t@TyFun {} = over ttFunType (mangleFunType f) t
mangleType _ t = t

mangleFunType :: TcId -> FunType UserType -> FunType UserType
mangleFunType f = over ftReturn (mangleType f) .
                  over (ftArgs.traverse.aType) (mangleType f)

toFun :: Term (Either Ref (AST TcId)) -> TC (Fun TcId)
toFun (TVar (Left (Direct TNative {..})) _) = do
  ft' <- traverse (traverse toUserType') _tFunTypes
  return $ FNative _tInfo (asString _tNativeName) ft' ((,[]) <$> isSpecialForm _tNativeName)
toFun (TVar (Left (Ref r)) _) = toFun (fmap Left r)
toFun (TVar Right {} i) = die i "Value in fun position"
toFun TDef {..} = do -- TODO currently creating new vars every time, is this ideal?
  let fn = asString _tModule ++ "." ++ asString _tDefName
  args <- forM (_ftArgs _tFunType) $ \(Arg n t ai) -> do
    an <- freshId ai $ pfx fn n
    t' <- mangleType an <$> traverse toUserType t
    trackAST an t'
    return an
  tcs <- scopeToBody _tInfo (map (\ai -> Var ai ai) args) _tDefBody
  ft' <- traverse toUserType _tFunType
  return $ FDefun _tInfo fn ft' args tcs
toFun t = die (_tInfo t) "Non-var in fun position"

notEmpty :: MonadThrow m => Info -> String -> [a] -> m [a]
notEmpty i msg [] = die i msg
notEmpty _ _ as = return as

toAST :: Term (Either Ref (AST TcId)) -> TC (AST TcId)
toAST TNative {..} = die _tInfo "Native in value position"
toAST TDef {..} = die _tInfo "Def in value position"
toAST TUserType {..} = die _tInfo "User type in value position"
toAST (TVar v i) = case v of -- value position only, TApp has its own resolver
  (Left (Ref r)) -> toAST (fmap Left r)
  (Left Direct {}) -> die i "Native in value context"
  (Right t) -> return t
toAST TApp {..} = do
  fun <- toFun _tAppFun
  i <- freshId _tInfo $
       "app" ++ (case fun of FDefun {} -> "D"; _ -> "N") ++  _fName fun
  trackAST i $ idTyVar i
  as <- mapM toAST _tAppArgs
  (as',fun') <- case fun of
    FDefun {..} -> assocAST i (last _fBody) >> return (as,fun) -- non-empty verified in 'scopeToBody'
    FNative {..} -> case _fSpecial of
      Nothing -> return (as,fun)
      Just (f,_) -> (,) <$> notEmpty _tInfo "Expected >1 arg" (init as)
                    <*> pure (set fSpecial (Just (f,[last as])) fun)
  return $ App i fun' as'

toAST TBinding {..} = do
  bi <- freshId _tInfo (case _tBindCtx of BindLet -> "let"; BindKV -> "bind")
  trackAST bi $ idTyVar bi
  bs <- forM _tBindPairs $ \(Arg n t ai,v) -> do
    an <- freshId ai (pfx (show bi) n)
    t' <- mangleType an <$> traverse toUserType t
    trackAST an t'
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

toAST TList {..} = do
  i <- freshId _tInfo "list"
  ty <- traverse toUserType _tListType
  trackAST i ty
  List i <$> mapM toAST _tList <*> pure ty
toAST TObject {..} = do
  i <- freshId _tInfo "object"
  ty <- traverse toUserType _tUserType
  trackAST i ty
  Object i <$> mapM (\(k,v) -> (,) <$> toAST k <*> toAST v) _tObject <*> pure ty
toAST TConst {..} = toAST _tConstVal -- TODO typecheck here
toAST TKeySet {..} = trackPrim _tInfo TyKeySet (PrimKeySet _tKeySet)
toAST TValue {..} = trackPrim _tInfo TyValue (PrimValue _tValue)
toAST TLiteral {..} = trackPrim _tInfo (litToPrim _tLiteral) (PrimLit _tLiteral)
toAST TTable {..} = do
  i <- freshId _tInfo (asString _tModule ++ "." ++ asString _tTableName)
  ty <- TySchema TyTable <$> traverse toUserType _tTableType
  trackAST i ty
  return $ Table i ty
toAST TModule {..} = die _tInfo "Modules not supported"
toAST TUse {..} = die _tInfo "Use not supported"
toAST TStep {..} = die _tInfo "TODO steps/pacts"

trackPrim :: Info -> PrimType -> PrimValue -> TC (AST TcId)
trackPrim inf ty v = do
  i <- freshId inf (show ty)
  trackAST i (TyPrim ty)
  return $ Prim i ty v


toUserType :: Term (Either Ref (AST TcId)) -> TC UserType
toUserType t = case t of
  (TVar (Left r) _) -> derefUT r
  _ -> die (_tInfo t) $ "toUserType: expected user type: " ++ show t
  where
    derefUT (Ref r) = toUserType' r
    derefUT Direct {} = die (_tInfo t) $ "toUserType: unexpected direct ref: " ++ show t

toUserType' :: Show n => Term n -> TC UserType
toUserType' TUserType {..} = UserType _tUserTypeName _tModule <$> mapM (traverse toUserType') _tFields <*> pure _tInfo
toUserType' t = die (_tInfo t) $ "toUserType: expected user type: " ++ show t

bindArgs :: Info -> [a] -> Int -> TC a
bindArgs i args b =
  case args `atMay` b of
    Nothing -> die i $ "Missing arg: " ++ show b ++ ", " ++ show (length args) ++ " provided"
    Just a -> return a


infer :: Term Ref -> TC (Fun TcId)
infer t@TDef {..} = toFun (fmap Left t)
infer t = die (_tInfo t) "Non-def"


allVarsCheck :: TC (Either (M.Map TcId TypeSet) (M.Map TcId (Type UserType)))
allVarsCheck = do
  vs <- use tcVars
  let vs' = (`map` M.toList vs) $ \(tid,s) -> case asConcreteSingleton s of
        Nothing -> Left (tid,s)
        Just ty -> Right (tid,ty)
  case sequence vs' of
    Left _ -> do

      debug $ "Unification failed:"
      liftIO $ putDoc $ (indent 2 (vsep (map (\(Left l) -> sshow l) $ filter isLeft vs')) <> hardline)
      return (Left $ M.fromList $ concatMap (either pure (const [])) vs')
    Right ps -> return (Right (M.fromList ps))

debugState :: TC ()
debugState = liftIO . putDoc . pretty =<< get


substFun :: Fun TcId -> TC (Either (String,Fun TcId) (Fun (TcId,Type UserType)))
substFun f@FNative {} = return $ Right $ fmap (const (TcId def "" 0,TyAny)) f
substFun f@FDefun {..} = do
  debug "Transform"
  b'' <- mapM (walkAST $ substAppDefun Nothing) _fBody
  debug "Substitution"
  b' <- mapM (walkAST processNatives) b''
  debug "Pivot"
  pivot
  debugState
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
  let (Just (Just (Ref d))) = firstOf (rEnv . eeRefStore . rsModules . at mn . _Just . _2 . at fn) s
  return d

_infer :: FilePath -> ModuleName -> String -> IO (Either (String,Fun TcId) (Fun (TcId,Type UserType)), TcState)
_infer fp mn fn = _loadFun fp mn fn >>= \d -> runTC (infer d >>= substFun)

-- _pretty =<< _inferIssue
_inferIssue :: IO (Either (String,Fun TcId) (Fun (TcId,Type UserType)), TcState)
_inferIssue = _infer "examples/cp/cp.repl" "cp" "issue"

-- _pretty =<< _inferTransfer
_inferTransfer :: IO (Either (String,Fun TcId) (Fun (TcId,Type UserType)), TcState)
_inferTransfer = _infer "examples/accounts/accounts.repl" "accounts" "transfer"

-- prettify output of '_infer' runs
_pretty :: (Either (String,Fun TcId) (Fun (TcId,Type UserType)), TcState) -> IO ()
_pretty (Left (m,f),tc) = putDoc (pretty f <> hardline <> hardline <> pretty tc <$$> text m <> hardline)
_pretty (Right f,tc) = putDoc (pretty f <> hardline <> hardline <> pretty tc)

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
import Control.Monad.Catch
import Control.Lens hiding (pre,List)
import Bound.Scope
import Safe hiding (at)
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import Control.Arrow hiding ((<+>))
import Data.Aeson hiding (Object, (.=))
import Data.Foldable
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
-- import Data.String
import Data.Maybe
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data CheckerException = CheckerException Info String deriving (Eq)
instance Exception CheckerException
instance Show CheckerException where show (CheckerException i s) = renderInfo i ++ ": " ++ s

die :: MonadThrow m => Info -> String -> m a
die i s = throwM $ CheckerException i s

data VarRole = ArgVar Int | RetVar deriving (Eq,Show,Ord)

data VarType = Spec { _vtType :: Type } |
               Overload { _vtRole :: VarRole, _vtOverApp :: TcId }
               deriving (Eq,Ord)

instance Show VarType where
  show (Spec t) = show t
  show (Overload r ts) =
    show ts ++ "?" ++ (case r of ArgVar i -> show i; RetVar -> "r")

instance Pretty VarType where pretty = string . show

data TcState = TcState {
  _tcSupply :: Int,
  _tcVars :: M.Map TcId (S.Set VarType),
  _tcOverloads :: M.Map TcId FunTypes,
  _tcPivot :: M.Map VarType (S.Set VarType)
  } deriving (Eq,Show)


instance Default TcState where def = TcState 0 def def def
instance Pretty TcState where
  pretty TcState {..} = string "Vars:" PP.<$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <+> colon <+> string (show v)) $ M.toList $ M.map S.toList _tcVars) PP.<$>
    string "Overloads:" PP.<$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <> string "?" <+> colon <+>
                           align (vsep (map (string . show) (toList v)))) $ M.toList _tcOverloads) PP.<$>
    string "Pivot:" PP.<$>
    indent 2 (vsep $ map (\(k,v) -> pretty k  <+> colon PP.<$>
                           indent 4 (hsep (map (string . show) (toList v)))) $ M.toList _tcPivot)

newtype TC a = TC { unTC :: StateT TcState IO a }
  deriving (Functor,Applicative,Monad,MonadState TcState,MonadIO,MonadThrow,MonadCatch)


data TcId = TcId {
  _tiInfo :: Maybe Info, -- info not used for Eq/Ord, but implies a code (non-var) asset
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


freshId :: Maybe Info -> String -> TC TcId
freshId i n = TcId i n <$> state (_tcSupply &&& over tcSupply succ)

freshId' :: Info -> String -> TC TcId
freshId' i = freshId (Just i)

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
    _fTypes :: FunTypes } |
  FDefun {
    _fInfo :: Info,
    _fName :: String,
    _fType :: FunType,
    _fArgs :: [t],
    _fBody :: [AST t] }
  deriving (Eq,Functor,Foldable,Show)

instance Pretty t => Pretty (Fun t) where
  pretty FNative {..} = text ("Native: " ++ show _fName) PP.<$>
    indent 2 (vsep (map (text.show) (toList _fTypes)))
  pretty FDefun {..} = text ("Defun: " ++ show _fName) <+> text (show _fType) PP.<$>
    sep (map pretty _fArgs) PP.<$>
    vsep (map pretty _fBody)



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
  pretty Lit {..} = pretty _aLitType
  pretty Var {..} = pretty _aVar
  pretty Object {..} = "{" <+> align (sep (map (\(k,v) -> pretty k <> text ":" <+> pretty v) _aObject)) <+> "}"
  pretty List {..} = list (map pretty _aList)
  pretty Binding {..} =
    pretty _aId PP.<$>
    indent 2 (vsep (map (\(k,v) -> pretty k <> text ":" PP.<$> indent 4 (pretty v)) _aBindings)) PP.<$>
    indent 4 (vsep (map pretty _aBody))
  pretty App {..} =
    pretty _aId <+> text (_fName _aAppFun) PP.<$>
    indent 4 (case _aAppFun of
       FNative {..} -> vsep (map (text . show) (toList _fTypes))
       FDefun {..} -> text (show _fType)) PP.<$>
    (case _aAppFun of
        FNative {} -> (<> empty)
        FDefun {..} -> (PP.<$> indent 4 (vsep (map pretty _fBody))))
    (indent 2 (vsep (map pretty _aAppArgs)))



makeLenses ''AST
makeLenses ''Fun

runTC :: TC a -> IO (a, TcState)
runTC a = runStateT (unTC a) def

data Visit = Pre | Post deriving (Eq,Show)
type Visitor n = Visit -> AST n -> TC (AST n)

-- | Walk the AST, performing function both before and after descent into child elements.
walkAST :: Visitor n -> AST n -> TC (AST n)
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
        (case _aAppFun of fun@FNative {} -> return fun
                          fun@FDefun {..} -> do
                             db <- mapM (walkAST f) _fBody
                             return $ set fBody db fun) <*>
        mapM (walkAST f) _aAppArgs
  f Post t'


pivot :: TC ()
pivot = do
  m <- use tcVars
  let isVar (Spec TyVar {}) = True
      isVar Overload {} = True
      isVar _ = False
      initP = (`execState` M.empty) $
        forM_ (M.elems m) $ \vts ->
          forM_ vts $ \vt ->
            when (isVar vt) $ modify $ M.insertWith S.union vt vts
      rinse = execState lather
      lather = do
        p <- get
        forM_ (M.elems p) $ \vts ->
          forM_ vts $ \vt ->
            when (isVar vt) $ modify $ M.insertWith S.union vt vts
      rpt p = let p' = rinse p in if p' == p then p else rpt p'
  tcPivot .= rpt initP



processNatives :: Visitor TcId
processNatives Pre a@(App i FNative {..} as) = do
  case _fTypes of
    -- single funtype
    ft@FunType {} :| [] -> do
      let FunType {..} = mangleFunType i ft
      zipWithM_ (\(Arg _ t) aa -> trackVar (_aId aa) (Spec t)) _ftArgs as
      trackVar i (Spec _ftReturn)
    -- multiple funtypes
    fts -> do
      let fts' = fmap (mangleFunType i) fts
      tcOverloads %= M.insert i fts'
      zipWithM_ (\ai aa -> trackVar (_aId aa) (Overload (ArgVar ai) i)) [0..] as
      trackVar i (Overload RetVar i)
  return a
  -- need to track return types here too.
  -- a following pass can "push up" return types as necessary, just needs to be created and associated strongly
  -- with the app here.
  -- interestingly, we can now say that every ctor in AST is tracked, and therefore needs a stable name.
  -- concrete, single ftype: track concrete type
  -- concrete, same in multiple ftypes: track concrete type
  -- concrete, different in multiple ftypes: track arg/app/funtypes
  -- var, not used in other slots, single ftype: ignore
  -- var, not used in other slots, multiple ftypes: ignore
  -- var used in other slots, single ftype: track arg/app/funtypes
  -- var used in other slots, multiple ftype: track arg/app/funtypes
  --
  -- track arg/app/funtype: for a single ftype makes sense to simply mangle the typevar and track.
  --   note the return type will need to be tracked as well at this point.
  -- for multiple ftypes,
processNatives _ a = return a

-- | substitute app args into vars for FDefuns
substAppDefun :: Maybe (TcId, AST TcId) -> Visitor TcId
substAppDefun nr Pre t@Var {..} = case nr of
    Nothing -> return t
    Just (n,r) | n == _aVar -> assoc n r >> return r -- might need a typecheck here
               | otherwise -> return t
substAppDefun _ Post App {..} = do -- Post, to allow args to get substituted out first.
    af <- case _aAppFun of
      f@FNative {} -> return f
      f@FDefun {..} -> do
        fb' <- forM _fBody $ \bt ->
          foldM (\b fa -> walkAST (substAppDefun (Just fa)) b) bt (zip _fArgs _aAppArgs) -- this zip might need a typecheck
        return $ set fBody fb' f
    return (App _aId af _aAppArgs)
substAppDefun _ _ t = return t

-- | associate this Id with the type/typevar corresponding to another term
assoc :: TcId -> AST TcId -> TC ()
assoc n r = tcToTy r >>= trackVar' n -- might want to typecheck here?

tcToTy :: AST TcId -> TC (S.Set VarType)
tcToTy Lit {..} = return $ S.singleton $ Spec $ _aLitType
tcToTy Var {..} = (fromMaybe S.empty . M.lookup _aVar) <$> use tcVars
tcToTy Object {..} = return $ S.singleton $ Spec $ TyObject _aUserType
tcToTy List {..} = return $ S.singleton $ Spec $ TyList _aListType
tcToTy App {..} = (fromMaybe S.empty . M.lookup _aId) <$> use tcVars
tcToTy Binding {..} = (fromMaybe S.empty . M.lookup _aId) <$> use tcVars

trackVar :: TcId -> VarType -> TC ()
trackVar i t = trackVar' i (S.singleton t)

trackVar' :: TcId -> S.Set VarType -> TC ()
trackVar' i ts = tcVars %= M.insertWith S.union i ts


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
toFun (TVar (Left (Direct (TNative DefData {..} _ i))) _) = return $ FNative i _dName _dType
toFun (TVar (Left (Ref r)) _) = toFun (fmap Left r)
toFun (TVar Right {} i) = die i "Value in fun position"
toFun (TDef DefData {..} bod _ i) = do -- TODO currently creating new vars every time, is this ideal?
  ty <- case _dType of
    t :| [] -> return t
    _ -> die i "Multiple def types not allowed"
  let fn = maybe _dName ((++ ('.':_dName)) . asString) _dModule
  args <- forM (_ftArgs ty) $ \(Arg n t) -> do
    an <- freshId Nothing $ pfx fn n
    let t' = mangleType an t
    trackVar an (Spec t')
    return an
  tcs <- mkVars "arg" args >>= \as -> scopeToBody i as bod
  return $ FDefun i fn ty args tcs
toFun t = die (_tInfo t) "Non-var in fun position"

mkVars :: String -> [n] -> TC [AST n]
mkVars s as = zipWithM (\n i -> Var <$> freshId Nothing (s ++ show i) <*> pure n) as [(0::Int)..]


toAST :: Term (Either Ref (AST TcId)) -> TC (AST TcId)
toAST TNative {..} = die _tInfo "Native in value position"
toAST TDef {..} = die _tInfo "Def in value position"
toAST (TVar v i) = case v of -- value position only, TApp has its own resolver
  (Left (Ref r)) -> toAST (fmap Left r)
  (Left Direct {}) -> die i "Native in value context"
  (Right t) -> return t
toAST TApp {..} = do
  fun <- toFun _tAppFun
  i <- freshId' _tInfo $
       "app" ++ (case fun of FDefun {} -> "D"; _ -> "N") ++  _fName fun
  trackVar i (Spec $ idTyVar i)
  as <- mapM toAST _tAppArgs
  case fun of
    FDefun {..} -> assoc i (last _fBody)
    FNative {} -> return ()
  return $ App i fun as

toAST TBinding {..} = do
  bi <- freshId' _tInfo (case _tBindCtx of BindLet -> "let"; BindKV -> "bind")
  trackVar bi $ Spec $ case _tBindCtx of BindLet -> idTyVar bi; BindKV -> TyObject Nothing
  bs <- forM _tBindPairs $ \(Arg n t,v) -> do
    an <- freshId Nothing (pfx (show bi) n)
    let t' = mangleType an t
    trackVar an $ Spec t'
    v' <- toAST v
    assoc an v'
    return (an,v')
  as <- mkVars (case _tBindCtx of BindLet -> "larg"; BindKV -> "barg") (map fst bs)
  bb <- scopeToBody _tInfo as _tBindBody
  assoc bi (last bb)
  return $ Binding bi bs bb _tBindCtx

toAST TList {..} = List <$> freshId' _tInfo "list" <*> mapM toAST _tList <*> pure _tListType
toAST TObject {..} = Object <$> freshId' _tInfo "object" <*>
                       mapM (\(k,v) -> (,) <$> toAST k <*> toAST v) _tObject <*> pure _tUserType
toAST TConst {..} = toAST _tConstVal -- TODO typecheck here
toAST TKeySet {..} = freshId' _tInfo "keyset" >>= \i -> return $ Lit i TyKeySet (LVKeySet _tKeySet)
toAST TValue {..} = freshId' _tInfo "value" >>= \i -> return $ Lit i TyValue (LVValue _tValue)
toAST TLiteral {..} = do
  let ty = l2ty _tLiteral
  i <- freshId' _tInfo (show ty)
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


substFun :: Fun TcId -> TC (Fun TcId)
substFun f@FNative {} = return f
substFun f@FDefun {..} = do
  b' <- mapM (walkAST processNatives) =<< mapM (walkAST $ substAppDefun Nothing) _fBody
  pivot
  return $ set fBody b' f

_loadFun :: FilePath -> ModuleName -> String -> IO (Term Ref)
_loadFun fp mn fn = do
  (r,s) <- execScript' (Script fp) fp
  either (die def) (const (return ())) r
  let (Just (Just (Ref d))) = firstOf (rEnv . eeRefStore . rsModules . at mn . _Just . at fn) s
  return d

_infer :: FilePath -> ModuleName -> String -> IO (Fun TcId, TcState)
_infer fp mn fn = _loadFun fp mn fn >>= \d -> runTC (infer d >>= substFun)

_inferIssue :: IO (Fun TcId, TcState)
_inferIssue = _infer "examples/cp/cp.repl" "cp" "issue"

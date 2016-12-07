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
import Data.Aeson hiding (Object)
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

data TcState = TcState {
  _tcSupply :: Int,
  _tcVars :: M.Map TcId (S.Set Type)
  } deriving (Eq,Show)
instance Default TcState where def = TcState 0 def
instance Pretty TcState where
  pretty TcState {..} = string "Vars:" PP.<$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <+> colon <+> string (show v)) $ M.toList $ M.map S.toList _tcVars)



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
    (case _aBindCtx of BindLet -> "Let"; BindKV -> "Bind") PP.<$>
    indent 2 (vsep (map (\(k,v) -> pretty k <> text ":" PP.<$> indent 4 (pretty v)) _aBindings)) PP.<$>
    indent 4 (vsep (map pretty _aBody))
  pretty App {..} =
    "App" <+> text (_fName _aAppFun) PP.<$>
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


processNatives :: Visitor TcId
processNatives _ (App i FNative {..} as) = undefined
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
processNatives _ t = return t

-- | substitute app args into vars for FDefuns
substAppDefun :: Maybe (TcId, AST TcId) -> Visitor TcId
substAppDefun nr Pre t@Var {..} = case nr of
    Nothing -> return t
    Just (n,r) | n == _aVar -> do
                   rTy <- tcToTy r
                   trackVar' n rTy -- may want to typecheck here for locality ...
                   return r
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


tcToTy :: AST TcId -> TC (S.Set Type)
tcToTy Lit {..} = return $ S.singleton _aLitType
tcToTy Var {..} = (fromMaybe S.empty . M.lookup _aVar) <$> use tcVars
tcToTy Object {..} = return $ S.singleton $ TyObject _aUserType
tcToTy List {..} = return $ S.singleton $ TyList _aListType
tcToTy App {..} = return S.empty -- opportunity to capture app return types here?
tcToTy Binding {..} = return S.empty -- ^^ same

trackVar :: TcId -> Type -> TC ()
trackVar i t = trackVar' i (S.singleton t)

trackVar' :: TcId -> S.Set Type -> TC ()
trackVar' i ts = tcVars %= M.insertWith S.union i ts


scopeToBody :: Info -> [AST TcId] -> Scope Int Term (Either Ref (AST TcId)) -> TC [AST TcId]
scopeToBody i args bod = do
  bt <- instantiate (return . Right) <$> traverseScope (bindArgs i args) return bod
  case bt of
    (TList ts@(_:_) _ _) -> mapM toAST ts
    _ -> die i "Malformed def body"

mangleType :: (String -> String) -> Type -> Type
mangleType f t@TyVar {} = over tvId f t
mangleType f t@TyList {} = over (tlType . _Just) (mangleType f) t
mangleType f t@TyFun {} = over tfType (over ftReturn (mangleType f) .
                                       over (ftArgs.traverse.aType) (mangleType f)) t
mangleType _ t = t


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
    an <- freshId Nothing $ fn ++ "_" ++ n
    let t' = mangleType (const (show an)) t
    trackVar an t'
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
  i <- freshId' _tInfo "app"
  trackVar i $ TyVar (show i) []
  App i <$> toFun _tAppFun <*> mapM toAST _tAppArgs

toAST TBinding {..} = do
  bi <- freshId' _tInfo (case _tBindCtx of BindLet -> "let"; BindKV -> "bind")
  trackVar bi $ case _tBindCtx of BindLet -> TyVar (show bi) []; BindKV -> TyObject Nothing
  bs <- forM _tBindPairs $ \(Arg n t,v) -> do
    an <- freshId Nothing (show bi ++ "_" ++ n)
    let t' = mangleType (const (show an)) t
    trackVar an t'
    (an,) <$> toAST v
  as <- mkVars (case _tBindCtx of BindLet -> "larg"; BindKV -> "barg") (map fst bs)
  Binding <$> freshId' _tInfo "bind" <*> pure bs <*>
    scopeToBody _tInfo as _tBindBody <*> pure _tBindCtx

toAST TList {..} = List <$> freshId' _tInfo "list" <*> mapM toAST _tList <*> pure _tListType
toAST TObject {..} = Object <$> freshId' _tInfo "object" <*>
                       mapM (\(k,v) -> (,) <$> toAST k <*> toAST v) _tObject <*> pure _tUserType
toAST TConst {..} = toAST _tConstVal -- TODO typecheck here
toAST TKeySet {..} = freshId' _tInfo "KeySet" >>= \i -> return $ Lit i TyKeySet (LVKeySet _tKeySet)
toAST TValue {..} = freshId' _tInfo "Value" >>= \i -> return $ Lit i TyValue (LVValue _tValue)
toAST TLiteral {..} = freshId' _tInfo "Literal" >>= \i -> return $ Lit i (ty _tLiteral) (LVLit _tLiteral)
  where ty LInteger {} = TyInteger
        ty LDecimal {} = TyDecimal
        ty LString {} = TyString
        ty LBool {} = TyBool
        ty LTime {} = TyTime
toAST TModule {..} = die _tInfo "Modules not supported"
toAST TUse {..} = die _tInfo "Use not supported"
toAST TStep {..} = die _tInfo "TODO steps/pacts"


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
  b' <- mapM (walkAST $ substAppDefun Nothing) _fBody
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

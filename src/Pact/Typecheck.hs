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
import Control.Lens hiding (pre)
import Bound.Scope
import Safe hiding (at)
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import Control.Arrow hiding ((<+>))
import Data.Aeson
import Data.Foldable
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Data.String
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

pwords :: [String] -> String
pwords ws = "(" ++ unwords ws ++ ")"

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

data TCLit =
  TCLit Literal |
  TCKeySet PactKeySet |
  TCValue Value
  deriving (Eq,Show)
instance Pretty TCLit where
  pretty (TCLit l) = text (show l)
  pretty (TCKeySet k) = text (show k)
  pretty (TCValue v) = text (show v)

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
    _fBody :: [TcTerm t] }
  deriving (Eq,Functor,Foldable)

instance Show t => Show (Fun t) where
  show (FNative _ a _b) = pwords ["Native",show a]
  show (FDefun _ a _b c d) = pwords ["Defun",show a,show c,show d]

ind :: Doc
ind = string " "

instance Pretty t => Pretty (Fun t) where
  pretty FNative {..} = text ("Native: " ++ show _fName) PP.<$>
    indent 2 (vsep (map (text.show) (toList _fTypes)))
  pretty FDefun {..} = text ("Defun: " ++ show _fName) <+> text (show _fType) PP.<$>
    sep (map pretty _fArgs) PP.<$>
    vsep (map pretty _fBody)



data TcTerm t =
  TcApp {
  _tcId :: TcId,
  _tcAppFun :: Fun t,
  _tcAppArgs :: [TcTerm t]
  } |
  TcBinding {
  _tcId :: TcId,
  _tcBindings :: [(t,TcTerm t)],
  _tcBody :: [TcTerm t],
  _tcBindCtx :: BindCtx
  } |
  TcList {
  _tcId :: TcId,
  _tcList :: [TcTerm t],
  _tcListType :: Maybe Type
  } |
  TcObject {
  _tcId :: TcId,
  _tcObject :: [(TcTerm t,TcTerm t)],
  _tcUserType :: Maybe TypeName
  } |
  TcLit {
  _tcId :: TcId,
  _tcType :: Type,
  _tcLit :: TCLit
  } |
  TcVar {
  _tcId :: TcId,
  _tcVar :: t }
  deriving (Eq,Functor,Foldable)

instance Show t => Show (TcTerm t) where
  show (TcApp _ a b) = pwords ["App",show b,show a]
  show (TcBinding _ a b c) = pwords ["Binding",show a,show b,show c]
  show (TcList _ a b) = pwords ["List",show a,show b]
  show (TcObject _ a b) = pwords ["Object",show a,show b]
  show (TcLit _ a _b) = pwords ["Lit",show a]
  show (TcVar _ a) = pwords ["Var",show a]

instance Pretty t => Pretty (TcTerm t) where
  pretty TcLit {..} = pretty _tcLit
  pretty TcVar {..} = pretty _tcVar
  pretty TcObject {..} = "{" <+> align (sep (map (\(k,v) -> pretty k <> text ":" <+> pretty v) _tcObject)) <+> "}"
  pretty TcList {..} = list (map pretty _tcList)
  pretty TcBinding {..} =
    (case _tcBindCtx of BindLet -> "Let"; BindKV -> "Bind") PP.<$>
    indent 2 (vsep (map (\(k,v) -> pretty k <> text ":" PP.<$> indent 4 (pretty v)) _tcBindings)) PP.<$>
    indent 4 (vsep (map pretty _tcBody))
  pretty TcApp {..} =
    "App" <+> text (_fName _tcAppFun) PP.<$>
    indent 4 (case _tcAppFun of
       FNative {..} -> vsep (map (text . show) (toList _fTypes))
       FDefun {..} -> text (show _fType)) PP.<$>
    (case _tcAppFun of
        FNative {} -> (<> empty)
        FDefun {..} -> (PP.<$> indent 4 (vsep (map pretty _fBody))))
    (indent 2 (vsep (map pretty _tcAppArgs)))



makeLenses ''TcTerm
makeLenses ''Fun

runTC :: TC a -> IO (a, TcState)
runTC a = runStateT (unTC a) def

data Visit = Pre | Post deriving (Eq,Show)
type Visitor n = Visit -> TcTerm n -> TC (TcTerm n)

-- | Walk the AST, performing function both before and after descent into child elements.
walkAST :: Visitor n -> TcTerm n -> TC (TcTerm n)
walkAST f t@TcLit {} = f Pre t -- lit, var should ignore Visit
walkAST f t@TcVar {} = f Pre t -- lit, var should ignore Visit
walkAST f t@TcObject {} = do
  TcObject {..} <- f Pre t
  t' <- TcObject _tcId <$>
         forM _tcObject (\(k,v) -> (,) <$> walkAST f k <*> walkAST f v) <*>
         pure _tcUserType
  f Post t'
walkAST f t@TcList {} = do
  TcList {..} <- f Pre t
  t' <- TcList _tcId <$> mapM (walkAST f) _tcList <*> pure _tcListType
  f Post t'
walkAST f t@TcBinding {} = do
  TcBinding {..} <- f Pre t
  t' <- TcBinding _tcId <$>
        forM _tcBindings (\(k,v) -> (k,) <$> walkAST f v) <*>
        mapM (walkAST f) _tcBody <*> pure _tcBindCtx
  f Post t'
walkAST f t@TcApp {} = do
  TcApp {..} <- f Pre t
  t' <- TcApp _tcId <$>
        (case _tcAppFun of fun@FNative {} -> return fun
                           fun@FDefun {..} -> do
                             db <- mapM (walkAST f) _fBody
                             return $ set fBody db fun) <*>
        mapM (walkAST f) _tcAppArgs
  f Post t'


processNatives :: Visitor TcId
processNatives _ (TcApp i FNative {..} as) = undefined
  -- need to track return types here too.
  -- a following pass can "push up" return types as necessary, just needs to be created and associated strongly
  -- with the app here.
  -- interestingly, we can now say that every ctor in TcTerm is tracked, and therefore needs a stable name.
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
substAppDefun :: Maybe (TcId, TcTerm TcId) -> Visitor TcId
substAppDefun nr _ t@TcVar {..} = case nr of
    Nothing -> return t
    Just (n,r) | n == _tcVar -> do
                   rTy <- tcToTy r
                   tcVars %= M.insertWith S.union n rTy -- may want to typecheck here for locality ...
                   return r
               | otherwise -> return t
substAppDefun _ Post TcApp {..} = do -- Post, to allow args to get substituted out first.
    af <- case _tcAppFun of
      f@FNative {} -> return f
      f@FDefun {..} -> do
        fb' <- forM _fBody $ \bt ->
          foldM (\b fa -> walkAST (substAppDefun (Just fa)) b) bt (zip _fArgs _tcAppArgs) -- this zip might need a typecheck
        return $ set fBody fb' f
    return (TcApp _tcId af _tcAppArgs)
substAppDefun _ _ t = return t


tcToTy :: TcTerm TcId -> TC (S.Set Type)
tcToTy TcLit {..} = return $ S.singleton _tcType
tcToTy TcVar {..} = (fromMaybe S.empty . M.lookup _tcVar) <$> use tcVars
tcToTy TcObject {..} = return $ S.singleton $ TyObject _tcUserType
tcToTy TcList {..} = return $ S.singleton $ TyList _tcListType
tcToTy TcApp {..} = return S.empty -- opportunity to capture app return types here?
tcToTy TcBinding {..} = return S.empty -- ^^ same




scopeToBody :: Info -> [TcTerm TcId] -> Scope Int Term (Either Ref (TcTerm TcId)) -> TC [TcTerm TcId]
scopeToBody i args bod = do
  bt <- instantiate (return . Right) <$> traverseScope (bindArgs i args) return bod
  case bt of
    (TList ts@(_:_) _ _) -> mapM asValue ts
    _ -> die i "Malformed def body"

mangleType :: (String -> String) -> Type -> Type
mangleType f t@TyVar {} = over tvId f t
mangleType f t@TyList {} = over (tlType . _Just) (mangleType f) t
mangleType f t@TyFun {} = over tfType (over ftReturn (mangleType f) .
                                       over (ftArgs.traverse.aType) (mangleType f)) t
mangleType _ t = t


asFun :: Term (Either Ref (TcTerm TcId)) -> TC (Fun TcId)
asFun (TVar (Left (Direct (TNative DefData {..} _ i))) _) = return $ FNative i _dName _dType
asFun (TVar (Left (Ref r)) _) = asFun (fmap Left r)
asFun (TVar Right {} i) = die i "Value in fun position"
asFun (TDef DefData {..} bod _ i) = do
  ty <- case _dType of
    t :| [] -> return t
    _ -> die i "Multiple def types not allowed"
  let fn = maybe _dName ((++ ('.':_dName)) . asString) _dModule
  args <- forM (_ftArgs ty) $ \(Arg n t) -> do
    an <- freshId Nothing $ fn ++ "_" ++ n
    let t' = mangleType (const (show an)) t
    tcVars %= M.insertWith S.union an (S.singleton t')
    return an
  tcs <- mkVars "arg" args >>= \as -> scopeToBody i as bod
  return $ FDefun i fn ty args tcs
asFun t = die (_tInfo t) "Non-var in fun position"

mkVars :: String -> [n] -> TC [TcTerm n]
mkVars s as = zipWithM (\n i -> TcVar <$> freshId Nothing (s ++ show i) <*> pure n) as [(0::Int)..]


asValue :: Term (Either Ref (TcTerm TcId)) -> TC (TcTerm TcId)
asValue TNative {..} = die _tInfo "Native in value position"
asValue TDef {..} = die _tInfo "Def in value position"
asValue (TVar v i) = case v of -- value position only, TApp has its own resolver
  (Left (Ref r)) -> asValue (fmap Left r)
  (Left Direct {}) -> die i "Native in value context"
  (Right t) -> return t
asValue TApp {..} = TcApp <$> freshId' _tInfo "app" <*> asFun _tAppFun <*> mapM asValue _tAppArgs
asValue TBinding {..} = do
  bi <- freshId' _tInfo (case _tBindCtx of BindLet -> "let"; BindKV -> "bind")
  bs <- forM _tBindPairs $ \(Arg n t,v) -> do
    an <- freshId Nothing (show bi ++ "_" ++ n)
    let t' = mangleType (const (show an)) t
    tcVars %= M.insertWith S.union an (S.singleton t')
    (an,) <$> asValue v
  as <- mkVars (case _tBindCtx of BindLet -> "larg"; BindKV -> "barg") (map fst bs)
  TcBinding <$> freshId' _tInfo "bind" <*> pure bs <*>
    scopeToBody _tInfo as _tBindBody <*> pure _tBindCtx

asValue TList {..} = TcList <$> freshId' _tInfo "list" <*> mapM asValue _tList <*> pure _tListType
asValue TObject {..} = TcObject <$> freshId' _tInfo "object" <*>
                       mapM (\(k,v) -> (,) <$> asValue k <*> asValue v) _tObject <*> pure _tUserType
asValue TConst {..} = asValue _tConstVal -- TODO typecheck here
asValue TKeySet {..} = freshId' _tInfo "KeySet" >>= \i -> return $ TcLit i TyKeySet (TCKeySet _tKeySet)
asValue TValue {..} = freshId' _tInfo "Value" >>= \i -> return $ TcLit i TyValue (TCValue _tValue)
asValue TLiteral {..} = freshId' _tInfo "Literal" >>= \i -> return $ TcLit i (ty _tLiteral) (TCLit _tLiteral)
  where ty LInteger {} = TyInteger
        ty LDecimal {} = TyDecimal
        ty LString {} = TyString
        ty LBool {} = TyBool
        ty LTime {} = TyTime
asValue TModule {..} = die _tInfo "Modules not supported"
asValue TUse {..} = die _tInfo "Use not supported"
asValue TStep {..} = die _tInfo "TODO steps/pacts"


bindArgs :: Info -> [a] -> Int -> TC a
bindArgs i args b =
  case args `atMay` b of
    Nothing -> die i $ "Missing arg: " ++ show b ++ ", " ++ show (length args) ++ " provided"
    Just a -> return a


infer :: Term Ref -> TC (Fun TcId)
infer t@TDef {..} = asFun (fmap Left t)
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

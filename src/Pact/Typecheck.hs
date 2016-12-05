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
import Control.Lens
import Bound.Scope
import Safe hiding (at)
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Control.Arrow hiding ((<+>))
import Data.Aeson
import Data.Data
import Data.Data.Lens
import Data.Maybe
import Data.Foldable
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data CheckerException = CheckerException Info String deriving (Eq)
instance Exception CheckerException
instance Show CheckerException where show (CheckerException i s) = renderInfo i ++ ": " ++ s

die :: MonadThrow m => Info -> String -> m a
die i s = throwM $ CheckerException i s

data TcState = TcState {
  _tcBind :: Int
  } deriving (Eq,Show)
instance Default TcState where def = TcState 0
makeLenses ''TcState

newtype TC a = TC { unTC :: StateT TcState IO a }
  deriving (Functor,Applicative,Monad,MonadState TcState,MonadIO,MonadThrow,MonadCatch)

pwords :: [String] -> String
pwords ws = "(" ++ unwords ws ++ ")"

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
    _fArgs :: [t], -- Just names not values. Need to track Arg metadata.
    _fBody :: [TcTerm t] } -- these are AST entries
  deriving (Eq,Functor,Foldable)

instance Show t => Show (Fun t) where
  show (FNative _ a b) = pwords ["Native",show a]
  show (FDefun _ a b c d) = pwords ["Defun",show a,show c,show d]

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
  _tcInfo :: Info,
  _tcAppFun :: Fun t,
  _tcAppArgs :: [TcTerm t] -- these are AST entries: apps, bindings, lists, objects, lits, or vars/refs.
  } |
  TcBinding {
  _tcInfo :: Info,
  _tcBindings :: [(t,TcTerm t)], -- names and values, need to track Arg metadata.
  _tcBody :: [TcTerm t], -- AST
  _tcBindCtx :: BindCtx
  } |
  TcList {
  _tcInfo :: Info,
  _tcList :: [TcTerm t], -- AST
  _tcListType :: Maybe Type
  } |
  TcObject {
  _tcInfo :: Info,
  _tcObject :: [(TcTerm t,TcTerm t)], -- AST
  _tcUserType :: Maybe TypeName
  } |
  TcLit {
  _tcInfo :: Info,
  _tcType :: Type,
  _tcLit :: TCLit
  } |
  TcVar {
  _tcInfo :: Info,
  _tcVar :: t }
  deriving (Eq,Functor,Foldable)

instance Show t => Show (TcTerm t) where
  show (TcApp _ a b) = pwords ["App",show b,show a]
  show (TcBinding _ a b c) = pwords ["Binding",show a,show b,show c]
  show (TcList _ a b) = pwords ["List",show a,show b]
  show (TcObject _ a b) = pwords ["Object",show a,show b]
  show (TcLit _ a b) = pwords ["Lit",show a]
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
    ((case _tcAppFun of
        FNative {} -> (<> empty)
        FDefun {..} -> (PP.<$> indent 4 (vsep (map pretty _fBody))))
     (indent 2 (vsep (map pretty _tcAppArgs))))



makeLenses ''TcTerm
makeLenses ''Fun

runTC :: TC a -> IO (a, TcState)
runTC a = runStateT (unTC a) def

type TcName = String


scopeToBody :: Info -> [TcTerm TcName] -> Scope Int Term (Either Ref (TcTerm TcName)) -> TC [TcTerm TcName]
scopeToBody i args bod = do
  bt <- instantiate (return . Right) <$> traverseScope (bindArgs i args) return bod
  case bt of
    (TList ts@(_:_) _ _) -> mapM asValue ts
    _ -> die i "Malformed def body"



asFun :: Term (Either Ref (TcTerm TcName)) -> TC (Fun TcName)
asFun (TVar (Left (Direct (TNative DefData {..} _ i))) _) = return $ FNative i _dName _dType
asFun (TVar (Left (Ref r)) _) = asFun (fmap Left r)
asFun (TVar Right {} i) = die i "Value in fun position"
asFun (TDef DefData {..} bod _ i) = do
  ty <- case _dType of
    t :| [] -> return t
    _ -> die i "Multiple def types not allowed"
  let fn = maybe _dName ((++ ('.':_dName)) . asString) _dModule
      args = map (\a@(Arg n _) -> (a,fn ++ "_" ++ n)) (_ftArgs ty)
  tcs <- scopeToBody i (map (TcVar def . snd) args) bod
  return $ FDefun i fn ty (map snd args) tcs -- TODO track Arg metadata
asFun t = die (_tInfo t) "Non-var in fun position"


asValue :: Term (Either Ref (TcTerm TcName)) -> TC (TcTerm TcName)
asValue TNative {..} = die _tInfo "Native in value position"
asValue TDef {..} = die _tInfo "Def in value position"
asValue (TVar v i) = case v of -- value position only, TApp has its own resolver
  (Left (Ref r)) -> asValue (fmap Left r)
  (Left Direct {}) -> die i "Native in value context"
  (Right t) -> return t
asValue (TApp tf tas ti) = TcApp ti <$> asFun tf <*> mapM asValue tas
asValue TBinding {..} = do
  bi <- state (_tcBind &&& over tcBind succ)
  let bindName a = "bind" ++ show bi ++ "_" ++ (_aName a)
  bs <- forM _tBindPairs $ \(a,v) -> (,) <$> return (bindName a) <*> asValue v
  TcBinding <$> pure _tInfo <*> pure bs <*> scopeToBody _tInfo (map (TcVar def . fst) bs) _tBindBody <*> pure _tBindCtx

asValue TList {..} = TcList _tInfo <$> mapM asValue _tList <*> pure _tListType
asValue TObject {..} = TcObject _tInfo <$> mapM (\(k,v) -> (,) <$> asValue k <*> asValue v) _tObject <*> pure _tUserType
asValue TConst {..} = asValue _tConstVal -- TODO typecheck here
asValue TKeySet {..} = return $ TcLit _tInfo TyKeySet (TCKeySet _tKeySet)
asValue TValue {..} = return $ TcLit _tInfo TyValue (TCValue _tValue)
asValue TLiteral {..} = return $ TcLit _tInfo (ty _tLiteral) (TCLit _tLiteral)
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


infer :: Term Ref -> TC (Fun TcName)
infer t@TDef {..} = asFun (fmap Left t)
infer t = die (_tInfo t) "Non-def"


_loadFun :: FilePath -> ModuleName -> TcName -> IO (Term Ref)
_loadFun fp mn fn = do
  (r,s) <- execScript' (Script fp) fp
  either (die def) (const (return ())) r
  let (Just (Just (Ref d))) = firstOf (rEnv . eeRefStore . rsModules . at mn . _Just . at fn) s
  return d

_infer :: FilePath -> ModuleName -> String -> IO (Fun TcName, TcState)
_infer fp mn fn = _loadFun fp mn fn >>= \d -> runTC (infer d)

_inferIssue :: IO (Fun TcName, TcState)
_inferIssue = _infer "examples/cp/cp.repl" "cp" "issue"

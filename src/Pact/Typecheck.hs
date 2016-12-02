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
import Control.Arrow
import Data.Aeson

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

type Body = Scope Int Term TcTerm

pwords ws = "(" ++ unwords ws ++ ")"

data TCLit =
  TCLit Literal |
  TCKeySet PactKeySet |
  TCValue Value
  deriving (Eq,Show)

data Fun =
  FNative Info String FunTypes |
  FDefun Info String FunType [(Arg,TcTerm)] [TcTerm]
  deriving (Eq)

instance Show Fun where
  show (FNative _ a b) = pwords ["Native",show a]
  show (FDefun _ a b c d) = pwords ["Defun",show a,show c,show d]


data TcTerm =
  TcApp Info Fun [TcTerm] |
  TcBinding Info [(Arg,TcTerm)] [TcTerm] BindCtx |
  TcList Info [TcTerm] (Maybe Type) |
  TcObject Info [(TcTerm,TcTerm)] (Maybe TypeName) |
  TcLit Info Type TCLit |
  TcVar Info String
  deriving (Eq)

instance Show TcTerm where
  show (TcApp _ a b) = pwords ["App",show a,show b]
  show (TcBinding _ a b c) = pwords ["Binding",show a,show b,show c]
  show (TcList _ a b) = pwords ["List",show a,show b]
  show (TcObject _ a b) = pwords ["Object",show a,show b]
  show (TcLit _ a b) = pwords ["Lit",show a]
  show (TcVar _ a) = pwords ["Var",show a]

runTC :: TC a -> IO (a, TcState)
runTC a = runStateT (unTC a) def

scopeToBody
  :: Info
     -> [TcTerm] -> Scope Int Term (Either Ref TcTerm) -> TC [TcTerm]
scopeToBody i args bod = do
  bt <- instantiate (return . Right) <$> traverseScope (bindArgs i args) return bod
  case bt of
    (TList ts@(_:_) _ _) -> mapM asValue ts
    _ -> die i "Malformed def body"



asFun :: Term (Either Ref TcTerm) -> TC Fun
asFun (TVar (Left (Direct (TNative DefData {..} _ i))) _) = return $ FNative i _dName _dType
asFun (TVar (Left (Ref r)) _) = asFun (fmap Left r)
asFun (TVar Right {} i) = die i "Value in fun position"
asFun (TDef DefData {..} bod _ i) = do
  ty <- case _dType of
    t :| [] -> return t
    _ -> die i "Multiple def types not allowed"
  let fn = maybe _dName ((++ ('.':_dName)) . asString) _dModule
      args = map (\a@(Arg n _) -> (a,TcVar def (fn ++ "_" ++ n))) (_ftArgs ty)
  tcs <- scopeToBody i (map snd args) bod
  return $ FDefun i fn ty args tcs
asFun t = die (_tInfo t) "Non-var in fun position"


asValue :: Term (Either Ref TcTerm) -> TC TcTerm
asValue TNative {..} = die _tInfo "Native in value position"
asValue TDef {..} = die _tInfo "Def in value position"
asValue (TVar v i) = case v of -- value position only, TApp has its own resolver
  (Left (Ref r)) -> asValue (fmap Left r)
  (Left Direct {}) -> die i "Native in value context"
  (Right t) -> return t
asValue (TApp tf tas ti) = TcApp ti <$> asFun tf <*> mapM asValue tas
asValue TBinding {..} = do
  bs <- case _tBindCtx of
    BindLet -> mapM (\(k,v) -> (k,) <$> asValue v) _tBindPairs -- TODO typecheck here?
    BindKV -> do
      bi <- state (_tcBind &&& over tcBind succ)
      let pfx = "bind" ++ show bi ++ "_"
      return $ map (second (TcVar def . ((pfx ++ "_") ++) . show)) _tBindPairs
  tcs <- scopeToBody _tInfo (map snd bs) _tBindBody
  return $ TcBinding _tInfo bs tcs _tBindCtx

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



bindArgs :: Show a => Info -> [a] -> Int -> TC a
bindArgs i args b =
  case args `atMay` b of
    Nothing -> die i $ "Missing arg: " ++ show b ++ ", " ++ show args
    Just a -> return a


infer :: Term Ref -> TC Fun
infer t@TDef {..} = asFun (fmap Left t)
infer t = die (_tInfo t) "Non-def"


_loadFun :: FilePath -> ModuleName -> String -> IO (Term Ref)
_loadFun fp mn fn = do
  (r,s) <- execScript' (Script fp) fp
  either (die def) (const (return ())) r
  let (Just (Just (Ref d))) = firstOf (rEnv . eeRefStore . rsModules . at mn . _Just . at fn) s
  return d

_infer :: FilePath -> ModuleName -> String -> IO (Fun, TcState)
_infer fp mn fn = _loadFun fp mn fn >>= \d -> runTC (infer d)

_inferIssue :: IO (Fun, TcState)
_inferIssue = _infer "examples/cp/cp.repl" "cp" "issue"

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Infer where

import Pact.Repl
import Pact.Types
import Control.Monad.Catch
import Control.Lens
import Bound.Scope
import Safe hiding (at)
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State

data CheckerException = CheckerException Info String deriving (Eq)
instance Exception CheckerException
instance Show CheckerException where show (CheckerException i s) = renderInfo i ++ ": " ++ s

die :: MonadThrow m => Info -> String -> m a
die i s = throwM $ CheckerException i s

newtype IName = IName (Int,String) deriving (Eq,Show,Ord)

data Type =
    TyInteger |
    TyDecimal |
    TyTime |
    TyBool |
    TyString |
    TyList |
    TyObject |
    TyValue |
    TyKeySet |
    TySpecial String
    deriving (Eq,Show,Ord)

data InferState = InferState {
      _isTypes :: M.Map IName (S.Set Type)
    , _isEquiv :: M.Map IName (S.Set IName)
    , _isFresh :: Int
    } deriving (Eq,Show)
makeLenses ''InferState
instance Default InferState where def = InferState def def 0

newtype TC a = TC { unTC :: StateT InferState IO a }
  deriving (Functor,Applicative,Monad,MonadState InferState,MonadIO,MonadThrow,MonadCatch)

runTC :: TC a -> IO (a, InferState)
runTC a = runStateT (unTC a) def

type FunInfer = [IVar] -> TC IVar

data FunSpec = FunSpec {
    _fsInfo :: Info
  , _fsName :: String
  , _fsRequired :: [ArgSpec]
  , _fsOptional :: [ArgSpec]
  , _fsInfer :: FunInfer
  }

data ArgSpec = ArgSpec {
    _asName :: String
  , _asType :: S.Set Type
  } deriving (Eq,Show)

data IVar =
    IVar Info IName |
    IRef Ref |
    ILit Type
    deriving (Eq,Show)


_loadFun :: FilePath -> ModuleName -> String -> IO (Term Ref)
_loadFun fp mn fn = do
  (r,s) <- execScript' (Script fp) fp
  either (die def) (const (return ())) r
  let (Just (Just (Ref d))) = firstOf (rEnv . eeRefStore . rsModules . at mn . _Just . at fn) s
  return d

_infer :: FilePath -> ModuleName -> String -> IO (IVar, InferState)
_infer fp mn fn = _loadFun fp mn fn >>= \d -> runTC (infer d Nothing)

_inferIssue :: IO (IVar, InferState)
_inferIssue = _infer "examples/cp/cp.repl" "cp" "issue"

_inferTransferInv :: IO (IVar, InferState)
_inferTransferInv = _infer "examples/cp/cp.repl" "cp" "transfer-inventory"

infer :: Term Ref -> Maybe [IVar] -> TC IVar
infer (TDef dd db _ i) vargs = do
  args <- case vargs of
    Just as -> return as
    Nothing -> mkFresh i (map (,def) $ _dArgs dd)
  s <- traverseScope (bindArgs i args) (return . IRef) db
  inferBody i s
infer t _ = die (_tInfo t) $ "infer: non-def: " ++ show t

bindArgs :: Info -> [IVar] -> Int -> TC IVar
bindArgs i args b =
  case args `atMay` b of
    Nothing -> die i $ "Missing arg: " ++ show b ++ ", " ++ show args
    Just a -> return a

inferBody :: Info -> Scope IVar Term IVar -> TC IVar
inferBody i s = do
  let (TList ts _) = instantiate (`TVar` def) s
  when (null ts) $ die i "Empty body"
  is <- forM ts inferTerm
  return (last is)

mkFresh :: Info -> [(String,S.Set Type)] -> TC [IVar]
mkFresh i as = do
  isFresh %= succ
  f <- use isFresh
  forM as $ \(a,tys) -> do
    let n = IName (f,a)
    old <- use (isTypes.at n)
    case old of
      Nothing -> isTypes %= M.insert n tys
      Just _ -> die i $ "Duplicate name: " ++ show n
    return $ IVar i n

mkFresh1 :: Info -> String -> S.Set Type -> TC IVar
mkFresh1 i n ts = head <$> mkFresh i [(n,ts)]


inferApp :: Info -> Term IVar -> [Term IVar] -> TC IVar
inferApp i (TVar (IRef d) _) as = inferAppRef i d as
inferApp i v _ = die i $ "inferApp: applying non-ref: " ++ show v

mkSpec :: Info -> DefData -> [[Type]] -> [[Type]] -> FunInfer -> TC FunSpec
mkSpec i dd@DefData {..} req opt ret
  | length _dArgs /= length (req ++ opt) = die def $ "mkSpec: argnames do not match spec: " ++
                                           show (dd,req,opt)
  | otherwise = return $ FunSpec i _dName
                (zipWith ArgSpec _dArgs (map S.fromList req))
                (zipWith ArgSpec (drop (length req) _dArgs) (map S.fromList opt))
                ret

asSingleton :: S.Set a -> Maybe a
asSingleton s | S.size s == 1 = Just (head (S.toList s))
              | otherwise = Nothing

funMono :: Type -> FunInfer
funMono ty = const (return (ILit ty))

type FunInferBin = Info -> IVar -> IVar -> TC IVar

funBin :: Info -> FunInferBin -> FunInfer
funBin i f [a,b] = f i a b
funBin i _ as = die i $ "Expected two arguments, received: " ++ show as

funBinEquiv :: Bool -> FunInferBin
funBinEquiv _ i v@(ILit a) (ILit b) | a == b = return v
                                    | otherwise = die i $ "Typecheck error, expecting equal types: " ++ show (a,b)
funBinEquiv _ i (ILit ty) (IVar vi n) = setMono i ty vi n
funBinEquiv _ i (IVar vi n) (ILit ty) = setMono i ty vi n
funBinEquiv mkF i a@(IVar ai an) (IVar bi bn) = do
  atys <- lookupVar ai an
  btys <- lookupVar bi bn
  let int = S.intersection atys btys
  when (not (S.null atys) && not (S.null btys) && S.null int) $
    die i $ "Typecheck error: equivalent variables have incompatible types: " ++ show (an,atys) ++ " <> " ++ show (bn,btys)
  isTypes %= M.insert an int . M.insert bn int
  case asSingleton int of
    Just ty -> return $ ILit ty
    Nothing -> do
      isEquiv %= M.insertWith S.union an (S.singleton bn)
      if mkF
        then mkFresh1 i "return" int
        else return a
funBinEquiv _ i a b = die i $ "funBinEquiv: unsupported IVars: " ++ show (a,b)

binNumCoercions :: Info -> Type -> Type -> TC Type
binNumCoercions _ TyDecimal TyInteger = return TyDecimal
binNumCoercions _ TyInteger TyDecimal = return TyDecimal
binNumCoercions _ TyDecimal TyDecimal = return TyDecimal
binNumCoercions _ TyInteger TyInteger = return TyInteger
binNumCoercions i a b = die i $ "Typecheck error, expected numeric types: " ++ show (a,b)

binNumOneVar :: Info -> Info -> IName -> Type -> TC IVar
binNumOneVar i vi n x = do
  vtys <- lookupVar vi n
  case asSingleton vtys of
    Just ty -> ILit <$> binNumCoercions i ty x
    Nothing | x == TyDecimal -> return (ILit TyDecimal)
            | otherwise -> mkFresh1 i "return" (S.fromList [TyInteger,TyDecimal])

funBinNum :: FunInferBin
funBinNum i (ILit x) (ILit y) = ILit <$> binNumCoercions i x y
funBinNum i (IVar vi n) (ILit x) = binNumOneVar i vi n x
funBinNum i (ILit x) (IVar vi n) = binNumOneVar i vi n x
funBinNum i IVar {} IVar {} = mkFresh1 i "return" (S.fromList [TyInteger,TyDecimal])
funBinNum i a b = die i $ "funBinNum: unsupported: " ++ show (a,b)

funKVBind :: Info -> FunInfer
funKVBind i [] = die i "Expected at least a binding in last arg position"
funKVBind _ as = return (last as)


setMono :: Info -> Type -> Info -> IName -> TC IVar
setMono i ty vi n = do
  vtys <- lookupVar vi n
  unless (not (S.null vtys) && ty `S.member` vtys)
          (die i $ "Typecheck failure: expected " ++ show ty ++ ", found " ++ show vtys ++ " for " ++ show n)
  isTypes %= M.insert n (S.singleton ty)
  return (ILit ty)

inferAppRef :: Info -> Ref -> [Term IVar] -> TC IVar
inferAppRef i (Direct (TNative dd (NativeDFun nn _) ni) ) as = do
  -- 1. interpret native to build up type information about args
  -- Note that this should also check type information already present to perhaps conclude already.
  -- 2. Return result type for use in other apps.
  as' <- mapM inferTerm as
  let binF fi tys = mkSpec ni dd [tys,tys] [] (funBin i fi)
      binMono = binF (funBinEquiv True)
      binEquivConstReturn tys rty = binF (\_ a b -> funBinEquiv False i a b >> return (ILit rty)) tys
      cmp = binEquivConstReturn [TyInteger,TyString,TyTime,TyDecimal] TyBool
      eq = binEquivConstReturn [TyInteger,TyString,TyTime,TyDecimal,TyBool,TyList,TyObject,TyKeySet] TyBool
      binNum = binF funBinNum [TyInteger,TyDecimal]
      timeM = mkSpec ni dd [[TyInteger,TyDecimal]] []  (funMono TyDecimal)
      endo ty = mkSpec ni dd [[ty]] []  (funMono ty)
      write = mkSpec ni dd [[TyString],[TyString],[TyObject]] []  (funMono TyString)
  case nn of
    "enforce" -> mkSpec ni dd [[],[TyString]] [] (funMono TyBool) >>= check i as'
    ">" -> cmp >>= check i as'
    "<" -> cmp >>= check i as'
    ">=" -> cmp  >>= check i as'
    "<=" -> cmp >>= check i as'
    "=" -> eq >>= check i as'
    "!=" -> eq >>= check i as'
    "+" -> binMono [TyString,TyInteger,TyDecimal,TyObject,TyList] >>= check i as'
    "-" -> binNum >>= check i as'
    "*" -> binNum >>= check i as'
    "/" -> binNum >>= check i as'
    "and" -> binMono [TyBool] >>= check i as'
    "or" -> binMono [TyBool] >>= check i as'
    "days" -> timeM >>= check i as'
    "add-time" -> mkSpec ni dd [[TyTime],[TyDecimal,TyInteger]] [] (funMono TyTime) >>= check i as'
    "is-string" -> endo TyString >>= check i as'
    "is-integer" -> endo TyInteger >>= check i as'
    "is-decimal" -> endo TyDecimal >>= check i as'
    "is-time" -> endo TyTime >>= check i as'
    "is-bool" -> endo TyBool >>= check i as'
    "insert" -> write >>= check i as'
    "update" -> write >>= check i as'
    "write" -> write >>= check i as'
    "format" -> mkSpec ni dd [[TyString],[]] [] (funMono TyString) >>= check i as'
    "with-default-read" -> mkSpec ni dd [[TyString],[TyString],[TyObject],[]] [] (funKVBind i) >>= check i as'
    "with-read" -> mkSpec ni dd [[TyString],[TyString],[]] [] (funKVBind i) >>= check i as'
    _ -> die i $ "Unspecified native: " ++ show nn
inferAppRef i (Direct t) _ = die i $ "inferAppRef: non-native ref: " ++ show t
inferAppRef i (Ref r) as = inferAppCode i r as

inferAppCode :: Info -> Term Ref -> [Term IVar] -> TC IVar
inferAppCode _ t@TDef {} as = do
  as' <- forM as inferTerm
  infer t (Just as')
inferAppCode i (TVar (Ref r) _) as = inferAppCode i r as
inferAppCode i t _ = die i $ "inferAppCode: unexpected: " ++ show t

inferBinding :: Info -> [(String,Term IVar)] -> Scope Int Term IVar -> BindCtx -> TC IVar
inferBinding i as bd bc = do
  args <- case bc of
    BindKV -> mkFresh i (map ((,def) . fst) as) -- TODO how to introspect on KV bindings??
    BindLet -> mapM (inferTerm . snd) as -- directly substitute values
  s <- traverseBound (bindArgs i args) bd
  inferBody i s


inferTerm :: Term IVar -> TC IVar
inferTerm t = case t of
    (TVar v _) -> return v
    (TApp v aas i) -> inferApp i v aas
    (TBinding bs bd c i) -> inferBinding i bs bd c
    (TLiteral l _) -> return $ ILit $ case l of
      LInteger {} -> TyInteger
      LBool {} -> TyBool
      LDecimal {} -> TyDecimal
      LTime {} -> TyTime
      LString {} -> TyString
    TObject {..} -> do
      forM_ _tObject $ \(k,v) -> inferTerm k >> inferTerm v
      return $ ILit TyObject
    TList {..} -> do
      forM_ _tList inferTerm
      return $ ILit TyList
    TKeySet {} -> return $ ILit TyKeySet
    TValue {} -> return $ ILit TyValue
    s -> return $ ILit (TySpecial (typeof s))


check :: Info -> [IVar] -> FunSpec -> TC IVar
check i args FunSpec {..}
  | length args < length _fsRequired =
      die i $ "check: Insufficient args for " ++ _fsName ++
              ", expected " ++ show (length _fsRequired) ++ ", found " ++ show (length args)
  | otherwise = do
      as' <- forM (zip args (_fsRequired ++ _fsOptional)) $ \(arg,spec) ->
        case arg of
          ILit ty -> typecheck i (S.singleton ty) spec >> return (ILit ty)
          IRef {} -> die i $ "Bad ref, or TODO check const: " ++ show arg
          IVar vi a -> do
            old <- use (isTypes.at a)
            case old of
              Nothing -> die vi $ "Untracked variable: " ++ show a
              Just tys -> do
                ttys <- typecheck i tys spec
                -- liftIO $ print (i,arg,ttys)
                isTypes %= M.insert a ttys -- substitute narrowed types
                return arg
      _fsInfer as'



typecheck :: Info -> S.Set Type -> ArgSpec -> TC (S.Set Type)
typecheck i tys (ArgSpec an atys)
  | null atys = return tys -- no spec information, use arg
  | null tys = return atys -- no type information, use spec
  | otherwise = case S.intersection tys atys of
      si | S.null si -> die i $ "Typecheck failure for arg " ++ an ++ ": expected " ++
                        show (S.toList atys) ++ ", found " ++ show (S.toList tys)
         | otherwise -> return si

lookupVar :: Info -> IName -> TC (S.Set Type)
lookupVar i a = do
  old <- use (isTypes.at a)
  case old of
    Nothing -> die i $ "Untracked variable: " ++ show a
    Just tys -> return tys

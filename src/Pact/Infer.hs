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
    , _isEqualities :: M.Map IName (S.Set IName)
    , _isFresh :: Int
    } deriving (Eq,Show)
makeLenses ''InferState
instance Default InferState where def = InferState def def 0

newtype TC a = TC { unTC :: StateT InferState IO a }
  deriving (Functor,Applicative,Monad,MonadState InferState,MonadIO,MonadThrow,MonadCatch)

runTC :: TC a -> IO (a, InferState)
runTC a = runStateT (unTC a) def

type Returns = [IVar] -> TC IVar

data FunSpec = FunSpec {
    _fsInfo :: Info
  , _fsName :: String
  , _fsRequired :: [ArgSpec]
  , _fsOptional :: [ArgSpec]
  , _fsEquivalences :: [[Int]]
  , _fsReturns :: Returns
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


_loadIssue :: IO (Term Ref)
_loadIssue = do
  (r,s) <- execScript' (Script "s") "examples/cp/cp.repl"
  either (die def) (const (return ())) r
  let (Just (Just (Ref d))) = firstOf (rEnv . eeRefStore . rsModules . at "cp" . _Just . at "issue") s
  return d

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


inferApp :: Info -> Term IVar -> [Term IVar] -> TC IVar
inferApp i (TVar (IRef d) _) as = inferAppRef i d as
inferApp i v _ = die i $ "inferApp: applying non-ref: " ++ show v

mkSpec :: Info -> DefData -> [[Type]] -> [[Type]] -> [[Int]] -> Returns -> TC FunSpec
mkSpec i dd@DefData {..} req opt equivs ret
  | length _dArgs /= length (req ++ opt) = die def $ "mkSpec: argnames do not match spec: " ++
                                           show (dd,req,opt)
  | otherwise = return $ FunSpec i _dName
                (zipWith ArgSpec _dArgs (map S.fromList req))
                (zipWith ArgSpec (drop (length req) _dArgs) (map S.fromList opt))
                equivs
                ret


returnMono :: Type -> Returns
returnMono ty = const (return (ILit ty))

returnBinMono :: Info -> Returns
returnBinMono _ [a@ILit {},_] = return a
returnBinMono _ [_,b@ILit {}] = return b
returnBinMono _ [a@IRef {},_] = return a -- assuming const here TODO
returnBinMono _ [_,b@IRef {}] = return b -- assuming const here TODO
returnBinMono i [IVar ai ats,IVar bi bts] = do
  a' <- lookupVar ai ats
  b' <- lookupVar bi bts
  case () of
    _ | S.size a' == 1 -> return $ ILit (head (S.toList a'))
      | S.size b' == 1 -> return $ ILit (head (S.toList b'))
      | otherwise -> do
          [f] <- mkFresh i [("return",S.intersection a' b')]
          return f

returnBinMono i as = die i $ "returnBinMono: Bad specification: more than two args: " ++ show as

inferAppRef :: Info -> Ref -> [Term IVar] -> TC IVar
inferAppRef i (Direct (TNative dd (NativeDFun nn _) ni) ) as = do
  -- 1. interpret native to build up type information about args
  -- Note that this should also check type information already present to perhaps conclude already.
  -- 2. Return result type for use in other apps.
  as' <- mapM inferTerm as
  let binMono tys = mkSpec ni dd [tys,tys] [] [[0,1]] (returnBinMono i)
      cmp = let tys = [TyInteger,TyString,TyTime,TyDecimal] in mkSpec ni dd [tys,tys] [] [[0,1]] (returnMono TyBool)
      binBool = binMono [TyBool]
      binNum = binMono [TyInteger,TyDecimal]
      timeM = mkSpec ni dd [[TyInteger,TyDecimal]] [] [] (returnMono TyDecimal)
      endo ty = mkSpec ni dd [[ty]] [] [] (returnMono ty)
      write = mkSpec ni dd [[TyString],[TyString],[TyObject]] [] [] (returnMono TyString)
  case nn of
    "enforce" -> mkSpec ni dd [[],[TyString]] [] [] (returnMono TyBool) >>= check i as'
    ">" -> cmp >>= check i as'
    "<" -> cmp >>= check i as'
    ">=" -> cmp  >>= check i as'
    "<=" -> cmp >>= check i as'
    "=" -> binMono [] >>= check i as'
    "!=" -> binMono [] >>= check i as'
    "+" -> binMono [TyString,TyInteger,TyDecimal,TyObject,TyList] >>= check i as'
    "-" -> binNum >>= check i as'
    "*" -> binNum >>= check i as'
    "/" -> binNum >>= check i as'
    "and" -> binBool >>= check i as'
    "or" -> binBool >>= check i as'
    "days" -> timeM >>= check i as'
    "add-time" -> mkSpec ni dd [[TyTime],[TyDecimal,TyInteger]] [] [] (returnMono TyTime) >>= check i as'
    "is-string" -> endo TyString >>= check i as'
    "is-integer" -> endo TyInteger >>= check i as'
    "is-decimal" -> endo TyDecimal >>= check i as'
    "is-time" -> endo TyTime >>= check i as'
    "is-bool" -> endo TyBool >>= check i as'
    "insert" -> write >>= check i as'
    "update" -> write >>= check i as'
    "write" -> write >>= check i as'
    "format" -> mkSpec ni dd [[TyString],[]] [] [] (returnMono TyString) >>= check i as'
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
    BindKV -> mkFresh i (map ((,def) . fst) as) -- no way to introspect on object bindings atm
    BindLet -> mapM (inferTerm . snd) as
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
    TKeySet {} -> return $ ILit TyKeySet
    TObject {} -> return $ ILit TyObject
    TList {} -> return $ ILit TyList
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
                liftIO $ print (i,arg,ttys)
                isTypes %= M.insert a ttys -- substitute narrowed types
                return arg
      as'' <- handleEquivs i as' _fsEquivalences
      _fsReturns as''

handleEquivs :: Info -> [IVar] -> [[Int]] -> TC [IVar]
handleEquivs _ args [] = return args
handleEquivs i args (eq:eqs) = return args -- TODO

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

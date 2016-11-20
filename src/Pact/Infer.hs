{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Pact.Infer where

import Pact.Repl
import Pact.Compile
import Pact.Eval
import Pact.Types
import Control.Monad.Catch
import Control.Lens
import Bound.Scope
import Bound
import Safe hiding (at)
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Maybe

die :: MonadThrow m => String -> m a
die = throwM . userError

type IName = (Int,String)


data Type =
    TyUnknown |
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
    deriving (Eq,Show)

data InferState = InferState {
      _isTypes :: M.Map IName (S.Set Type)
    , _isEqualities :: M.Map IName (S.Set IName)
    , _isNonce :: Int
    }
makeLenses ''InferState
instance Default InferState where def = InferState def def 0

type Returns = [IVar] -> IVar

data FunSpec = FunSpec {
    _fsName :: String
  , _fsRequired :: [ArgSpec]
  , _fsOptional :: [ArgSpec]
  , _fsEquivalences :: [[Int]]
  , _fsReturns :: Returns
  }

data ArgSpec = ArgSpec {
    _asName :: String
  , _asType :: [Type]
  } deriving (Eq,Show)

data IVar =
    IVar Info Int String |
    IRef Ref |
    ILit Type
    deriving (Eq,Show)


_loadAccounts :: IO (Term Ref)
_loadAccounts = do
  (r,s) <- execScript' (Script "s") "examples/cp/cp.repl"
  either die (const (return ())) r
  let (Just (Just (Ref d))) = firstOf (rEnv . eeRefStore . rsModules . at "cp" . _Just . at "issue") s
  return d

infer :: Term Ref -> Maybe [IVar] -> IO IVar
infer (TDef dd db _ i) vargs = do
  let args = fromMaybe (map (IVar i 0) (_dArgs dd)) vargs
  (TList ts _) <- instantiate (`TVar` def) <$>
        traverseScope (\b -> case args `atMay` b of
                               Nothing -> die $ "Missing arg: " ++ show b ++ ", " ++ show args
                               Just a -> return a)
                      (return . IRef) db
  print ts
  is <- forM ts inferTerm
  return (last is)
infer t _ = die $ "infer: non-def: " ++ show t


inferApp :: Info -> Term IVar -> [Term IVar] -> IO IVar
inferApp i (TVar (IRef d) _) as = inferAppRef i d as
inferApp i v _ = die $ "inferApp: applying non-ref: " ++ show v

mkSpec :: DefData -> [[Type]] -> [[Type]] -> [[Int]] -> Returns -> IO FunSpec
mkSpec dd@DefData {..} req opt equivs ret
  | length _dArgs /= length (req ++ opt) = die $ "mkSpec: argnames do not match spec: " ++
                                           show (dd,req,opt)
  | otherwise = return $ FunSpec _dName
                (zipWith ArgSpec _dArgs req)
                (zipWith ArgSpec (drop (length req) _dArgs) opt)
                equivs
                ret

inferAppRef :: Info -> Ref -> [Term IVar] -> IO IVar
inferAppRef i (Direct (TNative dd (NativeDFun nn _) _) ) as = do
  -- 1. interpret native to build up type information about args
  -- Note that this should also check type information already present to perhaps conclude already.
  -- 2. Return result type for use in other apps.
  as' <- mapM inferTerm as
  case nn of
    "enforce" -> mkSpec dd [[],[TyString]] [] [] (const (ILit $ TyBool)) >>= check as'

inferAppRef _ (Direct t) _ = die $ "inferAppRef: non-native ref: " ++ show t
inferAppRef i (Ref r) as = inferAppCode i r as

inferAppCode :: Info -> Term Ref -> [Term IVar] -> IO IVar
inferAppCode i t@TDef {} as = do
  as' <- forM as inferTerm
  infer t (Just as')
inferAppCode i (TVar (Ref r) _) as = inferAppCode i r as
inferAppCode _ t _ = die $ "inferAppCode: unexpected: " ++ show t

inferTerm :: Term IVar -> IO IVar
inferTerm t = case t of
    (TVar v@IVar {} _) -> return v
    (TApp v aas i) -> inferApp i v aas
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


check :: [IVar] -> FunSpec -> IO IVar
check args FunSpec {..}
  | length args < length _fsRequired = die $ "check: Insufficient args for " ++ _fsName ++
                                       ", expected " ++ show (length _fsRequired) ++ ", found " ++ show (length args)
  | otherwise = do
      as' <- forM (zip args (_fsRequired ++ _fsOptional)) $ \(arg,spec) ->
        case arg of
          ILit ty -> typecheck _fsName ty spec >> return arg
          IRef {} -> error "TODO check consts"
          IVar i a s -> error "TODO lookup var"
      return (_fsReturns as')

typecheck :: String -> Type -> ArgSpec -> IO ()
typecheck fn ty (ArgSpec an tys) | null tys = return ()
                                 | ty `elem` tys = return ()
                                 | ty == TyUnknown = return ()
                                 | otherwise = die $ "Typecheck failure in " ++ fn ++ "/" ++ an ++
                                               ", expected " ++ show tys ++ ", found " ++ show ty

{-# LANGUAGE DeriveTraversable #-}
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
import Data.List.NonEmpty (NonEmpty (..))
import Control.Arrow hiding ((<+>))
import Data.Aeson hiding (Object, (.=))
import Data.Foldable
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>),(<>))
import Data.String
import Data.Maybe
import Data.Monoid
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data CheckerException = CheckerException Info String deriving (Eq,Ord)

instance Exception CheckerException
instance Show CheckerException where show (CheckerException i s) = renderInfo i ++ ": " ++ s

die :: MonadThrow m => Info -> String -> m a
die i s = throwM $ CheckerException i s

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn

data UserType = UserType {
  _utName :: TypeName,
  _utModule :: ModuleName,
  _utFields :: [Arg UserType],
  _utInfo :: Info
  } deriving (Eq,Ord)
instance Show UserType where
  show UserType {..} = "{" ++ asString _utModule ++ "." ++ asString _utName ++ " " ++ show _utFields ++ "}"
instance Pretty UserType where
  pretty UserType {..} = braces (pretty _utModule <> dot <> pretty _utName)


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



data VarRole = ArgVar Int | RetVar
  deriving (Eq,Show,Ord)

data Overload = Overload { _oRole :: VarRole, _oOverApp :: TcId }
 deriving (Eq,Ord)

instance Show Overload where
  show (Overload r ts) = show ts ++ "?" ++ (case r of ArgVar i -> show i; RetVar -> "r")


data Types = Types {
  _tsPlain :: Type UserType,
  _tsOverloads :: [Overload]
  } deriving (Eq,Ord)

instance Show Types  where
  show (Types p []) = show p
  show (Types p os) = show p ++ " " ++ show os
instance Pretty Types where
  pretty (Types p []) = pretty p
  pretty (Types p os) = pretty p <+> sshow os



data TcState = TcState {
  _tcSupply :: Int,
  _tcOverloads :: M.Map TcId (FunTypes UserType),
  _tcFailures :: S.Set CheckerException,
  _tcAstToVar :: M.Map TcId (Type UserType),
  _tcVarToTypes :: M.Map (TypeVar UserType) Types
  } deriving (Eq,Show)

infixr 5 <$$>
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = (PP.<$>)

sshow :: Show a => a -> Doc
sshow = text . show

instance Default TcState where def = TcState 0 def def def def
instance Pretty TcState where
  pretty TcState {..} =
    "Overloads:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <> string "?" <+> colon <+>
                           align (vsep (map (string . show) (toList v)))) $ M.toList _tcOverloads) <$$>
    "AstToVar:" <$$>
    indent 2 (vsep (map (\(k,v) -> pretty k <> colon <+> pretty v) (M.toList _tcAstToVar))) <$$>
    "VarToTypes:" <$$>
    indent 2 (vsep $ map (\(k,v) -> sshow k <> colon <+> pretty v) $ M.toList _tcVarToTypes) <$$>
    "Failures:" <$$>
    indent 2 (vsep $ map (string.show) (toList _tcFailures))
    <> hardline


newtype TC a = TC { unTC :: StateT TcState IO a }
  deriving (Functor,Applicative,Monad,MonadState TcState,MonadIO,MonadThrow,MonadCatch)



makeLenses ''TcState
makeLenses ''Types


freshId :: Info -> String -> TC TcId
freshId i n = TcId i n <$> state (_tcSupply &&& over tcSupply succ)

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
    _fSpecial :: Maybe (SpecialForm,AST t)
    } |
  FDefun {
    _fInfo :: Info,
    _fName :: String,
    _fType :: FunType UserType,
    _fArgs :: [Named t],
    _fBody :: [AST t] }
  deriving (Eq,Functor,Foldable,Show)

instance Pretty t => Pretty (Fun t) where
  pretty FNative {..} = "(native " <> text _fName <$$>
    indent 2 ("::" <+> align (vsep (map pretty (toList _fTypes)))) <>
      (case _fSpecial of
         Nothing -> mempty
         Just (_,bod) -> mempty <$$> indent 2 (pretty bod)) <$$>
      ")"
  pretty FDefun {..} = "(defun " <> text _fName <$$>
    indent 2 ("::" <+> pretty _fType) <$$>
    indent 2 ("(" <$$>
              indent 2 (vsep (map pretty _fArgs)) <$$> ")") <$$>
    indent 2 (vsep (map pretty _fBody)) <$$>
    ")"

data Node = Node {
  _aId :: TcId,
  _aTy :: Type UserType
  } deriving (Eq,Ord)
instance Show Node where
  show (Node i t) = show i ++ "::" ++ show t
instance Pretty Node where
  pretty (Node i t) = pretty i <> "::" <> pretty t

data Named i = Named {
  _nnName :: String,
  _nnNamed :: i
  } deriving (Eq,Ord,Functor,Foldable,Traversable)
instance (Show i) => Show (Named i) where
  show (Named na no) = show na ++ "(" ++ show no ++ ")"
instance (Pretty i) => Pretty (Named i) where pretty (Named na no) = dquotes (pretty na) <+> parens (pretty no)


data AST n =
  App {
  _aNode :: n,
  _aAppFun :: Fun n,
  _aAppArgs :: [AST n]
  } |
  Binding {
  _aNode :: n,
  _aBindings :: [(Named n,AST n)],
  _aBody :: [AST n],
  _aBindType :: BindType n
  } |
  List {
  _aNode :: n,
  _aList :: [AST n]
  } |
  Object {
  _aNode :: n,
  _aObject :: [(AST n,AST n)]
  } |
  Prim {
  _aNode :: n,
  _aPrimValue :: PrimValue
  } |
  Var {
  _aNode :: n
  } |
  Table {
  _aNode :: n
  }

  deriving (Eq,Functor,Foldable,Show)

instance Pretty t => Pretty (AST t) where
  pretty a = case a of
     Prim {..} -> pn <+> equals <+> pretty _aPrimValue
     Var {..} -> pn
     Object {..} -> pn <$$> "{" <$$>
       indent 2 (vsep (map (\(k,v) -> pretty k <> text ":" <$$> indent 4 (pretty v)) _aObject)) <$$>
       "}"
     List {..} -> pn <$$> "[" <$$> indent 2 (vsep (map pretty _aList)) <$$> "]"
     Binding {..} -> pn <$$> "(" <> pretty _aBindType <$$>
       indent 2 (vsep (map (\(k,v) ->
                              "(" <$$>
                              indent 2 (pretty k <+> colon <$$>
                                        indent 2 (pretty v)) <$$>
                              ")" ) _aBindings)) <$$>
       indent 2 (vsep (map pretty _aBody)) <$$> ")"
     App {..} -> pn <$$>
       indent 2 ("(" <$$> indent 2 (vsep (map pretty _aAppArgs)) <$$> ")") <$$>
       indent 2 (pretty _aAppFun)
     Table {..} -> pn
   where pn = pretty (_aNode a)



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
  t' <- Object _aNode <$>
         forM _aObject (\(k,v) -> (,) <$> walkAST f k <*> walkAST f v)
  f Post t'
walkAST f t@List {} = do
  List {..} <- f Pre t
  t' <- List _aNode <$> mapM (walkAST f) _aList
  f Post t'
walkAST f t@Binding {} = do
  Binding {..} <- f Pre t
  t' <- Binding _aNode <$>
        forM _aBindings (\(k,v) -> (k,) <$> walkAST f v) <*>
        mapM (walkAST f) _aBody <*> pure _aBindType
  f Post t'
walkAST f t@App {} = do
  App {..} <- f Pre t
  t' <- App _aNode <$>
        (case _aAppFun of
           fun@FNative {..} -> case _fSpecial of
             Nothing -> return fun
             Just (fs,bod) -> do
               bod' <- walkAST f bod
               return (set fSpecial (Just (fs,bod')) fun)
           fun@FDefun {..} -> do
             db <- mapM (walkAST f) _fBody
             return $ set fBody db fun
        ) <*>
        mapM (walkAST f) _aAppArgs
  f Post t'

isConcreteTy :: Type n -> Bool
isConcreteTy ty = not (isAnyTy ty || isVarTy ty)

data SolveOverload o = SO {
  _soOverload :: o,
  _soRoles :: M.Map VarRole (TypeVar UserType),
  _soSolution :: Maybe (FunType UserType)
  } deriving (Eq,Show,Functor,Foldable,Traversable)


solveOverloads :: TC ()
solveOverloads = do
  vts <- M.toList <$> use tcVarToTypes
  let (edges :: [(TypeVar UserType,Overload)]) =
        (`concatMap` vts) $ \(v,Types _ os) -> map (v,) os
      (omap1 :: M.Map TcId [SolveOverload TcId]) =
        M.fromListWith (++) $ (`map` edges) $ \(v,Overload r oid) ->
        (oid,[SO oid (M.singleton r v) Nothing])
  omap :: M.Map TcId (SolveOverload (FunTypes UserType)) <- forM omap1 $ \sos -> do
    let sor = foldl1 (\(SO a b c) (SO _ e _) -> SO a (M.union b e) c) sos
    unless (length sos == M.size (_soRoles sor)) $
      die def $ "Role conflict in overloads: " ++ show sos
    forM sor $ \oid -> use tcOverloads >>= \m -> case M.lookup oid m of
        Just ft -> return ft
        Nothing -> die def $ "Bad overload, could not deref id: " ++ show oid
  let runSolve os = forM os $ \o@(SO fts roles sol) -> case sol of
        Just _solved -> return o
        Nothing -> SO fts roles <$> foldM (tryFunType roles) Nothing fts
      rptSolve os = runSolve os >>= \os' -> if os' == os then return os' else rptSolve os'
  done <- rptSolve omap
  if all (isJust . _soSolution) (M.elems done)
    then debug "Success!"
    else debug $ "Boo!" ++ show (filter (isNothing . _soSolution) (M.elems done))


tryFunType :: M.Map VarRole (TypeVar UserType) -> Maybe (FunType UserType) -> FunType UserType ->
               TC (Maybe (FunType UserType))
tryFunType _ r@Just {} _ = return r
tryFunType roles _ f@(FunType as rt) = do
  let tryRole rol fty = case M.lookup rol roles of
        Nothing -> return Nothing
        Just tv -> use tcVarToTypes >>= \m -> case M.lookup tv m of
          Nothing -> die def $ "Bad var in funtype solver: " ++ show tv
          Just (Types ty _) -> case unifyTypes fty ty of
            Nothing -> return Nothing
            Just _ -> return (Just (fty,[(tv,ty)]))
  subAsM <- forM (zip as [0..]) $ \(Arg _ fty _,i) -> tryRole (ArgVar i) fty
  subRolesM <- fmap (M.fromListWith (++)) . sequence . (:subAsM) <$> tryRole RetVar rt
  case subRolesM of
    Nothing -> return Nothing
    Just subRoles -> do
      let solvedM = sequence $ (`map` M.toList subRoles) $ \(fty,tvTys) ->
            let tys = foldl1 unifyM $ (Just fty:) $ map (Just . snd) tvTys
                unifyM (Just a) (Just b) = either id id <$> unifyTypes a b
                unifyM _ _ = Nothing
            in case tys of
              Nothing -> Nothing
              Just uty -> Just (fty,(uty,map fst tvTys))
          allConcrete = all isConcreteTy . map (fst . snd)
      case solvedM of
        Nothing -> return Nothing
        Just solved | allConcrete solved -> do
                        debug $ "Solved overload with " ++ show f ++ ": " ++ show solved
                        forM_ solved $ \(_,(uty,tvs)) -> forM_ tvs $ \tv ->
                          tcVarToTypes %= M.adjust (set tsPlain uty) tv
                        return $ Just f
                    | otherwise -> return Nothing


enforceSchemas :: Visitor TC Node
enforceSchemas Pre ast = case ast of
  (Object n ps) -> findSchema n $ \sch -> do
    debug $ "enforceSchemas: " ++ show (n,sch)
    return ast
  (Binding _ bs _ (BindSchema n)) -> findSchema n $ \sch -> do
    debug $ "enforceSchemas: " ++ show (n,sch)
    return ast
  _ -> return ast
  where
    findSchema n act = do
      ty <- resolveTy =<< lookupAst "enforceSchemas" (_aId n)
      case ty of
        (TySchema _ (TyUser sch)) -> act sch
        _ -> return ast
enforceSchemas Post a = return a

-- | Native funs get processed on their own walk.
-- 'assocAST' associates the app arg's ID with the fun ty.
processNatives :: Visitor TC Node
processNatives Pre a@(App i FNative {..} as) = do
  case _fTypes of
    -- single funtype
    ft@FunType {} :| [] -> do
      let FunType {..} = mangleFunType (_aId i) ft
      zipWithM_ (\(Arg _ t _) aa -> assocTy (_aNode aa) t) _ftArgs as
      assocTy i _ftReturn
      -- the following assumes that special forms are never overloaded!
      case _fSpecial of
        -- with-read et al have a single Binding body, associate this with return type
        Just (_,Binding bn _ _ (BindSchema sn)) -> do
          assocTy bn _ftReturn
          -- assoc schema with last ft arg
          assocTy sn (_aType (last (toList _ftArgs)))
        Just sf -> die _fInfo $ "Invalid special form: " ++ show sf
        _ -> return ()
    -- multiple funtypes
    fts -> do
      let fts' = fmap (mangleFunType (_aId i)) fts
      tcOverloads %= M.insert (_aId i) fts'
      zipWithM_ (\ai aa -> assocOverload (_aId (_aNode aa))
                  (Overload (ArgVar ai) (_aId i))) [0..] as -- this assoc's the funty with the app ty.
      assocOverload (_aId i) (Overload RetVar (_aId i))
  return a
processNatives _ a = return a

-- | Walk to substitute app args into vars for FDefuns
-- 'assocAST' associates the defun's arg with the app arg type.
substAppDefun :: Maybe (TcId, AST Node) -> Visitor TC Node
substAppDefun sub Pre t@Var {..} = case sub of
    Nothing -> return t
    Just (defArg,appAst)
      | defArg == _aId _aNode -> assocAST defArg appAst >> return appAst
      | otherwise -> return t
substAppDefun _ Post App {..} = do -- Post, to allow args to get substituted out first.
    af <- case _aAppFun of
      f@FNative {} -> return f
      f@FDefun {..} -> do
        fb' <- forM _fBody $ \bAst ->
          foldM (\b fa -> walkAST (substAppDefun (Just fa)) b) bAst (zip (map (_aId . _nnNamed) _fArgs) _aAppArgs) -- this zip might need a typecheck
        return $ set fBody fb' f
    return (App _aNode af _aAppArgs)
substAppDefun _ _ t = return t


trackAST :: Node -> TC ()
trackAST (Node i t) = do
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

addFailure :: TcId -> String -> TC ()
addFailure i s = do
  debug $ "Failure: " ++ show (i,s)
  tcFailures %= S.insert (CheckerException (_tiInfo i) s)

lookupAst :: String -> TcId -> TC (Type UserType)
lookupAst msg i = maybe (die' i $ msg ++ ": ast not already tracked: " ++ show i) return =<<
                  (M.lookup i <$> use tcAstToVar)

-- | Track type to id with typechecking
assocTy :: Node -> Type UserType -> TC ()
assocTy (Node ai _) ty = do
  aty <- lookupAst "assocTy" ai
  (avm,atysm) <- case aty of
    TyVar tv -> (Just tv,) . M.lookup tv <$> use tcVarToTypes
    _ -> return (Nothing,Nothing)
  debug $ "assocTy: " ++ show (ai,aty,ty)
  case unifyTypes aty ty of
    Nothing -> addFailure ai $ "assocTy: cannot unify: " ++ show (ai,aty,ty)
    Just (Left _same) -> do
      debug ("assocTy: noop: " ++ show (ai,aty,ty))
      assocParams ty aty
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

-- | Track type to id with typechecking
assocOverload :: TcId -> Overload -> TC ()
assocOverload ai o = do
  aty <- lookupAst "assocTy" ai
  (avm,atysm) <- case aty of
    TyVar tv -> (Just tv,) . M.lookup tv <$> use tcVarToTypes
    _ -> return (Nothing,Nothing)
  debug $ "assocOverload: " ++ show (ai,aty,o)
  case (avm,atysm) of
    (Just v,Just tys) -> do
      debug ("assocTy: associating " ++ show o ++ " with " ++ show (ai,v,tys))
      tcVarToTypes %= M.insert v (over tsOverloads (o:) tys)
    _ | isConcreteTy aty -> do
          debug ("assocTy: associating " ++ show o ++ " with concrete ty/id " ++ show (ai,aty))
          alterTypes (TypeVar (fromString (show ai)) []) (Types aty [o]) (over tsOverloads (o:))
      | otherwise -> die' ai $ "assocTy: cannot track overload, not a var or not tracked: " ++ show (ai,aty,atysm,o)

alterTypes :: TypeVar UserType -> Types -> (Types -> Types) -> TC ()
alterTypes v newVal upd = tcVarToTypes %= M.alter (Just . maybe newVal upd) v

updateTyVar :: Type UserType -> Type UserType -> TC ()
updateTyVar (TyVar uv) u = do
  debug $ "updateTyVar: " ++ show (uv,u)
  alterTypes uv (Types u []) (set tsPlain u)
updateTyVar _ _ = return ()

assocParams :: Type UserType -> Type UserType -> TC ()
assocParams x y = case (x,y) of
  _ | x == y -> return ()
  (TySchema _ a,TySchema _ b) -> assoc a b
  (TyList a,TyList b) -> assoc a b
  _ -> return ()
  where
    assoc a@TyVar {} b | b /= TyAny = updateTyVar a b
    assoc a b@TyVar {} | a /= TyAny = updateTyVar b a
    assoc _ _ = return ()

-- | Track ast type to id with typechecking
assocAST :: TcId -> AST Node -> TC ()
assocAST ai b = do
  let bi = _aId (_aNode b)
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


scopeToBody :: Info -> [AST Node] -> Scope Int Term (Either Ref (AST Node)) -> TC [AST Node]
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

toFun :: Term (Either Ref (AST Node)) -> TC (Fun Node)
toFun (TVar (Left (Direct TNative {..})) _) = do
  ft' <- traverse (traverse toUserType') _tFunTypes
  let special = (,Var (Node (TcId def "temp" 0) TyAny)) <$> isSpecialForm _tNativeName
  return $ FNative _tInfo (asString _tNativeName) ft' special
toFun (TVar (Left (Ref r)) _) = toFun (fmap Left r)
toFun (TVar Right {} i) = die i "Value in fun position"
toFun TDef {..} = do -- TODO currently creating new vars every time, is this ideal?
  let fn = asString _tModule ++ "." ++ asString _tDefName
  args <- forM (_ftArgs _tFunType) $ \(Arg n t ai) -> do
    an <- freshId ai $ pfx fn n
    t' <- mangleType an <$> traverse toUserType t
    Named n <$> trackNode t' an
  tcs <- scopeToBody _tInfo (map (\ai -> Var (_nnNamed ai)) args) _tDefBody
  ft' <- traverse toUserType _tFunType
  return $ FDefun _tInfo fn ft' args tcs
toFun t = die (_tInfo t) "Non-var in fun position"

notEmpty :: MonadThrow m => Info -> String -> [a] -> m [a]
notEmpty i msg [] = die i msg
notEmpty _ _ as = return as

toAST :: Term (Either Ref (AST Node)) -> TC (AST Node)
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
  n <- trackNode (idTyVar i) i
  as <- mapM toAST _tAppArgs
  (as',fun') <- case fun of
    FDefun {..} -> assocAST i (last _fBody) >> return (as,fun) -- non-empty verified in 'scopeToBody'
    FNative {..} -> case _fSpecial of
      Nothing -> return (as,fun)
      Just (f,_) -> (,) <$> notEmpty _tInfo "Expected >1 arg" (init as)
                    <*> pure (set fSpecial (Just (f,last as)) fun)
  return $ App n fun' as'

toAST TBinding {..} = do
  bi <- freshId _tInfo (show _tBindType)
  bn <- trackNode (idTyVar bi) bi
  bs <- forM _tBindPairs $ \(Arg n t ai,v) -> do
    aid <- freshId ai (pfx (show bi) n)
    t' <- mangleType aid <$> traverse toUserType t
    an <- trackNode t' aid
    v' <- toAST v
    case _tBindType of
      BindLet -> do
        assocAST aid v'
        return (Named n an,v')
      BindSchema _ -> do
        fieldName <- case v' of
          (Prim _ (PrimLit (LString f))) -> return f
          _ -> die ai $ "Bad binding, non-string field name: " ++ show v'
        return (Named fieldName an,Var an)
  bb <- scopeToBody _tInfo (map ((\ai -> Var (_nnNamed ai)).fst) bs) _tBindBody
  bt <- case _tBindType of
    BindLet -> assocAST bi (last bb) >> return BindLet
    BindSchema sty -> do
      sty' <- mangleType bi <$> traverse toUserType sty
      sn <- trackNode sty' =<< freshId _tInfo (show bi ++ "schema")
      return $ BindSchema sn
  return $ Binding bn bs bb bt

toAST TList {..} = do
  ty <- TyList <$> traverse toUserType _tListType
  List <$> (trackNode ty =<< freshId _tInfo "list") <*> mapM toAST _tList
toAST TObject {..} = do
  debug $ "TObject: " ++ show _tUserType
  ty <- TySchema TyObject <$> traverse toUserType _tUserType
  Object <$> (trackNode ty =<< freshId _tInfo "object")
    <*> mapM (\(k,v) -> (,) <$> toAST k <*> toAST v) _tObject
toAST TConst {..} = toAST _tConstVal -- TODO typecheck here
toAST TKeySet {..} = trackPrim _tInfo TyKeySet (PrimKeySet _tKeySet)
toAST TValue {..} = trackPrim _tInfo TyValue (PrimValue _tValue)
toAST TLiteral {..} = trackPrim _tInfo (litToPrim _tLiteral) (PrimLit _tLiteral)
toAST TTable {..} = do
  debug $ "TTable: " ++ show _tTableType
  ty <- TySchema TyTable <$> traverse toUserType _tTableType
  Table <$> (trackNode ty =<< freshId _tInfo (asString _tModule ++ "." ++ asString _tTableName))
toAST TModule {..} = die _tInfo "Modules not supported"
toAST TUse {..} = die _tInfo "Use not supported"
toAST TStep {..} = die _tInfo "TODO steps/pacts"

trackPrim :: Info -> PrimType -> PrimValue -> TC (AST Node)
trackPrim inf pty v = do
  let ty :: Type UserType = TyPrim pty
  Prim <$> (trackNode ty =<< freshId inf (show ty) ) <*> pure v

trackNode :: Type UserType -> TcId -> TC Node
trackNode ty i = trackAST node >> return node
  where node = Node i ty

toUserType :: Term (Either Ref (AST Node)) -> TC UserType
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


infer :: Term Ref -> TC (Fun Node)
infer t@TDef {..} = toFun (fmap Left t)
infer t = die (_tInfo t) "Non-def"


resolveTy :: Type UserType -> TC (Type UserType)
resolveTy tv@(TyVar v) = use tcVarToTypes >>= \m -> case M.lookup v m of
      Just (Types t _) -> resolveTy t
      Nothing -> return tv
resolveTy (TySchema s st) = TySchema s <$> resolveTy st
resolveTy (TyList l) = TyList <$> resolveTy l
resolveTy t = return t

isUnresolvedTy :: Type n -> Bool
isUnresolvedTy TyVar {} = True
isUnresolvedTy (TySchema _ v) = isUnresolvedTy v
isUnresolvedTy (TyList l) = isUnresolvedTy l
isUnresolvedTy _ = False -- TODO fun types

prettyMap :: (t -> Doc) -> (t1 -> Doc) -> M.Map t t1 -> Doc
prettyMap prettyK prettyV = vsep . map (\(k,v) -> prettyK k <> colon <+> prettyV v) . M.toList

resolveAllTypes :: TC (M.Map TcId (Type UserType))
resolveAllTypes = do
  ast2Ty <- use tcAstToVar >>= \a2v -> forM a2v resolveTy
  tcAstToVar .= ast2Ty
  let unresolved = M.filter isUnresolvedTy ast2Ty
  unless (M.null unresolved) $ do
    debug "Unable to resolve all types"
    liftIO $ putDoc (indent 2 (prettyMap pretty pretty unresolved))
  return ast2Ty

_debugState :: TC ()
_debugState = liftIO . putDoc . pretty =<< get


substFun :: Fun Node -> TC (Fun Node)
substFun FNative {} = error "Native TODO"
substFun f@FDefun {..} = do
  debug "Transform"
  appSub <- mapM (walkAST $ substAppDefun Nothing) _fBody
  debug "Substitution"
  nativesProc <- mapM (walkAST processNatives) appSub
  debug "Solve Overloads"
  solveOverloads
  schEnforced <- mapM (walkAST enforceSchemas) nativesProc
  ast2Ty <- resolveAllTypes
  return $ (\(Node i _) -> Node i (ast2Ty M.! i)) <$> set fBody schEnforced f


_loadFun :: FilePath -> ModuleName -> String -> IO (Term Ref)
_loadFun fp mn fn = do
  (r,s) <- execScript' (Script fp) fp
  either (die def) (const (return ())) r
  let (Just (Just (Ref d))) = firstOf (rEnv . eeRefStore . rsModules . at mn . _Just . _2 . at fn) s
  return d

_infer :: FilePath -> ModuleName -> String -> IO (Fun Node, TcState)
_infer fp mn fn = _loadFun fp mn fn >>= \d -> runTC (infer d >>= substFun)

-- _pretty =<< _inferIssue
_inferIssue :: IO (Fun Node, TcState)
_inferIssue = _infer "examples/cp/cp.repl" "cp" "issue"

-- _pretty =<< _inferTransfer
_inferTransfer :: IO (Fun Node, TcState)
_inferTransfer = _infer "examples/accounts/accounts.repl" "accounts" "transfer"

-- prettify output of '_infer' runs
_pretty :: (Fun Node, TcState) -> IO ()
_pretty (f,tc) = putDoc (pretty tc <> hardline <> hardline <> pretty f <> hardline)

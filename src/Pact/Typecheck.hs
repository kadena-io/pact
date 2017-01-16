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
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import Control.Arrow hiding ((<+>))
import Data.Aeson hiding (Object, (.=))
import Data.Foldable
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>),(<>))
import Data.String
import Data.Monoid
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data CheckerException = CheckerException Info String deriving (Eq,Ord)

instance Exception CheckerException
instance Show CheckerException where show (CheckerException i s) = renderInfo i ++ ": " ++ s

die :: MonadThrow m => Info -> String -> m a
die i s = throwM $ CheckerException i s


data UserType = Schema {
  _utName :: TypeName,
  _utModule :: ModuleName,
  _utFields :: [Arg UserType],
  _utInfo :: Info
  } deriving (Eq,Ord)
instance Show UserType where
  show Schema {..} = "{" ++ asString _utModule ++ "." ++ asString _utName ++ " " ++ show _utFields ++ "}"
instance Pretty UserType where
  pretty Schema {..} = braces (pretty _utModule <> dot <> pretty _utName)


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
  _tsType :: Type UserType,
  _tsOverloads :: [Overload]
  } deriving (Eq,Ord)

instance Show Types  where
  show (Types p []) = show p
  show (Types p os) = show p ++ " " ++ show os
instance Pretty Types where
  pretty (Types p []) = pretty p
  pretty (Types p os) = pretty p <+> sshow os

type Failures = S.Set CheckerException

data TcState = TcState {
  _doDebug :: Bool,
  _tcSupply :: Int,
  _tcOverloads :: M.Map TcId (Either (FunTypes UserType) (FunType UserType)),
  _tcFailures :: Failures,
  _tcAstToVar :: M.Map TcId (TypeVar UserType),
  _tcVarToTypes :: M.Map (TypeVar UserType) Types
  } deriving (Eq,Show)

infixr 5 <$$>
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = (PP.<$>)

sshow :: Show a => a -> Doc
sshow = text . show

mkTcState :: Int -> Bool -> TcState
mkTcState sup dbg = TcState dbg sup def def def def

instance Pretty TcState where
  pretty TcState {..} =
    "Overloads:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <> string "?" <+> colon <+>
                           align (vsep (map (string . show) (toList v)))) $ M.toList _tcOverloads) <$$>
    "AstToVar:" <$$>
    indent 2 (vsep (map (\(k,v) -> pretty k <> colon <+> pretty v) (M.toList _tcAstToVar))) <$$>
    "VarToTypes:" <$$>
    indent 2 (vsep $ map (\(k,v) -> sshow k <> colon <+> pretty v) $ M.toList _tcVarToTypes) <$$>
    prettyFails _tcFailures
    <> hardline

prettyFails :: Failures -> Doc
prettyFails fs = "Failures:" <$$>
    indent 2 (vsep $ map (string.show) (toList fs))


newtype TC a = TC { unTC :: StateT TcState IO a }
  deriving (Functor,Applicative,Monad,MonadState TcState,MonadIO,MonadThrow,MonadCatch)



makeLenses ''TcState
makeLenses ''Types



debug :: String -> TC ()
debug s = use doDebug >>= \d -> debug' d s

debug' :: MonadIO m => Bool -> String -> m ()
debug' d s = when d (liftIO $ putStrLn s)


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


data TopLevel t =
  TopFun {
    _tlFun :: Fun t
    } |
  TopConst {
    _tlInfo :: Info,
    _tlName :: String,
    _tlType :: Type UserType,
    _tlConstVal :: AST t
    } |
  TopTable {
    _tlInfo :: Info,
    _tlName :: String,
    _tlType :: Type UserType
  } |
  TopUserType {
    _tlInfo :: Info,
    _tlUserType :: UserType
  }
  deriving (Eq,Functor,Foldable,Traversable,Show)
instance Pretty t => Pretty (TopLevel t) where
  pretty (TopFun f) = "Fun" <$$> pretty f
  pretty (TopConst _i n t v) =
    "Const" <+> pretty n <> colon <> pretty t <$$>
    indent 2 (pretty v)
  pretty (TopTable _i n t) =
    "Table" <+> pretty n <> colon <> pretty t
  pretty (TopUserType _i t) = "UserType" <+> pretty t

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
  deriving (Eq,Functor,Foldable,Traversable,Show)

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
  } |
  Step {
  _aNode :: n,
  _aEntity :: AST n,
  _aExec :: AST n,
  _aRollback :: Maybe (AST n)
  }

  deriving (Eq,Functor,Foldable,Traversable,Show)

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
     Step {..} ->
       let rb = case _aRollback of
                  Nothing -> (<> empty)
                  Just r -> (<$$> "Rollback:" <$$> indent 2 (pretty r))
       in rb (pn <$$> indent 2 ("Entity" <> colon <+> pretty _aEntity) <$$>
              indent 2 (pretty _aExec))
   where pn = pretty (_aNode a)



makeLenses ''AST
makeLenses ''Fun
makeLenses ''TopLevel

runTC :: Int -> Bool -> TC a -> IO (a, TcState)
runTC sup dbg a = runStateT (unTC a) (mkTcState sup dbg)


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
walkAST f t@Step {} = do
  Step {..} <- f Pre t
  t' <- Step _aNode <$> walkAST f _aEntity <*> walkAST f _aExec <*> traverse (walkAST f) _aRollback
  f Post t'

isConcreteTy :: Type n -> Bool
isConcreteTy ty = not (isAnyTy ty || isVarTy ty)

data SolveOverload o = SO {
  _soOverload :: o,
  _soRoles :: M.Map VarRole (TypeVar UserType),
  _soSolution :: Maybe (FunType UserType)
  } deriving (Eq,Ord,Show,Functor,Foldable,Traversable)
makeLenses ''SolveOverload

solveOverloads :: TC ()
solveOverloads = do

  vts <- M.toList <$> use tcVarToTypes

  let edges :: [(TypeVar UserType,Overload)]
      edges = (`concatMap` vts) $ \(v,Types _ os) -> map (v,) os

      omap1 :: M.Map TcId (S.Set (SolveOverload TcId))
      omap1 = M.fromListWith mappend $ (`map` edges) $ \(v,Overload r oid) ->
                (oid,S.singleton $ SO oid (M.singleton r v) Nothing)

  omap :: [(TcId,SolveOverload (FunTypes UserType))] <-
    fmap concat $ forM (M.toList omap1) $ \(i,sos) -> do
    let sor = foldl1 (\(SO a b c) (SO _ e _) -> SO a (M.union b e) c) sos
    unless (length sos == M.size (_soRoles sor)) $
      die' i $ "Role conflict in overloads: " ++ show sos
    use tcOverloads >>= \m -> case M.lookup (_soOverload sor) m of
        Just (Left fts) -> return [(i,set soOverload fts sor)]
        Just (Right _ft) -> return [] -- already solved
        Nothing -> die def $ "Bad overload, could not deref id: " ++ show sor
  let runSolve os = forM os $ \(i,o@(SO fts roles sol)) -> case sol of
        Just _solved -> return (i,o)
        Nothing -> ((i,) . SO fts roles) <$> foldM (tryFunType i roles) Nothing fts
      rptSolve os = runSolve os >>= \os' -> if os' == os then return os' else rptSolve os'
  done <- rptSolve omap
  forM_ done $ \(i,o) -> case _soSolution o of
      Nothing -> do
        debug $ "Unable to solve overload: " ++ show (i,o)
        addFailure i $ "Unable to solve overloaded type: " ++ show (_soOverload o)
      Just ft -> tcOverloads %= M.insert i (Right ft)


tryFunType :: TcId -> M.Map VarRole (TypeVar UserType) -> Maybe (FunType UserType) -> FunType UserType ->
               TC (Maybe (FunType UserType))
tryFunType _ _ r@Just {} _ = return r
tryFunType i roles _ f@(FunType as rt) = do
  let tryRole rol fty = case M.lookup rol roles of
        Nothing -> return Nothing
        Just tv -> use tcVarToTypes >>= \m -> case M.lookup tv m of
          Nothing -> die def $ "Bad var in funtype solver: " ++ show tv
          Just (Types ty _) -> case unifyTypes fty ty of
            Nothing -> return Nothing
            Just _ -> return (Just (fty,[(tv,ty)]))
  subAsM <- forM (zip as [0..]) $ \(Arg _ fty _,ai) -> tryRole (ArgVar ai) fty
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
                        forM_ solved $ \(fty,(uty,tvs)) -> forM_ tvs $ \tv -> do
                          debug $ "Adjusting type for solution: " ++ show (tv,fty,uty)
                          assocTy' i tv fty
                          assocTy' i tv uty
                        return $ Just f
                    | otherwise -> return Nothing

asPrimString :: AST Node -> TC String
asPrimString (Prim _ (PrimLit (LString s))) = return s
asPrimString t = die (_tiInfo (_aId (_aNode t))) $ "Expected literal string: " ++ show t

applySchemas :: Visitor TC Node
applySchemas Pre ast = case ast of
  (Object n ps) -> findSchema n $ \sch -> do
    debug $ "applySchemas: " ++ show (n,sch)
    pmap <- M.fromList <$> forM ps
      (\(k,v) -> (,) <$> asPrimString k <*> ((v,) <$> lookupTy (_aNode v)))
    validateSchema sch pmap
    return ast
  (Binding _ bs _ (BindSchema n)) -> findSchema n $ \sch -> do
    debug $ "applySchemas: " ++ show (n,sch)
    pmap <- M.fromList <$> forM bs
      (\(Named bn _,v) -> (bn,) <$> ((v,) <$> lookupTy (_aNode v)))
    validateSchema sch pmap
    return ast
  _ -> return ast
  where
    validateSchema sch pmap = do
      let smap = M.fromList <$> (`map` _utFields sch) $ \(Arg an aty _) -> (an,aty)
      forM_ (M.toList pmap) $ \(k,(v,vty)) -> case M.lookup k smap of
        Nothing -> addFailure (_aId (_aNode v)) $ "Invalid field in schema object: " ++ show k
        Just aty -> case unifyTypes aty vty of
          Nothing -> addFailure (_aId (_aNode v)) $ "Unable to unify field type: " ++ show (k,aty,vty,v)
          Just u -> assocTy (_aNode v) (either id id u)
    lookupTy a = resolveTy =<< ((_tsType . snd) <$> lookupAst "lookupTy" (_aId a))
    findSchema n act = do
      ty <- lookupTy n
      case ty of
        (TySchema _ (TyUser sch)) -> act sch
        _ -> return ast
applySchemas Post a = return a

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
          debug "hi"
          assocTy bn _ftReturn
          -- assoc schema with last ft arg
          debug "hello"
          assocTy sn (_aType (last (toList _ftArgs)))
        Just sf -> die _fInfo $ "Invalid special form: " ++ show sf
        _ -> return ()
    -- multiple funtypes
    fts -> do
      let fts' = fmap (mangleFunType (_aId i)) fts
      tcOverloads %= M.insert (_aId i) (Left fts')
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

-- | Track AST as a TypeVar pointing to a Types. If the provided node type is already a var use that,
-- otherwise make a new var based on the TcId.
trackAST :: Node -> TC ()
trackAST (Node i t) = do
  debug $ "trackAST: " ++ show (i,t)
  maybe (return ()) (const (die' i $ "trackAST: ast already tracked: " ++ show (i,t)))
    =<< (M.lookup i <$> use tcAstToVar)
  let v = case t of
        (TyVar tv) -> tv
        _ -> TypeVar (fromString (show i)) []
  tcAstToVar %= M.insert i v
  maybe (return ()) (const (die' i $ "trackAST: var already tracked: " ++ show (i,t)))
    =<< (M.lookup v <$> use tcVarToTypes)
  tcVarToTypes %= M.insert v (Types t [])


addFailure :: TcId -> String -> TC ()
addFailure i s = do
  debug $ "Failure: " ++ show (i,s)
  tcFailures %= S.insert (CheckerException (_tiInfo i) s)

-- | Lookup both type var and Types for AST node.
lookupAst :: String -> TcId -> TC (TypeVar UserType,Types)
lookupAst msg i = do
  v <- lookupAstVar msg i
  (v,) <$> lookupTypes msg i v

-- | Lookup type var for AST node.
lookupAstVar :: String -> TcId -> TC (TypeVar UserType)
lookupAstVar msg i = maybe (die' i $ msg ++ ": ast not already tracked: " ++ show i) return =<<
       (M.lookup i <$> use tcAstToVar)

-- | Lookup Types for type var. TcId is for logging only.
lookupTypes :: String -> TcId -> TypeVar UserType -> TC Types
lookupTypes msg i v =
  maybe (die' i $ msg ++ ":  var not already tracked: " ++ show v) return =<<
        (M.lookup v <$> use tcVarToTypes)

-- | Do substitution between a non-AST type (types from natives, overloads, schemas)
-- and an AST type.
assocTy :: Node -> Type UserType -> TC ()
assocTy (Node ai _) ty = do
  av <- lookupAstVar "assocTy" ai
  assocTy' ai av ty



assocTy' :: TcId -> TypeVar UserType -> Type UserType -> TC ()
assocTy' ai av ty = do
  aty <- lookupTypes "assocTy" ai av
  debug $ "assocTy: " ++ show (ai,aty,ty)
  unifyTypes' "assocTy" ai (_tsType aty) ty $ \r -> case r of
    Left _same -> do
      -- AST type is most specialized. If assoc ty is a var, update it to point
      -- at ast type.
      assocParams ty (_tsType aty)
      case ty of
        TyVar tv -> do
          tvtysm <- M.lookup tv <$> use tcVarToTypes
          case tvtysm of
            Nothing -> do
              debug $ "assocTy1: substituting " ++ show aty ++ " for var " ++ show tv
              tcVarToTypes %= M.insert tv aty
            Just tvtys ->
              -- substitute now for tracked var if necessary
              unifyTypes' "assocTy" ai (_tsType aty) (_tsType tvtys) $ \r' ->
                (tcVarToTypes . at tv . _Just . tsType) .= either id id r'
        _ -> debug $ "assocTy: no var to update: " ++ show (aty,ty)
    Right u -> do
      -- Associated ty is most specialized, simply update record for AST type.
      debug $ "assocTy2: substituting " ++ show u ++ " for " ++ show (ai,av,aty)
      tcVarToTypes . at av . _Just . tsType .= u
      -- if old type was var, make entry and adjust it
      case _tsType aty of
        TyVar atyv | u /= _tsType aty -> do
                       debug $ "assocTy: tracking/updating type variable " ++ show atyv ++ " as " ++ show u
                       alterTypes atyv (Types u []) (set tsType u)
        _ -> return ()

-- | Track type to id with typechecking
assocOverload :: TcId -> Overload -> TC ()
assocOverload ai o = do
  (av,aty) <- lookupAst "assocTy" ai
  debug $ "assocOverload: " ++ show (ai,av,aty,o)
  (tcVarToTypes . at av . _Just . tsOverloads) %= (o:)


alterTypes :: TypeVar UserType -> Types -> (Types -> Types) -> TC ()
alterTypes v newVal upd = tcVarToTypes %= M.alter (Just . maybe newVal upd) v

updateTyVar :: Type UserType -> Type UserType -> TC ()
updateTyVar (TyVar uv) u = do
  debug $ "updateTyVar: " ++ show (uv,u)
  alterTypes uv (Types u []) (set tsType u)
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

-- | Substitute AST types to most specialized.
assocAST :: TcId -> AST Node -> TC ()
assocAST ai b = do
  let bi = _aId (_aNode b)
  (av,aty) <- lookupAst "assocAST" ai
  (bv,bty) <- lookupAst "assocAST" bi
  let doSub si sv sty fi fv fty = do
        debug $ "assocAST: substituting " ++ show (si,sv,sty) ++ " for " ++ show (fi,fv,fty)
        -- reassign any references to old var to new
        tcAstToVar %= fmap (\v -> if v == fv then sv else v)
        -- cleanup old var
        unless (sv == fv) $ tcVarToTypes %= M.delete fv
  case unifyTypes (_tsType aty) (_tsType bty) of
    Nothing -> addFailure bi $ "assocAST: cannot unify: " ++ show (aty,bty)
    Just (Left _) -> doSub ai av aty bi bv bty
    Just (Right _) -> doSub bi bv bty ai av aty

unifyTypes' :: (Show n,Eq n) => String -> TcId -> Type n -> Type n -> (Either (Type n) (Type n) -> TC ()) -> TC ()
unifyTypes' msg i a b act = case unifyTypes a b of
  Just r -> act r
  Nothing -> do
    addFailure i $ "Cannot unify [" ++ msg ++ "]: " ++ show (a,b)
    return ()

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
toAST TSchema {..} = die _tInfo "User type in value position"
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
        fieldName <- asPrimString v'
        return (Named fieldName an,Var an)
  bb <- scopeToBody _tInfo (map ((\ai -> Var (_nnNamed ai)).fst) bs) _tBindBody
  bt <- case _tBindType of
    BindLet -> assocAST bi (last bb) >> return BindLet
    BindSchema sty -> do
      assocAST bi (last bb)
      sty' <- mangleType bi <$> traverse toUserType sty
      sn <- trackNode sty' =<< freshId _tInfo (show bi ++ "schema")
      return $ BindSchema sn
  return $ Binding bn bs bb bt

toAST TList {..} = do
  ty <- TyList <$> traverse toUserType _tListType
  List <$> (trackNode ty =<< freshId _tInfo "list") <*> mapM toAST _tList
toAST TObject {..} = do
  debug $ "TObject: " ++ show _tObjectType
  ty <- TySchema TyObject <$> traverse toUserType _tObjectType
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
toAST TStep {..} = do
  ent <- toAST _tStepEntity
  assocTy (_aNode ent) $ TyPrim TyString
  si <- freshId _tInfo "step"
  sn <- trackNode (idTyVar si) si
  ex <- toAST _tStepExec
  assocAST si ex
  Step sn ent ex <$> traverse toAST _tStepRollback

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
toUserType' TSchema {..} = Schema _tSchemaName _tModule <$> mapM (traverse toUserType') _tFields <*> pure _tInfo
toUserType' t = die (_tInfo t) $ "toUserType: expected user type: " ++ show t

bindArgs :: Info -> [a] -> Int -> TC a
bindArgs i args b =
  case args `atMay` b of
    Nothing -> die i $ "Missing arg: " ++ show b ++ ", " ++ show (length args) ++ " provided"
    Just a -> return a

-- | Convert a top-level Term to a TopLevel.
mkTop :: Term (Either Ref (AST Node)) -> TC (TopLevel Node)
mkTop t@TDef {} = do
  debug $ "===== Fun: " ++ abbrev t
  TopFun <$> toFun t
mkTop t@TConst {..} = do
  debug $ "===== Const: " ++ abbrev t
  TopConst _tInfo (asString _tModule ++ "." ++ _aName _tConstArg) <$>
    traverse toUserType (_aType _tConstArg) <*>
    toAST _tConstVal
mkTop t@TTable {..} = do
  debug $ "===== Table: " ++ abbrev t
  TopTable _tInfo (asString _tModule ++ "." ++ asString _tTableName) <$>
    traverse toUserType _tTableType
mkTop t@TSchema {..} = do
  debug $ "===== Schema: " ++ abbrev t
  TopUserType _tInfo <$> toUserType' t
mkTop t = die (_tInfo t) $ "Invalid top-level term: " ++ abbrev t


resolveTy :: Type UserType -> TC (Type UserType)
resolveTy tv@(TyVar v) = use tcVarToTypes >>= \m -> case M.lookup v m of
      Just (Types t _) | t /= tv -> resolveTy t
                       | otherwise -> return tv
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
  ast2Ty <- use tcAstToVar >>= \a2v -> (`M.traverseWithKey` a2v) $ \i tv -> do
    tysm <- M.lookup tv <$> use tcVarToTypes
    case tysm of
      Nothing -> die def $ "resolveAllTypes: untracked type var: " ++ show (i,tv)
      Just tys -> resolveTy (_tsType tys)
  let unresolved = M.filter isUnresolvedTy ast2Ty
  if M.null unresolved then debug "Successfully resolved all types"
    else forM_ (M.toList unresolved) $ \(i,v) ->
      addFailure i $ "Unable to resolve type (" ++ show v ++ ")"
  return ast2Ty

_debugState :: TC ()
_debugState = liftIO . putDoc . pretty =<< get

showFails :: TC ()
showFails = do
  fails <- use tcFailures
  unless (S.null fails) $ liftIO $ putDoc (prettyFails fails <> hardline)

-- | Typecheck a top-level production.
typecheck :: TopLevel Node -> TC (TopLevel Node)
typecheck f@(TopFun FDefun {..}) = do
  bod <- typecheckBody _fBody
  return $ set (tlFun . fBody) bod f
typecheck c@TopConst {..} = do
  assocTy (_aNode _tlConstVal) _tlType
  [v'] <- typecheckBody [_tlConstVal]
  return $ set tlConstVal v' c
typecheck tl = return tl


typecheckBody :: [AST Node] -> TC [AST Node]
typecheckBody body = do
  debug "Substitute defuns"
  appSub <- mapM (walkAST $ substAppDefun Nothing) body
  debug "Substitute natives"
  nativesProc <- mapM (walkAST processNatives) appSub
  debug "Apply Schemas"
  schEnforced <- mapM (walkAST applySchemas) nativesProc
  debug "Solve Overloads"
  solveOverloads
  ast2Ty <- resolveAllTypes
  fails <- use tcFailures
  unless (S.null fails) $ liftIO $ putDoc (prettyFails fails <> hardline)
  forM schEnforced $ \a -> forM a $ \n@(Node i _) -> case M.lookup i ast2Ty of
    Nothing -> die' i $ "Failed to find tracked AST for node: " ++ show n
    Just ty -> return (Node i ty)


typecheckTopLevel :: Ref -> TC (TopLevel Node)
typecheckTopLevel (Ref r) = do
  tl <- mkTop (fmap Left r)
  tl' <- typecheck tl
  debug $ "===== Done: " ++ abbrev r
  return tl'
typecheckTopLevel (Direct d) = die (_tInfo d) $ "Unexpected direct ref: " ++ abbrev d

typecheckModule :: Bool -> ModuleData -> IO [TopLevel Node]
typecheckModule dbg (Module {..},refs) = do
  debug' dbg $ "Typechecking module " ++ show _mName
  let tc (tls,sup) r = do
        (tl,TcState {..}) <- runTC sup dbg (typecheckTopLevel r)
        return (tl:tls,succ _tcSupply)
  fst <$> foldM tc ([],0) (HM.elems refs)



_loadModule :: FilePath -> ModuleName -> IO ModuleData
_loadModule fp mn = do
  (r,s) <- execScript' (Script fp) fp
  either (die def) (const (return ())) r
  case view (rEnv . eeRefStore . rsModules . at mn) s of
    Just m -> return m
    Nothing -> die def $ "Module not found: " ++ show (fp,mn)

_loadFun :: FilePath -> ModuleName -> String -> IO Ref
_loadFun fp mn fn = _loadModule fp mn >>= \(_,m) -> case HM.lookup fn m of
  Nothing -> die def $ "Function not found: " ++ show (fp,mn,fn)
  Just f -> return f


_infer :: FilePath -> ModuleName -> String -> IO (TopLevel Node, TcState)
_infer fp mn fn = _loadFun fp mn fn >>= \r -> runTC 0 True (typecheckTopLevel r)

-- _pretty =<< _inferIssue
_inferIssue :: IO (TopLevel Node, TcState)
_inferIssue = _infer "examples/cp/cp.repl" "cp" "issue"

-- _pretty =<< _inferTransfer
_inferTransfer :: IO (TopLevel Node, TcState)
_inferTransfer = _infer "examples/accounts/accounts.repl" "accounts" "transfer"

_inferTest1 :: IO (TopLevel Node, TcState)
_inferTest1 = _infer "tests/pact/tc.repl" "tctest" "unconsumed-app-typevar"

_inferModule :: FilePath -> ModuleName -> IO [TopLevel Node]
_inferModule fp mn = _loadModule fp mn >>= typecheckModule True

_inferTestModule :: IO [TopLevel Node]
_inferTestModule = _inferModule "tests/pact/tc.repl" "tctest"

_inferAccounts :: IO [TopLevel Node]
_inferAccounts = _inferModule "examples/accounts/accounts.repl" "accounts"

_inferCP :: IO [TopLevel Node]
_inferCP = _inferModule "examples/cp/cp.repl" "cp"

-- prettify output of '_infer' runs
_pretty :: (TopLevel Node, TcState) -> IO ()
_pretty (f,tc) = putDoc (pretty tc <> hardline <> hardline <> pretty f <> hardline)

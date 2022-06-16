{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}


module Pact.Core.IR.Desugar
 ( runDesugarTerm
 , runDesugarModule
 , runDesugarProgram
 ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens hiding (List,ix)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.List.NonEmpty(NonEmpty(..))
import Data.IORef
import Data.Set(Set)
import Data.Foldable(foldlM)
import Data.Graph(stronglyConnComp, SCC(..))
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Set as Set
import qualified Data.Text as T

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Literal
import Pact.Core.Hash
import Pact.Core.Persistence
import Pact.Core.IR.Term

import qualified Pact.Core.Syntax.New.ParseTree as PT
import qualified Pact.Core.Typed.Term as Typed

{- Note on Desugaring + Renaming:

  [Desugaring]
  In surface new pact core (and the lisp as well) we have "blocks",
  which are more like a sequence term `a; b`. We desugar blocks such as:
    let x1 = e1;
    let x2 = e2;
    defun f(a, b) = e3
    f1(f(x1, x2));
    f2();
  into:
    let x1 = e1 in
    let x2 = e2 in
    let f = fn (a, b) => e3 in
    {f1(f(x1, x2)), f2()}
  Moreover, `if a then b else c` gets desugared into
  `if a then () => b else () => c`

  [Renaming]
  In core, we use a locally nameless representation, and prior to the final pass,
  we use unique names for bound variables, for simplicity in the typechecker and in other passes.
  In the process of generating unique names and renaming bound locals, we also perform two other
  tasks:
    - We resolve imported names
    - We ensure the call graph in the functions declared in the module is acyclic
    If perf of stronglyConnCompR is every measured to be suboptimal, it might be
    worth writing our own.
-}


data RenamerEnv
  = RenamerEnv
  { _reBinds :: Map Text (IRNameKind, Unique)
  , _reTyBinds :: Map Text Unique
  , _reSupply :: IORef Supply
  }
makeLenses ''RenamerEnv

data RenamerState b i
  = RenamerState
  { _rsModuleBinds :: Map ModuleName (Map Text IRNameKind)
  , _rsLoaded :: Loaded b i }

makeLenses ''RenamerState

newtype RenamerT cb ci a =
  RenamerT (StateT (RenamerState cb ci) (ReaderT RenamerEnv IO) a)
  deriving
    (Functor, Applicative, Monad
    , MonadReader RenamerEnv
    , MonadState (RenamerState cb ci)
    , MonadFail
    , MonadIO)
  via (StateT (RenamerState cb ci) (ReaderT RenamerEnv IO))

newUnique' :: RenamerT cb ci Unique
newUnique' = do
  sup <- view reSupply
  u <- liftIO (readIORef sup)
  liftIO (modifyIORef' sup (+ 1))
  pure u

dummyTLUnique :: Unique
dummyTLUnique = -1111

class DesugarBuiltin b where
  builtinIf :: b
  reservedNatives :: Map Text b
  desugarBinary :: PT.BinaryOp -> b
  desugarUnary :: PT.UnaryOp -> b

instance DesugarBuiltin RawBuiltin where
  reservedNatives = rawBuiltinMap
  builtinIf = RawIf
  desugarBinary = desugarBinary'
  desugarUnary = desugarUnary'

instance DesugarBuiltin (ReplBuiltin RawBuiltin) where
  reservedNatives = replRawBuiltinMap
  builtinIf = RBuiltinWrap RawIf
  desugarBinary = RBuiltinWrap . desugarBinary'
  desugarUnary = RBuiltinWrap . desugarUnary'

-----------------------------------------------------------
-- Desugaring
-----------------------------------------------------------
defunToLet :: PT.Defun ParsedName i -> PT.Expr ParsedName i
defunToLet = \case
  PT.Defun defname args retTy body i ->
    let lamName = BN (BareName defname)
    in case args of
      [] ->
        let defTy = PT.TyFun PT.TyUnit retTy
            lamBody = PT.Lam lamName (("#unitArg", Just PT.TyUnit):| []) body i
        in PT.Let defname (Just defTy) lamBody i
      x:xs ->
        let args' = x:|xs
            defTy = foldr PT.TyFun retTy (PT._argType <$> args')
            lamArgs = (\(PT.Arg n ty) -> (n, Just ty)) <$> args'
            lamBody = PT.Lam lamName lamArgs body i
        in PT.Let defname (Just defTy) lamBody i


desugarTerm :: forall b i. DesugarBuiltin b => PT.Expr ParsedName i -> Term ParsedName Text b i
desugarTerm = \case
  PT.Var (BN n) i | isReservedNative (_bnName n) ->
    Builtin (reservedNatives Map.! _bnName n) i
  PT.Var n i -> Var n i
  PT.Block (h :| hs) _ ->
    unLetBlock h hs
  PT.Let name mt expr i -> let
    name' = BN (BareName  name)
    expr' = desugarTerm expr
    mt' = desugarType <$> mt
    in Let name' mt' expr' (Constant LUnit i) i
  PT.NestedDefun d _ ->
    desugarTerm (defunToLet d)
  PT.LetIn name mt e1 e2 i -> let
    name' = BN (BareName  name)
    e1' = desugarTerm e1
    mt' = desugarType <$> mt
    e2' = desugarTerm e2
    in Let name' mt' e1' e2' i
  PT.Lam _name nsts body i -> let
    (ns, ts) = NE.unzip nsts
    ns' = BN . BareName <$> ns
    ts' = fmap desugarType <$> ts
    body' = desugarTerm body
    in Lam (NE.zip ns' ts') body' i
  PT.If cond e1 e2 i -> let
    cond' = desugarTerm cond
    e1' = suspend i e1
    e2' = suspend i e2
    in App (Builtin builtinIf i) (cond' :| [e1', e2']) i
  PT.App e [] i -> let
    e' = desugarTerm e
    body = Constant LUnit i :| []
    in App e' body i
  PT.App e (h:hs) i -> let
    e' = desugarTerm e
    h' = desugarTerm h
    hs' = fmap desugarTerm hs
    in App e' (h' :| hs') i
  PT.BinaryOp bop e1 e2 i -> let
    e1' = desugarTerm e1
    e2' = desugarTerm e2
    in App (Builtin (desugarBinary bop) i) (e1' :| [e2']) i
  PT.UnaryOp uop e1 i -> let
    e1' = desugarTerm e1
    in App (Builtin (desugarUnary uop) i) (e1' :| []) i
  PT.List e1 i ->
    ListLit (V.fromList (desugarTerm <$> e1)) i
  PT.Constant l i ->
    Constant l i
  PT.Object objs i ->
    ObjectLit (desugarTerm <$> objs) i
  PT.ObjectOp o i ->
    ObjectOp (desugarTerm <$> o) i
  PT.Error text i ->
    Error text i
  where
  isReservedNative n =
    Map.member n (reservedNatives @b)
  suspend i e = let
    name = BN (BareName "#ifArg")
    e' = desugarTerm e
    in Lam ((name, Just TyUnit) :| []) e' i
  unLetBlock (PT.NestedDefun d _) rest = do
    unLetBlock (defunToLet d) rest
  unLetBlock (PT.Let name mt expr i) (h:hs) = let
    name' = BN (BareName name)
    expr' = desugarTerm expr
    mt' = desugarType <$> mt
    e2 = unLetBlock h hs
    in Let name' mt' expr' e2 i
  unLetBlock other l = case l of
    h:hs -> let
      other' = desugarTerm other
      in case unLetBlock h hs of
        Block nel' i' ->
          Block (NE.cons other' nel') i'
        t -> Block (other' :| [t]) (other' ^. termInfo)
    [] -> desugarTerm other

desugarDefun :: DesugarBuiltin b => PT.Defun ParsedName i -> Defun ParsedName Text b i
desugarDefun (PT.Defun defname [] rt body i) = let
  defname' = BN (BareName defname)
  dfnType = TyFun TyUnit (desugarType rt)
  lamName = BN (BareName defname)
  lamBody = PT.Lam lamName (("#unitArg", Just PT.TyUnit) :| []) body i
  body' = desugarTerm lamBody
  in Defun defname' dfnType body' i
desugarDefun (PT.Defun defname (arg:args) rt body i) = let
  defname' = BN (BareName defname)
  neArgs = arg :| args
  dfnType = foldr TyFun (desugarType rt) (desugarType . PT._argType <$> neArgs)
  lamName = BN (BareName defname)
  lamArgs = (\(PT.Arg n ty) -> (n, Just ty)) <$> neArgs
  lamBody = PT.Lam lamName lamArgs body i
  body' = desugarTerm lamBody
  in Defun defname' dfnType body' i

desugarDefConst :: DesugarBuiltin b => PT.DefConst ParsedName i -> DefConst ParsedName Text b i
desugarDefConst (PT.DefConst n mty e i) = let
  n' = BN (BareName n)
  mty' = desugarType <$> mty
  e' = desugarTerm e
  in DefConst n' mty' e' i

desugarDef :: DesugarBuiltin b => PT.Def ParsedName i -> Def ParsedName Text b i
desugarDef = \case
  PT.Dfun d -> Dfun (desugarDefun d)
  PT.DConst d -> DConst (desugarDefConst d)

desugarModule :: DesugarBuiltin b => PT.Module ParsedName i -> Module ParsedName Text b i
desugarModule (PT.Module mname gov extdecls defs) = let
  (imports, blessed, implemented) = splitExts extdecls
  defs' = desugarDef <$> NE.toList defs
  mhash = ModuleHash (Hash "placeholder")
  gov' = BN . BareName <$> gov
  in Module mname gov' defs' blessed imports implemented mhash
  where
  splitExts = split ([], Set.empty, [])
  split (accI, accB, accImp) (h:hs) = case h of
    -- todo: implement bless hashes
    PT.ExtBless _ -> split (accI, accB, accImp) hs
    PT.ExtImport i -> split (i:accI, accB, accImp) hs
    PT.ExtImplements mn -> split (accI, accB, mn:accImp) hs
  split (a, b, c) [] = (reverse a, b, reverse c)


desugarType :: PT.Type -> Type Text
desugarType = \case
  PT.TyVar v ->
    TyVar v
  PT.TyPrim p -> TyPrim p
  PT.TyFun l r ->
    TyFun (desugarType l) (desugarType r)
  PT.TyObject o mt ->
    let o' = desugarType <$> o
    in TyRow (RowTy o' mt)
  PT.TyList t ->
    TyList (desugarType t)
  PT.TyCap -> TyCap

desugarUnary' :: PT.UnaryOp -> RawBuiltin
desugarUnary' = \case
  PT.NegateOp -> RawNegate
  PT.FlipBitsOp -> RawBitwiseFlip

desugarBinary' :: PT.BinaryOp -> RawBuiltin
desugarBinary' = \case
  PT.AddOp -> RawAdd
  PT.SubOp -> RawSub
  PT.MultOp -> RawMultiply
  PT.DivOp -> RawDivide
  PT.GTOp -> RawGT
  PT.GEQOp -> RawGEQ
  PT.LTOp -> RawLT
  PT.LEQOp -> RawLEQ
  PT.EQOp -> RawEq
  PT.NEQOp -> RawNeq
  PT.BitAndOp -> RawBitwiseAnd
  PT.BitOrOp -> RawBitwiseOr
  PT.AndOp -> RawAnd
  PT.OrOp -> RawOr

-----------------------------------------------------------
-- Renaming
-----------------------------------------------------------

termSCC
  :: ModuleName
  -> Term IRName TypeVar b1 i1
  -> Set Text
termSCC currM = conn
  where
  conn = \case
    Var n _ -> case _irNameKind n of
      IRTopLevel m _ | m == currM ->
        Set.singleton (_irName n)
      _ -> Set.empty
    Lam _ e _ -> conn e
    Let _ _ e1 e2 _ -> Set.union (conn e1) (conn e2)
    App fn apps _ ->
      Set.union (conn fn) (foldMap conn apps)
    Block nel _ -> foldMap conn nel
    Builtin{} -> Set.empty
    DynAccess{} -> Set.empty
    Constant{} -> Set.empty
    ObjectLit o _ -> foldMap conn o
    ListLit v _ -> foldMap conn v
    ObjectOp o _ -> foldMap conn o
    Error{} -> Set.empty


defunSCC :: ModuleName -> Defun IRName TypeVar b i -> Set Text
defunSCC mn = termSCC mn . _dfunTerm

defConstSCC :: ModuleName -> DefConst IRName TypeVar b i -> Set Text
defConstSCC mn = termSCC mn . _dcTerm

defSCC :: ModuleName -> Def IRName TypeVar b i1 -> Set Text
defSCC mn = \case
  Dfun d -> defunSCC mn d
  DConst d -> defConstSCC mn d

-- | Look up a qualified name in the pact db
-- if it's there, great! We will load the module into the scope of
-- `Loaded`, as well as include it in the renamer map
-- Todo: Bare namespace lookup first, then
-- current namespace.
-- Namespace definitions are yet to be supported in core
lookupModuleMember
  :: HasPactDb cb ci
  => ModuleName
  -> Text
  -> RenamerT cb ci IRName
lookupModuleMember modName name = do
  liftIO (_readModule ?pactDb modName) >>= \case
    Just md -> let
      module_ = _mdModule md
      mhash = Typed._mHash module_
      depMap = Map.fromList $ toDepMap mhash <$> Typed._mDefs module_
      in case Map.lookup name depMap of
        -- Great! The name exists
        -- This, we must include the module in `Loaded`, as well as propagate its deps and
        -- all loaded members in `loAllLoaded`
        Just irtl -> do
          let memberTerms = Map.fromList (toFqDep mhash <$> Typed._mDefs module_)
              allDeps = Map.union memberTerms (_mdDependencies md)
          rsLoaded %= over loModules (Map.insert modName md) . over loAllLoaded (Map.union allDeps)
          rsModuleBinds %= Map.insert modName depMap
          pure (IRName name irtl dummyTLUnique)
        -- Module exists, but it has no such member
        -- Todo: check whether the module name includes a namespace
        -- if it does not, we retry the lookup under the current namespace
        Nothing -> fail "boom: module does not have member"
    Nothing -> fail "no such module"
  where
  rawDefName def = _nName (Typed.defName def)
  toDepMap mhash def = (rawDefName def, IRTopLevel modName mhash)
  toFqDep mhash def = let
    fqn = FullyQualifiedName modName (rawDefName def) mhash
    in (fqn, Typed.defTerm def)


-- Rename a term (that is part of a module)
-- emitting the list of dependent calls
renameTerm
  :: HasPactDb cb ci
  => Term ParsedName Text b i
  -> RenamerT cb ci (Term IRName TypeVar b i)
renameTerm = fvd
  where
  fvd (Var n i) = case n of
      BN (BareName bn) ->
        views reBinds (Map.lookup bn) >>= \case
          -- We emit a top level name from the same module
          -- as part of the set of dependencies
          Just (irnk, u) ->
            pure (Var (IRName bn irnk u) i)
          Nothing -> fail "unbound free variable"
      QN (QualifiedName qn qmn) -> do
        uses rsModuleBinds (Map.lookup qmn) >>= \case
          Just binds -> case Map.lookup qn binds of
            Just irnk -> pure (Var (IRName qn irnk dummyTLUnique) i)
            Nothing -> do
              irn <- lookupModuleMember qmn qn
              pure (Var irn i)
          Nothing -> fail "todo: implement module lookup"
  fvd (Lam nsts body i) = do
    let (pns, ts) = NE.unzip nsts
        ns = rawParsedName <$> pns
    nUniques <- traverse (const newUnique') ns
    let m = Map.fromList $ NE.toList $ NE.zip ns ((IRBound,) <$> nUniques)
        ns' = NE.zipWith (`IRName` IRBound) ns nUniques
    term' <- locally reBinds (Map.union m) (fvd body)
    ts' <- (traverse.traverse) renameType ts
    pure (Lam (NE.zip ns' ts') term' i)
  fvd (Let name mt e1 e2 i) = do
    nu <- newUnique'
    let rawName = rawParsedName name
        name' = IRName rawName IRBound nu
    e1' <- fvd e1
    mt' <- traverse renameType mt
    e2' <- locally reBinds (Map.insert rawName (IRBound, nu)) (fvd e2)
    pure (Let name' mt' e1' e2' i)
  fvd (App fn apps i) = do
    fn' <- fvd fn
    apps' <- traverse fvd apps
    pure (App fn' apps' i)
  fvd (Block exprs i) = do
    exprs' <- traverse fvd exprs
    pure (Block exprs' i)
  fvd (Builtin b i) = pure (Builtin b i)
  fvd DynAccess{} = fail "todo: implement"
  fvd (Constant l i) =
    pure (Constant l i)
  fvd (ObjectLit o i) =
    ObjectLit <$> traverse fvd o <*> pure i
  fvd (ListLit v i) = do
    ListLit <$> traverse fvd v <*> pure i
  fvd (ObjectOp o i) = do
    ObjectOp <$> traverse fvd o <*> pure i
  fvd (Error e i) =
    pure (Error e i)

renameType :: Type Text -> RenamerT b i (Type TypeVar)
renameType = \case
  TyVar v ->
    TyVar <$> lookupTyVar v
  TyPrim p ->
    pure (TyPrim p)
  TyFun l r ->
    TyFun <$> renameType l <*> renameType r
  TyRow row -> case row of
    RowTy o mt -> do
      o' <- traverse renameType o
      mt' <- traverse lookupTyVar mt
      pure (TyRow (RowTy o' mt'))
    RowVar rv -> do
      TyRow . RowVar <$> lookupTyVar rv
    EmptyRow -> pure (TyRow EmptyRow)
  TyList t ->
    TyList <$> renameType t
  TyCap -> pure TyCap
  _ -> fail "Unsupported renaming of nested quantifier"
  where
  lookupTyVar v = views reTyBinds (Map.lookup v) >>= \case
    Just u ->
      pure $ TypeVar v u
    Nothing ->
      TypeVar v <$> newUnique'

renameDefun
  :: HasPactDb cb ci
  => ModuleName
  -> ModuleHash
  -> Defun ParsedName Text b i
  -> RenamerT cb ci (Defun IRName TypeVar b i)
renameDefun mn mh (Defun n dty term i) = do
  let n' = IRName (rawParsedName n) (IRTopLevel mn mh) dummyTLUnique
  -- Todo: put type variables in scope here, if we want to support polymorphism
  dty' <- renameType dty
  term' <- renameTerm term
  pure (Defun n' dty' term' i)

renameDefConst
  :: HasPactDb cb ci
  => ModuleName
  -> ModuleHash
  -> DefConst ParsedName Text b i
  -> RenamerT cb ci (DefConst IRName TypeVar b i)
renameDefConst mn mh (DefConst n mty term i) = do
  let n' = IRName (rawParsedName n) (IRTopLevel mn mh) dummyTLUnique
  -- Todo: put type variables in scope here, if we want to support polymorphism
  mty' <- traverse renameType mty
  term' <- renameTerm term
  pure (DefConst n' mty' term' i)

renameDef
  :: HasPactDb cb ci
  => ModuleName
  -> ModuleHash
  -> Def ParsedName Text b i
  -> RenamerT cb ci (Def IRName TypeVar b i)
renameDef mn mh = \case
  Dfun d -> Dfun <$> renameDefun mn mh d
  DConst d -> DConst <$> renameDefConst mn mh d

resolveBareName' :: Text -> RenamerT b i IRName
resolveBareName' bn = views reBinds (Map.lookup bn) >>= \case
  Just (irnk, u) -> pure (IRName bn irnk u)
  Nothing -> fail $ "Expected identifier " <> T.unpack bn <> " in scope"

-- | Todo: support imports
renameModule
  :: HasPactDb cb ci
  => Module ParsedName Text b i
  -> RenamerT cb ci (Module IRName TypeVar b i)
renameModule (Module mname mgov defs blessed imp implements mhash) = do
  let rawDefNames = rawDefName <$> defs
      defMap = Map.fromList $ (, (IRTopLevel mname mhash, dummyTLUnique)) <$> rawDefNames
      fqns = Map.fromList $ (\n -> (n, FullyQualifiedName mname n mhash)) <$> rawDefNames
  -- `maybe all of this next section should be in a block laid out by the
  -- `locally reBinds`
  rsModuleBinds %= Map.insert mname (fst <$> defMap)
  rsLoaded . loToplevel %= Map.union fqns
  defs' <- locally reBinds (Map.union defMap) $ traverse (renameDef mname mhash) defs
  let scc = mkScc <$> defs'
  defs'' <- forM (stronglyConnComp scc) \case
    AcyclicSCC d -> pure d
    CyclicSCC d -> fail $ "Functions: " <> show (defName  <$> d) <> " form a cycle"
  mgov' <- locally reBinds (Map.union defMap) $ traverse (resolveBareName' . rawParsedName) mgov
  pure (Module mname mgov' defs'' blessed imp implements mhash)
  where
  rawDefName = rawParsedName . defName
  mkScc def = (def, _irName (defName def), Set.toList (defSCC mname def))

runRenamerT
  :: RenamerState b i
  -> RenamerEnv
  -> RenamerT b i a
  -> IO (a, RenamerState b i)
runRenamerT st env (RenamerT act) = runReaderT (runStateT act st) env

reStateFromLoaded :: Loaded b i -> RenamerState b i
reStateFromLoaded loaded = RenamerState mbinds loaded
  where
  mbind md = let
    m = _mdModule md
    depNames = _nName . Typed.defName <$> Typed._mDefs m
    in Map.fromList $ (,IRTopLevel (Typed._mName m) (Typed._mHash m)) <$> depNames
  mbinds = fmap mbind (_loModules loaded)

loadedBinds :: Loaded b i -> Map Text (IRNameKind, Unique)
loadedBinds loaded =
  let f fqn  = (IRTopLevel (_fqModule fqn) (_fqHash fqn), dummyTLUnique)
  in f <$> _loToplevel loaded

runDesugar'
  :: Loaded b i
  -> Supply
  -> RenamerT b i a
  -> IO (a, Supply, Loaded b i)
runDesugar' loaded supply act = do
  ref <- newIORef supply
  let reState = reStateFromLoaded loaded
      rTLBinds = loadedBinds loaded
      rEnv = RenamerEnv rTLBinds mempty ref
  (renamed, state') <- runRenamerT reState rEnv act
  lastSupply <- readIORef ref
  pure (renamed, lastSupply, _rsLoaded state')

runDesugarTerm'
  :: (HasPactDb b i, DesugarBuiltin b')
  => Loaded b i
  -> Supply
  -> PT.Expr ParsedName i
  -> IO (Term IRName TypeVar b' i, Supply, Loaded b i)
runDesugarTerm' loaded supply e = let
  desugared = desugarTerm e
  in runDesugar' loaded supply (renameTerm desugared)

runDesugarTerm
  :: (HasPactDb b i, DesugarBuiltin b')
  => Loaded b i
  -> PT.Expr ParsedName i
  -> IO (Term IRName TypeVar b' i, Supply, Loaded b i)
runDesugarTerm loaded = runDesugarTerm' loaded 0

runDesugarModule'
  :: (HasPactDb b i, DesugarBuiltin b')
  => Loaded b i
  -> Supply
  -> PT.Module ParsedName i
  -> IO (Module IRName TypeVar b' i, Supply, Loaded b i)
runDesugarModule' loaded supply m  = let
  desugared = desugarModule m
  in runDesugar' loaded supply (renameModule desugared)

runDesugarModule
  :: (HasPactDb b i, DesugarBuiltin b')
  => Loaded b i
  -> PT.Module ParsedName i
  -> IO (Module IRName TypeVar b' i, Supply, Loaded b i)
runDesugarModule loaded = runDesugarModule' loaded 0


runDesugarTopLevel'
  :: (HasPactDb b i, DesugarBuiltin b')
  => Loaded b i
  -> Supply
  -> PT.TopLevel ParsedName i
  -> IO (TopLevel IRName TypeVar b' i, Supply, Loaded b i)
runDesugarTopLevel' loaded supply = \case
  PT.TLModule m -> over _1 TLModule <$> runDesugarModule' loaded supply m
  PT.TLTerm e -> over _1 TLTerm <$> runDesugarTerm' loaded supply e

runDesugarProgram
  :: (HasPactDb b i, DesugarBuiltin b')
  => Loaded b i
  -> [PT.TopLevel ParsedName i]
  -> IO ([TopLevel IRName TypeVar b' i], Supply, Loaded b i)
runDesugarProgram loaded program = do
  let supply = 0
  over _1 reverse <$> foldlM go ([], supply, loaded) program
  where
  go (tls, s, l) tl = do
    (tl', s', l') <- runDesugarTopLevel' l s tl
    pure (tl':tls, s', l')

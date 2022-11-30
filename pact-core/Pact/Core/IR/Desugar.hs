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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}


module Pact.Core.IR.Desugar
 ( runDesugarTermLisp
 , runDesugarTopLevelLisp
 , DesugarOutput(..)
 , DesugarBuiltin(..)
 ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens hiding (List,ix)
import Data.Text(Text)
import Data.Map.Strict(Map)
-- import Data.List(findIndex)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Set(Set)
import Data.Graph(stronglyConnComp, SCC(..))
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Literal
import Pact.Core.Hash
import Pact.Core.Persistence
import Pact.Core.IR.Term

import qualified Pact.Core.Syntax.Common as Common
import qualified Pact.Core.Syntax.Lisp.ParseTree as Lisp
import qualified Pact.Core.Untyped.Term as Term

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

  A special note: The "empty application" is currently being desugared into unit application for
  all but builtins. That is:
  (f) is desugared as `(f ())` and

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


data RenamerEnv b i
  = RenamerEnv
  { _reBinds :: Map Text NameKind
  , _reVarDepth :: DeBruijn
  , _rePactDb :: PactDb b i
  }
makeLenses ''RenamerEnv

data RenamerState b i
  = RenamerState
  { _rsModuleBinds :: Map ModuleName (Map Text NameKind)
  , _rsLoaded :: Loaded b i
  , _rsDependencies :: Set ModuleName }

makeLenses ''RenamerState

newtype RenamerM cb ci a =
  RenamerT (StateT (RenamerState cb ci) (ReaderT (RenamerEnv cb ci) IO) a)
  deriving
    (Functor, Applicative, Monad
    , MonadReader (RenamerEnv cb ci)
    , MonadState (RenamerState cb ci)
    , MonadFail
    , MonadIO)
  via (StateT (RenamerState cb ci) (ReaderT (RenamerEnv cb ci) IO))

data DesugarOutput b i a
  = DesugarOutput
  { _dsOut :: a
  , _dsLoaded :: Loaded b i
  , _dsDeps :: Set ModuleName
  } deriving (Show, Functor)

dsOut :: Lens (DesugarOutput b i a) (DesugarOutput b i a') a a'
dsOut f (DesugarOutput a l d) =
  f a <&> \a' -> DesugarOutput a' l d

class DesugarBuiltin b where
  builtinIf :: b
  reservedNatives :: Map Text b
  desugarBinary :: Common.BinaryOp -> b
  desugarUnary :: Common.UnaryOp -> b

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

-- type DesugarTerm term b i = (?desugarTerm :: term -> Term ParsedName Text b i)
class DesugarTerm term b i where
  desugarTerm  :: term -> Term ParsedName b i

instance DesugarBuiltin b => DesugarTerm (Lisp.Expr i) b i where
  desugarTerm = desugarLispTerm

desugarLispTerm :: forall b i. DesugarBuiltin b => Lisp.Expr i -> Term ParsedName b i
desugarLispTerm = \case
  Lisp.Var (BN n) i | isReservedNative (_bnName n) ->
    Builtin (reservedNatives Map.! _bnName n) i
  Lisp.Var n i -> Var n i
  Lisp.Block nel i ->
    let nel' = desugarLispTerm <$> nel
    in foldr (\a b -> Sequence a b i) (NE.last nel') (NE.init nel')
  Lisp.LetIn binders expr i ->
    let
      expr' = desugarLispTerm expr
    in foldr (binderToLet i) expr' binders
  Lisp.Lam _name nsts body i ->
    let
      (ns, ts) = NE.unzip nsts
      ns' = BN . BareName <$> ns
      ts' = fmap desugarType <$> ts
      body' = desugarLispTerm body
    in Lam (NE.zip ns' ts') body' i
  Lisp.If cond e1 e2 i ->
    let
      cond' = desugarLispTerm cond
      e1' = suspend i e1
      e2' = suspend i e2
    in App (Builtin builtinIf i) (cond' :| [e1', e2']) i
  -- Todo: this is a syntactic hack, at the moment, for `(+) to mean `+` but
  -- `(f)` to mean `(f ())`. Whether this stays or not is tentative.
  Lisp.App e [] i -> case desugarLispTerm e of
    v@Var{} ->
      let arg = Constant LUnit i :| []
      in App v arg i
    e' -> e'
  Lisp.App e (h:hs) i ->
    let
      e' = desugarLispTerm e
      h' = desugarLispTerm h
      hs' = fmap desugarLispTerm hs
    in App e' (h' :| hs') i
  Lisp.Operator bop i ->
    Builtin (desugarBinary bop) i
  Lisp.List e1 i ->
    ListLit (desugarLispTerm <$> e1) i
  Lisp.Constant l i ->
    Constant l i
  Lisp.Try e1 e2 i ->
    Try (desugarLispTerm e1) (desugarLispTerm e2) i
  Lisp.Error e i ->
    Error e i
  where
  binderToLet i (Lisp.Binder n mty expr) term =
    Let (BN (BareName n)) (desugarType <$> mty) (desugarLispTerm expr) term i
  isReservedNative n =
    Map.member n (reservedNatives @b)
  suspend i e = let
    name = BN (BareName "#ifArg")
    e' = desugarLispTerm e
    in Lam ((name, Just TyUnit) :| []) e' i

desugarDefun :: (DesugarTerm term b i) => Common.Defun term i -> Defun ParsedName b i
desugarDefun (Common.Defun defname [] rt body i) = let
  dfnType = TyFun TyUnit (desugarType rt)
  lamName = BN (BareName defname)
  body' = Lam ((lamName, Just TyUnit) :| []) (desugarTerm body) i
  in Defun defname dfnType body' i
desugarDefun (Common.Defun defname (arg:args) rt body i) = let
  neArgs = arg :| args
  dfnType = foldr TyFun (desugarType rt) (desugarType . Common._argType <$> neArgs)
  lamArgs = (\(Common.Arg n ty) -> (BN (BareName n), Just (desugarType ty))) <$> neArgs
  body' = Lam lamArgs (desugarTerm body) i
  in Defun defname dfnType body' i

desugarDefConst :: (DesugarTerm term b i) => Common.DefConst term i -> DefConst ParsedName b i
desugarDefConst (Common.DefConst n mty e i) = let
  mty' = desugarType <$> mty
  e' = desugarTerm e
  in DefConst n mty' e' i

-- desugarDefCap :: DesugarTerm expr builtin info => Common.DefCap expr info -> DefCap ParsedName builtin info
-- desugarDefCap (Common.DefCap dn argList managed term i) = let
--   managed' = maybe Unmanaged fromCommonCap managed
--   lamArgs = (\(Common.Arg n ty) -> (BN (BareName n), Just (desugarType ty))) <$> argList
--   -- term' = Lam lamArgs (desugarTerm term) i
--   capType = foldr TyFun TyCap (desugarType . Common._argType <$> argList)
--   in case lamArgs of
--     [] -> DefCap dn [] (desugarTerm term) managed' capType i
--     (arg:args) ->  DefCap dn [] (Lam (arg :| args) (desugarTerm term) i) managed' capType i
--   where
--   fromCommonCap = \case
--     Common.AutoManaged -> AutomanagedCap
--     Common.Managed t pn -> case findIndex ((== t) . Common._argName) argList of
--       Nothing -> error "invalid managed cap decl"
--       Just n -> let
--         ty' = desugarType $ Common._argType (argList !! n)
--         in ManagedCap n ty' pn


desugarDef :: (DesugarTerm term b i) => Common.Def term i -> Def ParsedName b i
desugarDef = \case
  Common.Dfun d -> Dfun (desugarDefun d)
  Common.DConst d -> DConst (desugarDefConst d)
  -- Common.DCap d -> DCap (desugarDefCap d)

desugarModule :: (DesugarTerm term b i) => Common.Module term i -> Module ParsedName b i
desugarModule (Common.Module mname extdecls defs) = let
  (imports, blessed, implemented) = splitExts extdecls
  defs' = desugarDef <$> NE.toList defs
  mhash = ModuleHash (Hash "placeholder")
  -- gov' = BN . BareName <$> gov
  in Module mname defs' blessed imports implemented mhash
  where
  splitExts = split ([], Set.empty, [])
  split (accI, accB, accImp) (h:hs) = case h of
    -- todo: implement bless hashes
    Common.ExtBless _ -> split (accI, accB, accImp) hs
    Common.ExtImport i -> split (i:accI, accB, accImp) hs
    Common.ExtImplements mn -> split (accI, accB, mn:accImp) hs
  split (a, b, c) [] = (reverse a, b, reverse c)


desugarType :: Common.Type -> Type a
desugarType = \case
  Common.TyPrim p -> TyPrim p
  Common.TyFun l r ->
    TyFun (desugarType l) (desugarType r)
  -- Common.TyObject o ->
  --   let o' = desugarType <$> o
  --   in TyRow (RowTy o' Nothing)
  Common.TyList t ->
    TyList (desugarType t)
  -- Common.TyCap -> TyCap

desugarUnary' :: Common.UnaryOp -> RawBuiltin
desugarUnary' = \case
  Common.NegateOp -> RawNegate
  Common.ComplementOp -> RawBitwiseFlip

desugarBinary' :: Common.BinaryOp -> RawBuiltin
desugarBinary' = \case
  Common.AddOp -> RawAdd
  Common.SubOp -> RawSub
  Common.MultOp -> RawMultiply
  Common.DivOp -> RawDivide
  Common.GTOp -> RawGT
  Common.GEQOp -> RawGEQ
  Common.LTOp -> RawLT
  Common.LEQOp -> RawLEQ
  Common.EQOp -> RawEq
  Common.NEQOp -> RawNeq
  Common.BitAndOp -> RawBitwiseAnd
  Common.BitOrOp -> RawBitwiseOr
  Common.AndOp -> RawAnd
  Common.OrOp -> RawOr

-----------------------------------------------------------
-- Renaming
-----------------------------------------------------------

termSCC
  :: ModuleName
  -> Term Name b1 i1
  -> Set Text
termSCC currM = conn
  where
  conn = \case
    Var n _ -> case _nKind n of
      NTopLevel m _ | m == currM ->
        Set.singleton (_nName n)
      _ -> Set.empty
    Lam _ e _ -> conn e
    Let _ _ e1 e2 _ -> Set.union (conn e1) (conn e2)
    App fn apps _ ->
      Set.union (conn fn) (foldMap conn apps)
    Sequence e1 e2 _ -> Set.union (conn e1) (conn e2)
    Builtin{} -> Set.empty
    Constant{} -> Set.empty
    ListLit v _ -> foldMap conn v
    Try e1 e2 _ -> Set.union (conn e1) (conn e2)
    Error {} -> Set.empty
    -- ObjectLit o _ -> foldMap conn o
    -- ObjectOp o _ -> foldMap conn o


defunSCC :: ModuleName -> Defun Name b i -> Set Text
defunSCC mn = termSCC mn . _dfunTerm

defConstSCC :: ModuleName -> DefConst Name b i -> Set Text
defConstSCC mn = termSCC mn . _dcTerm

-- defCapSCC :: ModuleName -> DefCap Name b i -> Set Text
-- defCapSCC mn = termSCC mn . _dcapTerm

defSCC :: ModuleName -> Def Name b i1 -> Set Text
defSCC mn = \case
  Dfun d -> defunSCC mn d
  DConst d -> defConstSCC mn d
  -- DCap d -> defCapSCC mn d

-- | Look up a qualified name in the pact db
-- if it's there, great! We will load the module into the scope of
-- `Loaded`, as well as include it in the renamer map
-- Todo: Bare namespace lookup first, then
-- current namespace.
-- Namespace definitions are yet to be supported in core
lookupModuleMember
  :: ModuleName
  -> Text
  -> RenamerM cb ci Name
lookupModuleMember modName name = do
  view rePactDb >>= liftIO . (`_readModule` modName) >>= \case
    Just md -> let
      module_ = _mdModule md
      mhash = Term._mHash module_
      depMap = Map.fromList $ toDepMap mhash <$> Term._mDefs module_
      in case Map.lookup name depMap of
        -- Great! The name exists
        -- This, we must include the module in `Loaded`, as well as propagate its deps and
        -- all loaded members in `loAllLoaded`
        Just irtl -> do
          let memberTerms = Map.fromList (toFqDep mhash <$> Term._mDefs module_)
              allDeps = Map.union memberTerms (_mdDependencies md)
          rsLoaded %= over loModules (Map.insert modName md) . over loAllLoaded (Map.union allDeps)
          rsModuleBinds %= Map.insert modName depMap
          rsDependencies %= Set.insert modName
          pure (Name name irtl)
        -- Module exists, but it has no such member
        -- Todo: check whether the module name includes a namespace
        -- if it does not, we retry the lookup under the current namespace
        Nothing -> fail "boom: module does not have member"
    Nothing -> fail "no such module"
  where
  rawDefName def = Term.defName def
  toDepMap mhash def = (rawDefName def, NTopLevel modName mhash)
  toFqDep mhash def = let
    fqn = FullyQualifiedName modName (rawDefName def) mhash
    in (fqn, def)

-- Rename a term (that is part of a module)
-- emitting the list of dependent calls
renameTerm
  :: Term ParsedName b i
  -> RenamerM cb ci (Term Name b i)
renameTerm (Var n i) = (`Var` i) <$> resolveName n
renameTerm (Lam nsts body i) = do
  depth <- view reVarDepth
  let (pns, ts) = NE.unzip nsts
      len = fromIntegral (NE.length nsts)
      newDepth = depth + len
      ixs = NE.fromList [depth .. newDepth - 1]
      ns = rawParsedName <$> pns
  let m = Map.fromList $ NE.toList $ NE.zip ns (NBound <$> ixs)
      ns' = NE.zipWith (\ix n -> Name n (NBound ix)) ixs ns
  term' <- local (inEnv m newDepth) (renameTerm body)
  pure (Lam (NE.zip ns' ts) term' i)
  where
  inEnv m depth =
    over reBinds (Map.union m) .
    set reVarDepth depth
renameTerm (Let name mt e1 e2 i) = do
  depth <- view reVarDepth
  let rawName = rawParsedName name
      name' = Name rawName (NBound depth)
      inEnv = over reVarDepth succ .
              over reBinds (Map.insert rawName (NBound depth))
  e1' <- renameTerm e1
  e2' <- local inEnv (renameTerm e2)
  pure (Let name' mt e1' e2' i)
renameTerm (App fn apps i) = do
  fn' <- renameTerm fn
  apps' <- traverse renameTerm apps
  pure (App fn' apps' i)
renameTerm (Sequence e1 e2 i) = do
  Sequence <$> renameTerm e1 <*> renameTerm e2 <*> pure i
renameTerm (Builtin b i) = pure (Builtin b i)
renameTerm (Constant l i) =
  pure (Constant l i)
renameTerm (ListLit v i) = do
  ListLit <$> traverse renameTerm v <*> pure i
renameTerm (Try e1 e2 i) = do
  Try <$> renameTerm e1 <*> renameTerm e2 <*> pure i
renameTerm (Error e i) = pure (Error e i)
-- renameTerm (ObjectLit o i) =
--   ObjectLit <$> traverse renameTerm o <*> pure i
-- renameTerm (ObjectOp o i) =
--   ObjectOp <$> traverse renameTerm o <*> pure i

renameDefun
  :: Defun ParsedName b i
  -> RenamerM cb ci (Defun Name b i)
renameDefun (Defun n dty term i) = do
  -- Todo: put type variables in scope here, if we want to support polymorphism
  term' <- renameTerm term
  pure (Defun n dty term' i)

renameDefConst
  :: DefConst ParsedName b i
  -> RenamerM cb ci (DefConst Name b i)
renameDefConst (DefConst n mty term i) = do
  -- Todo: put type variables in scope here, if we want to support polymorphism
  term' <- renameTerm term
  pure (DefConst n mty term' i)

-- renameDefCap
--   :: DefCap ParsedName builtin info
--   -> RenamerM cb ci (DefCap Name builtin info)
-- renameDefCap (DefCap name args term capType ty i) = do
--   term' <- renameTerm term
--   capType' <- traverse resolveName capType
--   pure (DefCap name args term' capType' ty i)

renameDef
  :: Def ParsedName b i
  -> RenamerM cb ci (Def Name b i)
renameDef = \case
  Dfun d -> Dfun <$> renameDefun d
  DConst d -> DConst <$> renameDefConst d
  -- DCap d -> DCap <$> renameDefCap d

resolveName :: ParsedName -> RenamerM b i Name
resolveName = \case
  BN b -> resolveBare b
  QN q -> resolveQualified q

-- not in immediate binds, so it must be in the module
-- Todo: resolve module ref within this model
-- Todo: hierarchical namespace search
resolveBare :: BareName -> RenamerM cb ci Name
resolveBare (BareName bn) = views reBinds (Map.lookup bn) >>= \case
  Just nk -> case nk of
    NBound d -> do
      depth <- view reVarDepth
      pure (Name bn (NBound (depth - d - 1)))
    _ -> pure (Name bn nk)
  Nothing -> uses (rsLoaded . loToplevel) (Map.lookup bn) >>= \case
    Just fqn -> pure (Name bn (NTopLevel (_fqModule fqn) (_fqHash fqn)))
    Nothing -> fail $ "unbound free variable " <> show bn

-- resolveBareName' :: Text -> RenamerM b i Name
-- resolveBareName' bn = views reBinds (Map.lookup bn) >>= \case
--   Just irnk -> pure (Name bn irnk)
--   Nothing -> fail $ "Expected identifier " <> T.unpack bn <> " in scope"

resolveQualified :: QualifiedName -> RenamerM b i Name
resolveQualified (QualifiedName qn qmn) = do
  uses rsModuleBinds (Map.lookup qmn) >>= \case
    Just binds -> case Map.lookup qn binds of
      Just irnk -> pure (Name qn irnk)
      Nothing -> fail "bound module has no such member"
    Nothing -> lookupModuleMember qmn qn

-- | Todo: support imports
renameModule
  :: Module ParsedName b i
  -> RenamerM cb ci (Module Name b i)
renameModule (Module mname defs blessed imp implements mhash) = do
  let rawDefNames = defName <$> defs
      defMap = Map.fromList $ (, NTopLevel mname mhash) <$> rawDefNames
      fqns = Map.fromList $ (\n -> (n, FullyQualifiedName mname n mhash)) <$> rawDefNames
  -- `maybe all of this next section should be in a block laid out by the
  -- `locally reBinds`
  rsModuleBinds %= Map.insert mname defMap
  rsLoaded . loToplevel %= Map.union fqns
  defs' <- locally reBinds (Map.union defMap) $ traverse renameDef defs
  let scc = mkScc <$> defs'
  defs'' <- forM (stronglyConnComp scc) \case
    AcyclicSCC d -> pure d
    CyclicSCC d -> fail $ "Functions: " <> show (defName  <$> d) <> " form a cycle"
  -- mgov' <- locally reBinds (Map.union defMap) $ traverse (resolveBareName' . rawParsedName) mgov
  pure (Module mname defs'' blessed imp implements mhash)
  where
  mkScc def = (def, defName def, Set.toList (defSCC mname def))

runRenamerT
  :: RenamerState b i
  -> RenamerEnv b i
  -> RenamerM b i a
  -> IO (a, RenamerState b i)
runRenamerT st env (RenamerT act) = runReaderT (runStateT act st) env

reStateFromLoaded :: Loaded b i -> RenamerState b i
reStateFromLoaded loaded = RenamerState mbinds loaded Set.empty
  where
  mbind md = let
    m = _mdModule md
    depNames = Term.defName <$> Term._mDefs m
    in Map.fromList $ (,NTopLevel (Term._mName m) (Term._mHash m)) <$> depNames
  mbinds = fmap mbind (_loModules loaded)

loadedBinds :: Loaded b i -> Map Text NameKind
loadedBinds loaded =
  let f fqn = NTopLevel (_fqModule fqn) (_fqHash fqn)
  in f <$> _loToplevel loaded

runDesugar'
  :: PactDb b i
  -> Loaded b i
  -> RenamerM b i a
  -> IO (DesugarOutput b i a)
runDesugar' pdb loaded act = do
  let reState = reStateFromLoaded loaded
      rTLBinds = loadedBinds loaded
      rEnv = RenamerEnv rTLBinds 0 pdb
  (renamed, RenamerState _ loaded' deps) <- runRenamerT reState rEnv act
  pure (DesugarOutput renamed loaded' deps)

runDesugarTerm
  :: (DesugarTerm term b' i)
  => PactDb b i
  -> Loaded b i
  -> term
  -> IO (DesugarOutput b i (Term Name b' i))
runDesugarTerm pdb loaded e = let
  desugared = desugarTerm e
  in runDesugar' pdb loaded (renameTerm desugared)

runDesugarModule'
  :: (DesugarTerm term b' i)
  => PactDb b i
  -> Loaded b i
  -> Common.Module term i
  -> IO (DesugarOutput b i (Module Name b' i))
runDesugarModule' pdb loaded m  = let
  desugared = desugarModule m
  in runDesugar' pdb loaded (renameModule desugared)

-- runDesugarModule
--   :: (DesugarTerm term b' i)
--   => Loaded b i
--   -> Common.Module term i
--   -> IO (DesugarOutput b i (Module Name TypeVar b' i))
-- runDesugarModule loaded = runDesugarModule' loaded 0

runDesugarTopLevel
  :: (DesugarTerm term b' i)
  => PactDb b i
  -> Loaded b i
  -> Common.TopLevel term i
  -> IO (DesugarOutput b i (TopLevel Name b' i))
runDesugarTopLevel pdb loaded = \case
  Common.TLModule m -> over dsOut TLModule <$> runDesugarModule' pdb loaded m
  Common.TLTerm e -> over dsOut TLTerm <$> runDesugarTerm pdb loaded e

-- runDesugarTopLevel
--   :: (DesugarTerm term b' i)
--   => PactDb b i
--   -> Loaded b i
--   -> Common.TopLevel term i
--   -> IO (DesugarOutput b i (TopLevel Name b' i))
-- runDesugarTopLevel pdb loaded = runDesugarTopLevel' pdb loaded


runDesugarTermLisp
  :: (DesugarBuiltin b')
  => PactDb b i
  -> Loaded b i
  -> Lisp.Expr i
  -> IO (DesugarOutput b i (Term Name b' i))
runDesugarTermLisp = runDesugarTerm

runDesugarTopLevelLisp
  :: (DesugarBuiltin b')
  => PactDb b i
  -> Loaded b i
  -> Common.TopLevel (Lisp.Expr i) i
  -> IO (DesugarOutput b i (TopLevel Name b' i))
runDesugarTopLevelLisp = runDesugarTopLevel

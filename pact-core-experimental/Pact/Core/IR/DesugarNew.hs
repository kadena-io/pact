module Pact.Core.IR.Desugarnew where

instance DesugarBuiltin b => DesugarTerm (New.Expr ParsedName i) b i where
  desugarTerm = desugarNewTerm

-----------------------------------------------------------
-- Desugaring for new syntax
-----------------------------------------------------------
defunToLetNew :: Common.Defun (New.Expr ParsedName i) i -> New.Expr ParsedName i
defunToLetNew = \case
  Common.Defun defname args retTy body i ->
    let lamName = BN (BareName defname)
    in case args of
      [] ->
        let defTy = Common.TyFun Common.TyUnit retTy
            lamBody = New.Lam lamName (("#unitArg", Just Common.TyUnit):| []) body i
        in New.Let defname (Just defTy) lamBody i
      x:xs ->
        let args' = x:|xs
            defTy = foldr Common.TyFun retTy (Common._argType <$> args')
            lamArgs = (\(Common.Arg n ty) -> (n, Just ty)) <$> args'
            lamBody = New.Lam lamName lamArgs body i
        in New.Let defname (Just defTy) lamBody i


desugarNewTerm :: forall b i. DesugarBuiltin b => New.Expr ParsedName i -> Term ParsedName b i
desugarNewTerm = \case
  New.Var (BN n) i | isReservedNative (_bnName n) ->
    Builtin (reservedNatives Map.! _bnName n) i
  New.Var n i -> Var n i
  New.Block (h :| hs) _ ->
    unLetBlock h hs
  New.Let name mt expr i -> let
    name' = BN (BareName  name)
    expr' = desugarNewTerm expr
    mt' = desugarType <$> mt
    in Let name' mt' expr' (Constant LUnit i) i
  New.NestedDefun d _ ->
    desugarNewTerm (defunToLetNew d)
  New.LetIn name mt e1 e2 i -> let
    name' = BN (BareName  name)
    e1' = desugarNewTerm e1
    mt' = desugarType <$> mt
    e2' = desugarNewTerm e2
    in Let name' mt' e1' e2' i
  New.Lam _name nsts body i -> let
    (ns, ts) = NE.unzip nsts
    ns' = BN . BareName <$> ns
    ts' = fmap desugarType <$> ts
    body' = desugarNewTerm body
    in Lam (NE.zip ns' ts') body' i
  New.If cond e1 e2 i -> let
    cond' = desugarNewTerm cond
    e1' = suspend i e1
    e2' = suspend i e2
    in App (Builtin builtinIf i) (cond' :| [e1', e2']) i
  New.App e [] i -> let
    e' = desugarNewTerm e
    body = Constant LUnit i :| []
    in App e' body i
  New.App e (h:hs) i -> let
    e' = desugarNewTerm e
    h' = desugarNewTerm h
    hs' = fmap desugarNewTerm hs
    in App e' (h' :| hs') i
  New.BinaryOp bop e1 e2 i -> let
    e1' = desugarNewTerm e1
    e2' = desugarNewTerm e2
    in App (Builtin (desugarBinary bop) i) (e1' :| [e2']) i
  New.UnaryOp uop e1 i -> let
    e1' = desugarNewTerm e1
    in App (Builtin (desugarUnary uop) i) (e1' :| []) i
  New.List e1 i ->
    ListLit (V.fromList (desugarNewTerm <$> e1)) i
  New.Constant l i ->
    Constant l i
  New.Object objs i ->
    ObjectLit (desugarNewTerm <$> objs) i
  New.ObjectOp o i ->
    ObjectOp (desugarNewTerm <$> o) i
  where
  isReservedNative n =
    Map.member n (reservedNatives @b)
  suspend i e = let
    name = BN (BareName "#ifArg")
    e' = desugarNewTerm e
    in Lam ((name, Just TyUnit) :| []) e' i
  unLetBlock (New.NestedDefun d _) rest = do
    unLetBlock (defunToLetNew d) rest
  unLetBlock (New.Let name mt expr i) (h:hs) = let
    name' = BN (BareName name)
    expr' = desugarNewTerm expr
    mt' = desugarType <$> mt
    e2 = unLetBlock h hs
    in Let name' mt' expr' e2 i
  unLetBlock other l = case l of
    h:hs -> let
      other' = desugarNewTerm other
      in case unLetBlock h hs of
        Block nel' i' ->
          Block (NE.cons other' nel') i'
        t -> Block (other' :| [t]) (other' ^. termInfo)
    [] -> desugarNewTerm other


runDesugarTermNew
  :: (DesugarBuiltin b')
  => PactDb b i
  -> Loaded b i
  -> New.Expr ParsedName i
  -> IO (DesugarOutput b i (Term Name b' i))
runDesugarTermNew = runDesugarTerm

runDesugarTopLevelNew
  :: (DesugarBuiltin b')
  => PactDb b i
  -> Loaded b i
  -> Common.TopLevel (New.Expr ParsedName i) i
  -> IO (DesugarOutput b i (TopLevel Name b' i))
runDesugarTopLevelNew = runDesugarTopLevel

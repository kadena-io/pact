

isUnsupportedOperator :: Text -> Maybe Text
isUnsupportedOperator s = if isUnsupported then Just s else Nothing
  where
    isUnsupported = Set.member s $ Set.fromList ["log", "ln", "ceiling", "floor", "mod"]

isInsertOrUpdate :: Fun Node -> Maybe Text
isInsertOrUpdate (NativeFunc "insert") = Just "insert"
isInsertOrUpdate (NativeFunc "update") = Just "update"
isInsertOrUpdate _ = Nothing

ofPrimType :: Node -> Maybe PrimType
ofPrimType (Node _ (TyPrim ty)) = Just ty
ofPrimType _ = Nothing

pattern RawTableName :: Text -> AST Node
pattern RawTableName t <- (Table (Node (TcId _ t _) _))

pattern OfPrimType :: PrimType -> Node
pattern OfPrimType pType <- (ofPrimType -> Just pType)

pattern AST_UnsupportedOp :: forall a. Text -> AST a
pattern AST_UnsupportedOp s <-
  (App _ (NativeFunc (isUnsupportedOperator -> Just s)) _ )

pattern AST_InsertOrUpdate :: Node
                           -> Text
                           -> Text
                           -> AST Node
                           -> TcId
                           -> [(AST Node, AST Node)]
                           -> AST Node
pattern AST_InsertOrUpdate node' fnName' table' key' tcId' kvs' <-
  (App node' (isInsertOrUpdate -> (Just fnName')) [RawTableName table', key', AST_Obj (Node tcId' _) kvs'])

pattern UserFunc :: forall a. Text -> [Named a] -> [AST a] -> Fun a
pattern UserFunc name' args bdy <- (FDefun _ name' _ args bdy _)

pattern NativeFuncSpecial :: forall a. Text -> AST a -> Fun a
pattern NativeFuncSpecial f bdy <- (FNative _ f _ (Just (_,SBinding bdy)))

pattern AST_UFun :: forall a. Text -> a -> [AST a] -> [AST a] -> AST a
pattern AST_UFun name' node' bdy' args' <-
  (App node' (UserFunc name' _ bdy') args')

pattern AST_WithRead :: Node
                     -> Text
                     -> AST Node
                     -> [(Named Node, AST Node)]
                     -> [AST Node]
                     -> AST Node
pattern AST_WithRead node' table' key' bindings' bdy' <-
  (App node'
       (NativeFuncSpecial "with-read" (AST_Binding _ bindings' bdy'))
       [RawTableName table', key'])

pattern AST_Bind :: forall a. AST a
pattern AST_Bind <- (App _ (NativeFuncSpecial "bind" _) _)

pattern AST_Enforce :: forall a. a -> AST a -> Text -> AST a
pattern AST_Enforce node' app' msg' <-
  (App node' (NativeFunc "enforce") [app', AST_Lit (LString msg')])

pattern AST_EnforceKeyset :: forall a. a -> Text -> AST a
pattern AST_EnforceKeyset node' keyset' <-
  (App node' (NativeFunc "enforce-keyset") [AST_Lit (LString keyset')])

pattern AST_Binding :: forall a. a -> [(Named a, AST a)] -> [AST a] -> AST a
pattern AST_Binding node' bindings' bdy' <- (Binding node' bindings' bdy' _)

pattern AST_Format :: forall a. a -> Text -> [AST a] -> AST a
pattern AST_Format node' fmtStr' args' <-
  (App node' (NativeFunc "format") (AST_Lit (LString fmtStr'):args'))

pattern AST_Read :: forall a. AST a
pattern AST_Read <- (App _ (NativeFunc "read") _)

pattern AST_Obj :: forall a. a -> [(AST a, AST a)] -> AST a
pattern AST_Obj objNode kvs <- (Object objNode kvs)

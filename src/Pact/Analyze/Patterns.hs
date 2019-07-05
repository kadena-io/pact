{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Pattern synonym definitions for translation from typechecked 'AST' to
-- 'Term'.
module Pact.Analyze.Patterns where

import           Control.Lens         ((^?))
import           Data.Maybe           (isJust)
import           Data.Text            (Text)

import qualified Pact.Types.Lang      as Lang
import           Pact.Types.Exp       (Literal (LString))
import           Pact.Types.Typecheck (AST(..), AstBindType (..), Fun (FDefun, FNative),
                                       Named, Node, PrimValue (PrimLit),
                                       Special (SBinding), aNode, aTy, YieldResume)
import qualified Pact.Types.Typecheck as TC
import           Pact.Analyze.Feature
import           Pact.Analyze.Types   (arithOpP, comparisonOpP, isGuardTy,
                                       logicalOpP, roundingLikeOpP,
                                       unaryArithOpP)

ofBasicOp :: Text -> Maybe Text
ofBasicOp s = if isBasicOp then Just s else Nothing
  where
    isBasicOp
      =  s == SModulus
      || isJust (toOp arithOpP s)
      || isJust (toOp unaryArithOpP s)
      || isJust (toOp comparisonOpP s)
      || isJust (toOp logicalOpP s)
      || isJust (toOp roundingLikeOpP s)

-- helper patterns
pattern NativeFunc :: forall a. Text -> Fun a
pattern NativeFunc f <- FNative f _ _

-- compileNode's Patterns

pattern AST_InlinedApp :: Lang.ModuleName -> Text -> [(Named a, AST a)] -> [AST a] -> AST a
pattern AST_InlinedApp modName funName bindings body <-
  ASTApp _ (FDefun _ modName funName _ _ _ [ASTBinding _ bindings body AstBindInlinedCallArgs]) _args

pattern AST_Let :: forall a. [(Named a, AST a)] -> [AST a] -> AST a
pattern AST_Let bindings body <- ASTBinding _ bindings body AstBindLet

pattern AST_BindSchema :: forall a. a -> [(Named a, AST a)] -> a -> [AST a] -> AST a
pattern AST_BindSchema node bindings schema body <- ASTBinding node bindings body (AstBindSchema schema)

pattern AST_Var :: forall a. a -> AST a
pattern AST_Var var <- ASTVar var

pattern AST_Lit :: forall a. Literal -> AST a
pattern AST_Lit lit <- ASTPrim _ (PrimLit lit)

pattern AST_NegativeVar :: forall a. a -> AST a
pattern AST_NegativeVar var <- ASTApp _ (NativeFunc "-") [AST_Var var]

pattern AST_NegativeLit :: forall a. Literal -> AST a
pattern AST_NegativeLit lit <- ASTApp _ (NativeFunc "-") [AST_Lit lit]

pattern AST_NFun :: forall a. a -> Text -> [AST a] -> AST a
pattern AST_NFun node fn args <- ASTApp node (NativeFunc fn) args

pattern AST_NFun_Basic :: forall a. Text -> [AST a] -> AST a
pattern AST_NFun_Basic fn args <- AST_NFun _ (ofBasicOp -> Just fn) args

pattern AST_If :: forall a. a -> AST a -> AST a -> AST a -> AST a
pattern AST_If node cond then' else' <-
  ASTApp node (NativeFunc "if") [cond, then', else']

pattern AST_StringLit :: forall a. Text -> AST a
pattern AST_StringLit str <- AST_Lit (LString str)

pattern AST_KeysetRefGuard :: forall a. AST a -> AST a
pattern AST_KeysetRefGuard name <-
  ASTApp _node (NativeFunc "keyset-ref-guard") [name]

pattern AST_CreatePactGuard :: forall a. AST a -> AST a
pattern AST_CreatePactGuard name <-
  ASTApp _node (NativeFunc "create-pact-guard") [name]

pattern AST_CreateUserGuard :: forall a. AST a -> AST a -> AST a
pattern AST_CreateUserGuard obj funName <-
  ASTApp _node (NativeFunc "create-user-guard") [obj, funName]

pattern AST_Enforce :: forall a. a -> AST a -> AST a
pattern AST_Enforce node cond <-
  ASTApp node (NativeFunc "enforce") (cond:_rest)

pattern AST_ReadKeyset :: forall a. AST a -> AST a
pattern AST_ReadKeyset name <-
  ASTApp _node (NativeFunc "read-keyset") [name]

pattern AST_ReadDecimal :: forall a. AST a -> AST a
pattern AST_ReadDecimal name <-
  ASTApp _node (NativeFunc "read-decimal") [name]

pattern AST_ReadInteger :: forall a. AST a -> AST a
pattern AST_ReadInteger name <-
  ASTApp _node (NativeFunc "read-integer") [name]

pattern AST_ReadMsg :: forall a. AST a -> AST a
pattern AST_ReadMsg name <-
  ASTApp _node (NativeFunc "read-msg") [name]

pattern AST_PactId :: AST a
pattern AST_PactId <- ASTApp _node (NativeFunc "pact-id") []

typeSatisfying :: (Lang.Type TC.UserType -> Bool) -> AST Node -> Maybe (AST Node)
typeSatisfying test x = case x ^? aNode.aTy of
  Just ty | test ty -> Just x
  _       -> Nothing

ofType :: Lang.Type TC.UserType -> AST Node -> Maybe (AST Node)
ofType ty = typeSatisfying (== ty)

pattern AST_EnforceKeyset_Str :: AST Node -> AST Node
pattern AST_EnforceKeyset_Str str <-
  ASTApp _node (NativeFunc "enforce-keyset")
    [ofType (Lang.TyPrim Lang.TyString) -> Just str]

pattern AST_EnforceKeyset_Guard :: AST Node -> AST Node
pattern AST_EnforceKeyset_Guard g <-
  ASTApp _node (NativeFunc "enforce-keyset") [typeSatisfying isGuardTy -> Just g]

pattern AST_EnforceGuard_Str :: AST Node -> AST Node
pattern AST_EnforceGuard_Str str <-
  ASTApp _node (NativeFunc "enforce-guard")
    [ofType (Lang.TyPrim Lang.TyString) -> Just str]

pattern AST_EnforceGuard_Guard :: AST Node -> AST Node
pattern AST_EnforceGuard_Guard g <-
  ASTApp _node (NativeFunc "enforce-guard") [typeSatisfying isGuardTy -> Just g]

pattern AST_EnforceOne :: forall a. a -> [AST a] -> AST a
pattern AST_EnforceOne node enforces <-
  ASTApp node (NativeFunc "enforce-one") [_, ASTList _ enforces]

pattern AST_Format :: forall a. AST a -> [AST a] -> AST a
pattern AST_Format str vars <-
  ASTApp _node (NativeFunc "format") [str, ASTList _ vars]

pattern AST_FormatTime :: forall a. AST a -> AST a -> AST a
pattern AST_FormatTime str time <-
  ASTApp _node (NativeFunc "format-time") [str, time]

pattern AST_Time :: forall a. AST a -> AST a
pattern AST_Time timeStr <- ASTApp _node (NativeFunc "time") [timeStr]

pattern AST_ParseTime :: forall a. AST a -> AST a -> AST a
pattern AST_ParseTime formatStr timeStr <-
  ASTApp _node (NativeFunc "parse-time") [formatStr, timeStr]

pattern AST_Hash :: forall a. AST a -> AST a
pattern AST_Hash val <- ASTApp _node (NativeFunc "hash") [val]

pattern AST_AddTime :: forall a. AST a -> AST a -> AST a
pattern AST_AddTime time seconds <- ASTApp _ (NativeFunc STemporalAddition) [time, seconds]

pattern AST_Days :: forall a. AST a -> AST a
pattern AST_Days days <- ASTApp _ (NativeFunc "days") [days]

pattern AST_Hours :: forall a. AST a -> AST a
pattern AST_Hours hours <- ASTApp _ (NativeFunc "hours") [hours]

pattern AST_Minutes :: forall a. AST a -> AST a
pattern AST_Minutes minutes <- ASTApp _ (NativeFunc "minutes") [minutes]

pattern AST_WithRead
  :: Node
  -> Text
  -> AST Node
  -> [(Named Node, AST Node)]
  -> Node
  -> [AST Node]
  -> AST Node
pattern AST_WithRead node table key bindings schema body <-
  ASTApp node
      (NativeFuncSpecial "with-read" (AST_BindSchema _ bindings schema body))
      [ShortTableName table, key]

pattern AST_WithDefaultRead
  :: Node                     -- ^ node
  -> Text                     -- ^ table name
  -> AST Node                 -- ^ key
  -> [(Named Node, AST Node)] -- ^ table bindings
  -> Node                     -- ^ schema node
  -> AST Node                 -- ^ default node
  -> [AST Node]               -- ^ body
  -> AST Node
pattern AST_WithDefaultRead node table key bindings schema default' body <-
  ASTApp node (NativeFuncSpecial "with-default-read" (AST_BindSchema _ bindings schema body))
           [ShortTableName table, key, default']

pattern AST_Bind
  :: Node
  -> AST Node
  -> [(Named Node, AST Node)]
  -> Node
  -> [AST Node]
  -> AST Node
pattern AST_Bind node object bindings schema body <-
  ASTApp node
      (NativeFuncSpecial "bind" (AST_BindSchema _ bindings schema body))
      [object]

pattern AST_WithCapability
  :: AST Node
  -> [AST Node]
  -> AST Node
pattern AST_WithCapability app body <-
  ASTApp _node
      (NativeFuncSpecial "with-capability" (ASTList _ body))
      [app]

pattern AST_Resume
  :: Node -> [(Named Node, AST Node)] -> Node -> [AST Node] -> AST Node
pattern AST_Resume node bindings schema body <-
  ASTApp node
      (NativeFuncSpecial "resume" (AST_BindSchema _ bindings schema body))
      []

pattern AST_RequireCapability :: Node -> AST Node -> AST Node
pattern AST_RequireCapability node app <-
  ASTApp node (NativeFunc "require-capability") [app]

pattern AST_ComposeCapability :: AST Node -> AST Node
pattern AST_ComposeCapability app <-
  ASTApp _node (NativeFunc "compose-capability") [app]

-- pattern RawTableName :: Text -> AST Node
-- pattern RawTableName t <- Table (Node (TcId _ t _) _) _

pattern ShortTableName :: Text -> AST Node
pattern ShortTableName tn <- ASTTable _node (Lang.TableName tn)

pattern NativeFuncSpecial :: forall a. Text -> AST a -> Fun a
pattern NativeFuncSpecial f bdy <- FNative f _ (Just (_,SBinding bdy))

pattern AST_Read :: Node -> Text -> AST Node -> AST Node
pattern AST_Read node tn key <- ASTApp node (NativeFunc "read") [ShortTableName tn, key]

pattern AST_ReadCols :: Node -> Text -> AST Node -> [AST Node] -> AST Node
pattern AST_ReadCols node tn key columns
  <- ASTApp node (NativeFunc "read") [ShortTableName tn, key, ASTList _ columns]

pattern AST_At :: a -> AST a -> AST a -> AST a
pattern AST_At node colName obj <- ASTApp node (NativeFunc SObjectProjection) [colName, obj]

pattern AST_Contains :: a -> AST a -> AST a -> AST a
pattern AST_Contains node val collection <- ASTApp node (NativeFunc SContains) [val, collection]

pattern AST_Drop :: a -> AST a -> AST a -> AST a
pattern AST_Drop node num collection <- ASTApp node (NativeFunc SListDrop) [num, collection]

pattern AST_Reverse :: a -> AST a -> AST a
pattern AST_Reverse node list <- ASTApp node (NativeFunc SReverse) [list]

pattern AST_Sort :: a -> AST a -> AST a
pattern AST_Sort node list <- ASTApp node (NativeFunc SSort) [list]

pattern AST_Take :: a -> AST a -> AST a -> AST a
pattern AST_Take node num list <- ASTApp node (NativeFunc SListTake) [num, list]

pattern AST_MakeList :: a -> AST a -> AST a -> AST a
pattern AST_MakeList node num val <- ASTApp node (NativeFunc SMakeList) [num, val]

pattern AST_Obj :: forall a. a -> Lang.ObjectMap (AST a) -> AST a
pattern AST_Obj objNode kvs <- ASTObject objNode kvs

pattern AST_List :: forall a. a -> [AST a] -> AST a
pattern AST_List node elems <- ASTList node elems

pattern AST_Step :: a -> Maybe (AST a) -> AST a -> Maybe (AST a) -> Maybe (YieldResume a) -> AST a
pattern AST_Step node entity exec rollback yr <- ASTStep node entity exec rollback yr

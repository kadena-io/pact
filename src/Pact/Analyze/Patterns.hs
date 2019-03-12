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
import           Pact.Types.Runtime   (Literal (LString))
import           Pact.Types.Typecheck (AST (App, Binding, List, Object, Prim, Step, Table),
                                       AstBindType (..), Fun (FDefun, FNative),
                                       Named, Node, PrimValue (PrimLit),
                                       Special (SBinding), aNode, aTy)
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
pattern NativeFunc f <- FNative _ f _ _

-- compileNode's Patterns

pattern AST_InlinedApp :: Lang.ModuleName -> Text -> [(Named a, AST a)] -> [AST a] -> AST a
pattern AST_InlinedApp modName funName bindings body <-
  App _ (FDefun _ modName funName _ _ [Binding _ bindings body AstBindInlinedCallArgs]) _args

pattern AST_Let :: forall a. [(Named a, AST a)] -> [AST a] -> AST a
pattern AST_Let bindings body <- Binding _ bindings body AstBindLet

pattern AST_BindSchema :: forall a. a -> [(Named a, AST a)] -> a -> [AST a] -> AST a
pattern AST_BindSchema node bindings schema body <- Binding node bindings body (AstBindSchema schema)

pattern AST_Var :: forall a. a -> AST a
pattern AST_Var var <- TC.Var var

pattern AST_Lit :: forall a. Literal -> AST a
pattern AST_Lit lit <- Prim _ (PrimLit lit)

pattern AST_NegativeVar :: forall a. a -> AST a
pattern AST_NegativeVar var <- App _ (NativeFunc "-") [AST_Var var]

pattern AST_NegativeLit :: forall a. Literal -> AST a
pattern AST_NegativeLit lit <- App _ (NativeFunc "-") [AST_Lit lit]

pattern AST_NFun :: forall a. a -> Text -> [AST a] -> AST a
pattern AST_NFun node fn args <- App node (NativeFunc fn) args

pattern AST_NFun_Basic :: forall a. Text -> [AST a] -> AST a
pattern AST_NFun_Basic fn args <- AST_NFun _ (ofBasicOp -> Just fn) args

pattern AST_If :: forall a. a -> AST a -> AST a -> AST a -> AST a
pattern AST_If node cond then' else' <-
  App node (NativeFunc "if") [cond, then', else']

pattern AST_StringLit :: forall a. Text -> AST a
pattern AST_StringLit str <- AST_Lit (LString str)

pattern AST_KeysetRefGuard :: forall a. AST a -> AST a
pattern AST_KeysetRefGuard name <-
  App _node (NativeFunc "keyset-ref-guard") [name]

pattern AST_CreatePactGuard :: forall a. AST a -> AST a
pattern AST_CreatePactGuard name <-
  App _node (NativeFunc "create-pact-guard") [name]

pattern AST_CreateUserGuard :: forall a. AST a -> AST a -> AST a
pattern AST_CreateUserGuard obj funName <-
  App _node (NativeFunc "create-user-guard") [obj, funName]

pattern AST_Enforce :: forall a. a -> AST a -> AST a
pattern AST_Enforce node cond <-
  App node (NativeFunc "enforce") (cond:_rest)

pattern AST_ReadKeyset :: forall a. AST a -> AST a
pattern AST_ReadKeyset name <-
  App _node (NativeFunc "read-keyset") [name]

pattern AST_ReadDecimal :: forall a. AST a -> AST a
pattern AST_ReadDecimal name <-
  App _node (NativeFunc "read-decimal") [name]

pattern AST_ReadInteger :: forall a. AST a -> AST a
pattern AST_ReadInteger name <-
  App _node (NativeFunc "read-integer") [name]

pattern AST_ReadMsg :: forall a. AST a -> AST a
pattern AST_ReadMsg name <-
  App _node (NativeFunc "read-msg") [name]

pattern AST_PactId :: AST a
pattern AST_PactId <- App _node (NativeFunc "pact-id") []

typeSatisfying :: (Lang.Type TC.UserType -> Bool) -> AST Node -> Maybe (AST Node)
typeSatisfying test x = case x ^? aNode.aTy of
  Just ty | test ty -> Just x
  _       -> Nothing

ofType :: Lang.Type TC.UserType -> AST Node -> Maybe (AST Node)
ofType ty = typeSatisfying (== ty)

pattern AST_EnforceKeyset_Str :: AST Node -> AST Node
pattern AST_EnforceKeyset_Str str <-
  App _node (NativeFunc "enforce-keyset")
    [ofType (Lang.TyPrim Lang.TyString) -> Just str]

pattern AST_EnforceKeyset_Guard :: AST Node -> AST Node
pattern AST_EnforceKeyset_Guard g <-
  App _node (NativeFunc "enforce-keyset") [typeSatisfying isGuardTy -> Just g]

pattern AST_EnforceGuard_Str :: AST Node -> AST Node
pattern AST_EnforceGuard_Str str <-
  App _node (NativeFunc "enforce-guard")
    [ofType (Lang.TyPrim Lang.TyString) -> Just str]

pattern AST_EnforceGuard_Guard :: AST Node -> AST Node
pattern AST_EnforceGuard_Guard g <-
  App _node (NativeFunc "enforce-guard") [typeSatisfying isGuardTy -> Just g]

pattern AST_EnforceOne :: forall a. a -> [AST a] -> AST a
pattern AST_EnforceOne node enforces <-
  App node (NativeFunc "enforce-one") [_, List _ enforces]

pattern AST_Format :: forall a. AST a -> [AST a] -> AST a
pattern AST_Format str vars <-
  App _node (NativeFunc "format") [str, List _ vars]

pattern AST_FormatTime :: forall a. AST a -> AST a -> AST a
pattern AST_FormatTime str time <-
  App _node (NativeFunc "format-time") [str, time]

pattern AST_Time :: forall a. AST a -> AST a
pattern AST_Time timeStr <- App _node (NativeFunc "time") [timeStr]

pattern AST_ParseTime :: forall a. AST a -> AST a -> AST a
pattern AST_ParseTime formatStr timeStr <-
  App _node (NativeFunc "parse-time") [formatStr, timeStr]

pattern AST_Hash :: forall a. AST a -> AST a
pattern AST_Hash val <- App _node (NativeFunc "hash") [val]

pattern AST_AddTime :: forall a. AST a -> AST a -> AST a
pattern AST_AddTime time seconds <- App _ (NativeFunc STemporalAddition) [time, seconds]

pattern AST_Days :: forall a. AST a -> AST a
pattern AST_Days days <- App _ (NativeFunc "days") [days]

pattern AST_Hours :: forall a. AST a -> AST a
pattern AST_Hours hours <- App _ (NativeFunc "hours") [hours]

pattern AST_Minutes :: forall a. AST a -> AST a
pattern AST_Minutes minutes <- App _ (NativeFunc "minutes") [minutes]

pattern AST_WithRead
  :: Node
  -> Text
  -> AST Node
  -> [(Named Node, AST Node)]
  -> Node
  -> [AST Node]
  -> AST Node
pattern AST_WithRead node table key bindings schema body <-
  App node
      (NativeFuncSpecial "with-read" (AST_BindSchema _ bindings schema body))
      [ShortTableName table, key]

pattern AST_Bind
  :: Node
  -> AST Node
  -> [(Named Node, AST Node)]
  -> Node
  -> [AST Node]
  -> AST Node
pattern AST_Bind node object bindings schema body <-
  App node
      (NativeFuncSpecial "bind" (AST_BindSchema _ bindings schema body))
      [object]

pattern AST_WithCapability
  :: AST Node
  -> [AST Node]
  -> AST Node
pattern AST_WithCapability app body <-
  App _node
      (NativeFuncSpecial "with-capability" (List _ body))
      [app]

pattern AST_RequireCapability :: Node -> AST Node -> AST Node
pattern AST_RequireCapability node app <-
  App node (NativeFunc "require-capability") [app]

pattern AST_ComposeCapability :: AST Node -> AST Node
pattern AST_ComposeCapability app <-
  App _node (NativeFunc "compose-capability") [app]

-- pattern RawTableName :: Text -> AST Node
-- pattern RawTableName t <- Table (Node (TcId _ t _) _) _

pattern ShortTableName :: Text -> AST Node
pattern ShortTableName tn <- Table _node (Lang.TableName tn)

pattern NativeFuncSpecial :: forall a. Text -> AST a -> Fun a
pattern NativeFuncSpecial f bdy <- FNative _ f _ (Just (_,SBinding bdy))

pattern AST_Read :: Node -> Text -> AST Node -> AST Node
pattern AST_Read node tn key <- App node (NativeFunc "read") [ShortTableName tn, key]

pattern AST_ReadCols :: Node -> Text -> AST Node -> [AST Node] -> AST Node
pattern AST_ReadCols node tn key columns
  <- App node (NativeFunc "read") [ShortTableName tn, key, List _ columns]

pattern AST_At :: a -> AST a -> AST a -> AST a
pattern AST_At node colName obj <- App node (NativeFunc SObjectProjection) [colName, obj]

pattern AST_Contains :: a -> AST a -> AST a -> AST a
pattern AST_Contains node val collection <- App node (NativeFunc SContains) [val, collection]

pattern AST_Drop :: a -> AST a -> AST a -> AST a
pattern AST_Drop node num collection <- App node (NativeFunc SListDrop) [num, collection]

pattern AST_Reverse :: a -> AST a -> AST a
pattern AST_Reverse node list <- App node (NativeFunc SReverse) [list]

pattern AST_Sort :: a -> AST a -> AST a
pattern AST_Sort node list <- App node (NativeFunc SSort) [list]

pattern AST_Take :: a -> AST a -> AST a -> AST a
pattern AST_Take node num list <- App node (NativeFunc SListTake) [num, list]

pattern AST_MakeList :: a -> AST a -> AST a -> AST a
pattern AST_MakeList node num val <- App node (NativeFunc SMakeList) [num, val]

pattern AST_Obj :: forall a. a -> [(Lang.FieldKey, AST a)] -> AST a
pattern AST_Obj objNode kvs <- Object objNode kvs

pattern AST_List :: forall a. a -> [AST a] -> AST a
pattern AST_List node elems <- List node elems

pattern AST_Step :: AST a
pattern AST_Step <- Step {}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Pact.Native
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Pact builtins/standard library.
--

module Pact.Native
    ( natives
    , nativeDefs
    , moduleToMap
    , enforceDef
    , enforceOneDef
    , pactVersionDef
    , formatDef
    , strToIntDef
    , hashDef
    , ifDef
    , readDecimalDef
    , baseStrToInt
    , mapDef
    , foldDef
    , makeListDef
    , reverseDef
    , filterDef
    , sortDef
    , whereDef
    , composeDef
    , lengthDef
    , takeDef
    , dropDef
    , atDef
    ) where

import Control.Arrow hiding (app)
import Control.Lens hiding (parts,Fold,contains)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson hiding ((.=),Object)
import qualified Data.Attoparsec.Text as AP
import Data.ByteString.Lazy (toStrict)
import Data.Char (isHexDigit, digitToInt)
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V

import Pact.Eval
import Pact.Native.Capabilities
import Pact.Native.Db
import Pact.Native.Internal
import Pact.Native.Keysets
import Pact.Native.Ops
import Pact.Native.SPV
import Pact.Native.Time
import Pact.Parse
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Pretty hiding (list)
import Pact.Types.Runtime
import Pact.Types.Version

-- | All production native modules.
natives :: [NativeModule]
natives = [
  langDefs,
  dbDefs,
  timeDefs,
  opDefs,
  keyDefs,
  capDefs,
  spvDefs]


-- | Production native modules as a dispatch map.
nativeDefs :: HM.HashMap Name Ref
nativeDefs = mconcat $ map moduleToMap natives

moduleToMap :: NativeModule -> HM.HashMap Name Ref
moduleToMap = HM.fromList . map (((`Name` def) . asString) *** Direct) . snd


lengthDef :: NativeDef
lengthDef = defRNative "length" length' (funType tTyInteger [("x",listA)])
 ["(length [1 2 3])", "(length \"abcdefgh\")", "(length { \"a\": 1, \"b\": 2 })"]
 "Compute length of X, which can be a list, a string, or an object."

listA :: Type n
listA = mkTyVar "a" [TyList (mkTyVar "l" []),TyPrim TyString,TySchema TyObject (mkSchemaVar "o") def]

enforceDef :: NativeDef
enforceDef = defNative "enforce" enforce
  (funType tTyBool [("test",tTyBool),("msg",tTyString)])
  [ExecErrExample "(enforce (!= (+ 2 2) 4) \"Chaos reigns\")"]
  "Fail transaction with MSG if pure expression TEST is false. Otherwise, returns true."
  where

    enforce :: NativeFun e
    enforce i as = runSysOnly $ gasUnreduced i as $ mapM reduce as >>= enforce' i

    enforce' :: RNativeFun e
    enforce' i [TLiteral (LBool b') _,TLitString msg]
        | b' = return $ TLiteral (LBool True) def
        | otherwise = failTx (_faInfo i) $ pretty msg
    enforce' i as = argsError i as
    {-# INLINE enforce' #-}

enforceOneDef :: NativeDef
enforceOneDef =
  defNative "enforce-one" enforceOne (funType tTyBool [("msg",tTyString),("tests",TyList tTyBool)])
  ["(enforce-one \"Should succeed on second test\" [(enforce false \"Skip me\") (enforce (= (+ 2 2) 4) \"Chaos reigns\")])"]
  "Run TESTS in order (in pure context, plus keyset enforces). If all fail, fail transaction. Short-circuits on first success."
  where

    enforceOne :: NativeFun e
    enforceOne i as@[msg,TList conds _ _] = runReadOnly (_faInfo i) $
      gasUnreduced i as $ do
        msg' <- reduce msg >>= \m -> case m of
          TLitString s -> return s
          _ -> argsError' i as
        let tryCond r@Just {} _ = return r
            tryCond Nothing cond = catch
              (Just <$> reduce cond)
              (\(_ :: SomeException) -> return Nothing)
        r <- foldM tryCond Nothing conds
        case r of
          Nothing -> failTx (_faInfo i) $ pretty msg'
          Just b' -> return b'
    enforceOne i as = argsError' i as

pactVersionDef :: NativeDef
pactVersionDef = setTopLevelOnly $ defRNative "pact-version"
  (\_ _ -> return $ toTerm pactVersion)
  (funType tTyString [])
  ["(pact-version)"]
  "Obtain current pact build version."


formatDef :: NativeDef
formatDef =
  defRNative "format" format
  (funType tTyString [("template",tTyString),("vars",TyList TyAny)])
  ["(format \"My {} has {}\" [\"dog\" \"fleas\"])"]
  "Interpolate VARS into TEMPLATE using {}."
  where

    format :: RNativeFun e
    format i [TLitString s,TList es _ _] = do
      let parts = T.splitOn "{}" s
          plen = length parts
          rep (TLitString t) = t
          rep t = renderCompactText t
      if plen == 1
      then return $ tStr s
      else if plen - length es > 1
           then evalError' i "format: not enough arguments for template"
           else return $ tStr $
                foldl'
                  (\r (e,t) -> r <> rep e <> t)
                  (head parts)
                  (zip (V.toList es) (tail parts))
    format i as = argsError i as

strToIntDef :: NativeDef
strToIntDef = defRNative "str-to-int" strToInt
  (funType tTyInteger [("str-val", tTyString)] <>
   funType tTyInteger [("base", tTyInteger), ("str-val", tTyString)])
  ["(str-to-int 16 \"abcdef123456\")", "(str-to-int \"123456\")"]
  "Compute the integer value of STR-VAL in base 10, or in BASE if specified. STR-VAL must be <= 128 \
  \chars in length and BASE must be between 2 and 16. Each digit must be in the correct range for \
  \the base."

hashDef :: NativeDef
hashDef = defRNative "hash" hash' (funType tTyString [("value",a)])
  ["(hash \"hello\")", "(hash { 'foo: 1 })"]
  "Compute BLAKE2b 256-bit hash of VALUE represented in unpadded base64-url. \
  \Strings are converted directly while other values are \
  \converted using their JSON representation. Non-value-level arguments are not allowed."
  where
    hash' :: RNativeFun e
    hash' i as = case as of
      [TLitString s] -> go $ encodeUtf8 s
      [a'] -> enforcePactValue a' >>= \pv -> go $ toStrict $ encode pv
      _ -> argsError i as
      where go = return . tStr . asString . pactHash

ifDef :: NativeDef
ifDef = defNative "if" if' (funType a [("cond",tTyBool),("then",a),("else",a)])
  ["(if (= (+ 2 2) 4) \"Sanity prevails\" \"Chaos reigns\")"]
  "Test COND. If true, evaluate THEN. Otherwise, evaluate ELSE."
  where

    if' :: NativeFun e
    if' i as@[cond,then',else'] = gasUnreduced i as $ reduce cond >>= \case
               TLiteral (LBool c') _ -> reduce (if c' then then' else else')
               t -> evalError' i $ "if: conditional not boolean: " <> pretty t
    if' i as = argsError' i as


readDecimalDef :: NativeDef
readDecimalDef = defRNative "read-decimal" readDecimal
  (funType tTyDecimal [("key",tTyString)])
  [LitExample "(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))"]
  "Parse KEY string or number value from top level of message data body as decimal."
  where

    readDecimal :: RNativeFun e
    readDecimal i [TLitString key] = do
      (ParsedDecimal a') <- parseMsgKey i "read-decimal" key
      return $ toTerm a'
    readDecimal i as = argsError i as

defineNamespaceDef :: NativeDef
defineNamespaceDef = setTopLevelOnly $ defRNative "define-namespace" defineNamespace
  (funType tTyString [("namespace", tTyString), ("guard", tTyGuard Nothing)])
  [LitExample "(define-namespace 'my-namespace (read-keyset 'my-keyset))"]
  "Create a namespace called NAMESPACE where ownership and use of the namespace is controlled by GUARD. \
  \If NAMESPACE is already defined, then the guard previously defined in NAMESPACE will be enforced, \
  \and GUARD will be rotated in its place."
  where
    defineNamespace :: RNativeFun e
    defineNamespace i as = case as of
      [TLitString nsn, TGuard g _] -> go i nsn g
      _ -> argsError i as

    go fi nsn g = do
      let name = NamespaceName nsn
          info = _faInfo fi
      mOldNs <- readRow info Namespaces name
      case mOldNs of
        Just ns'@(Namespace _ g') ->
          -- if namespace is defined, enforce old guard and rotate if policy allows
          enforceGuard fi g' >> enforcePolicy info ns' >> writeNamespace info name g
        Nothing -> writeNamespace info name g

    enforcePolicy info ns = do
      NamespacePolicy{..} <- view eeNamespacePolicy
      unless (_nsPolicy . Just $ ns) $ evalError info "Namespace definition not permitted"

    writeNamespace info n g =
      success ("Namespace defined: " <> asString n) $
      writeRow info Write Namespaces n (Namespace n g)

namespaceDef :: NativeDef
namespaceDef = setTopLevelOnly $ defRNative "namespace" namespace
  (funType tTyString [("namespace", tTyString)])
  [LitExample "(namespace 'my-namespace)"]
  "Set the current namespace to NAMESPACE. All expressions that occur in a current \
  \transaction will be contained in NAMESPACE, and once committed, may be accessed \
  \via their fully qualified name, which will include the namespace. Subsequent \
  \namespace calls in the same tx will set a new namespace for all declarations \
  \until either the next namespace declaration, or the end of the tx."
  where
    namespace :: RNativeFun e
    namespace i as = case as of
      [TLitString nsn] -> go i nsn
      _ -> argsError i as

    go fa ns = do
      let name = NamespaceName ns
          info = _faInfo fa

      mNs <- readRow info Namespaces name
      case mNs of
        Just n@(Namespace ns' g) -> do
          enforceGuard fa g
          success ("Namespace set to " <> (asString ns')) $
            evalRefs . rsNamespace .= (Just n)
        Nothing  -> evalError info $
          "namespace: '" <> pretty name <> "' not defined"

chainDataDef :: NativeDef
chainDataDef = defRNative "chain-data" chainData (funType obj [])
    ["(chain-data)"]
    "Get transaction public metadata. Returns an object with 'chain-id', 'block-height', \
    \'block-time', 'sender', 'gas-limit', 'gas-price', and 'gas-fee' fields."
  where
    chainData :: RNativeFun e
    chainData _ [] = do
      PublicData{..} <- view eePublicData

      let PublicMeta{..} = _pdPublicMeta

      let (ParsedInteger gl) = _pmGasLimit
          (ParsedDecimal gp) = _pmGasPrice

      pure $ toTObject TyAny def
        [ ("chain-id"    , toTerm _pmChainId    )
        , ("block-height", toTerm _pdBlockHeight)
        , ("block-time"  , toTerm _pdBlockTime  )
        , ("sender"      , toTerm _pmSender     )
        , ("gas-limit"   , toTerm gl            )
        , ("gas-price"   , toTerm gp            )
        ]
    chainData i as = argsError i as

mapDef :: NativeDef
mapDef = defNative "map" map'
  (funType (TyList a) [("app",lam b a),("list",TyList b)])
  ["(map (+ 1) [1 2 3])"]
  "Apply APP to each element in LIST, returning a new list of results."

foldDef :: NativeDef
foldDef = defNative "fold" fold'
  (funType a [("app",lam2 a b a),("init",a),("list",TyList b)])
  ["(fold (+) 0 [100 10 5])"]
  "Iteratively reduce LIST by applying APP to last result and element, starting with INIT."

makeListDef :: NativeDef
makeListDef = defRNative "make-list" makeList (funType (TyList a) [("length",tTyInteger),("value",a)])
  ["(make-list 5 true)"]
  "Create list by repeating VALUE LENGTH times."

reverseDef :: NativeDef
reverseDef = defRNative "reverse" reverse' (funType (TyList a) [("list",TyList a)])
  ["(reverse [1 2 3])"] "Reverse LIST."

filterDef :: NativeDef
filterDef = defNative "filter" filter'
  (funType (TyList a) [("app",lam a tTyBool),("list",TyList a)])
  ["(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])"]
  "Filter LIST by applying APP to each element. For each true result, the original value is kept."

sortDef :: NativeDef
sortDef = defRNative "sort" sort'
  (funType (TyList a) [("values",TyList a)] <>
   funType (TyList (tTyObject (mkSchemaVar "o"))) [("fields",TyList tTyString),("values",TyList (tTyObject (mkSchemaVar "o")))])
  ["(sort [3 1 2])", "(sort ['age] [{'name: \"Lin\",'age: 30} {'name: \"Val\",'age: 25}])"]
  "Sort a homogeneous list of primitive VALUES, or objects using supplied FIELDS list."

whereDef :: NativeDef
whereDef = defNative (specialForm Where) where'
  (funType tTyBool [("field",tTyString),("app",lam a tTyBool),("value",tTyObject (mkSchemaVar "row"))])
  ["(filter (where 'age (> 20)) [{'name: \"Mary\",'age: 30} {'name: \"Juan\",'age: 15}])"]
  "Utility for use in 'filter' and 'select' applying APP to FIELD in VALUE."

composeDef :: NativeDef
composeDef = defNative "compose" compose (funType c [("x",lam a b),("y", lam b c),("value",a)])
  ["(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])"]
  "Compose X and Y, such that X operates on VALUE, and Y on the results of X."

takeDef :: NativeDef
takeDef = defRNative "take" take' takeDrop
  [ "(take 2 \"abcd\")"
  , "(take (- 3) [1 2 3 4 5])"
  , "(take ['name] { 'name: \"Vlad\", 'active: false})"
  ]
  "Take COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, take from end."

dropDef :: NativeDef
dropDef = defRNative "drop" drop' takeDrop
  [ "(drop 2 \"vwxyz\")"
  , "(drop (- 2) [1 2 3 4 5])"
  , "(drop ['name] { 'name: \"Vlad\", 'active: false})"
  ]
  "Drop COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, drop from end."

atDef :: NativeDef
atDef = defRNative "at" at' (funType a [("idx",tTyInteger),("list",TyList (mkTyVar "l" []))] <>
                      funType a [("idx",tTyString),("object",tTyObject (mkSchemaVar "o"))])
  ["(at 1 [1 2 3])", "(at \"bar\" { \"foo\": 1, \"bar\": 2 })"]
  "Index LIST at IDX, or get value with key IDX from OBJECT."

langDefs :: NativeModule
langDefs =
    ("General",[
     ifDef
    ,mapDef
    ,foldDef

    ,defRNative "list" list
     (funType (TyList TyAny) [("elems",TyAny)])
     ["(list 1 2 3)"]
     "Create list from ELEMS. Deprecated in Pact 2.1.1 with literal list support."

    ,makeListDef
    ,reverseDef
    ,filterDef
    ,sortDef
    ,whereDef
    ,composeDef
    ,lengthDef
    ,takeDef
    ,dropDef

    ,defRNative "remove" remove (funType (tTyObject (mkSchemaVar "o")) [("key",tTyString),("object",tTyObject (mkSchemaVar "o"))])
     ["(remove \"bar\" { \"foo\": 1, \"bar\": 2 })"]
     "Remove entry for KEY from OBJECT."

    ,atDef
    ,enforceDef
    ,enforceOneDef
    ,formatDef

    ,defRNative "pact-id" pactId (funType tTyString []) []
     "Return ID if called during current pact execution, failing if not."

    ,readDecimalDef
    ,defRNative "read-integer" readInteger (funType tTyInteger [("key",tTyString)])
     [LitExample "(read-integer \"age\")"]
     "Parse KEY string or number value from top level of message data body as integer."
    ,defRNative "read-msg" readMsg (funType a [] <> funType a [("key",tTyString)])
     [LitExample "(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))"]
     "Read KEY from top level of message data body, or data body itself if not provided. \
     \Coerces value to their corresponding pact type: String -> string, Number -> integer, Boolean -> bool, \
     \List -> list, Object -> object."


    ,defRNative "tx-hash" txHash (funType tTyString []) ["(tx-hash)"]
     "Obtain hash of current transaction as a string."

    ,defNative (specialForm Bind) bind
     (funType a [("src",tTyObject row),("binding",TySchema TyBinding row def)])
     ["(bind { \"a\": 1, \"b\": 2 } { \"a\" := a-value } a-value)"]
     "Special form evaluates SRC to an object which is bound to with BINDINGS over subsequent body statements."
    ,defRNative "typeof" typeof'' (funType tTyString [("x",a)])
     ["(typeof \"hello\")"] "Returns type of X as string."
    ,setTopLevelOnly $ defRNative "list-modules" listModules
     (funType (TyList tTyString) []) [] "List modules available for loading."

    ,defRNative (specialForm YieldSF) yield
     (funType yieldv [("object",yieldv)] <>
      funType yieldv [("object", yieldv), ("target-chain",tTyString)])
     [ LitExample "(yield { \"amount\": 100.0 })"
     , LitExample "(yield { \"amount\": 100.0 } \"some-chain-id\")"
     ]
     "Yield OBJECT for use with 'resume' in following pact step. With optional argument TARGET-CHAIN, \
     \target subsequent step to execute on targeted chain using automated SPV endorsement-based dispatch."

    ,defNative (specialForm Resume) resume
     (funType a [("binding",TySchema TyBinding (mkSchemaVar "r") def)]) []
     "Special form binds to a yielded object value from the prior step execution in a pact."

    ,pactVersionDef

    ,setTopLevelOnly $ defRNative "enforce-pact-version" enforceVersion
     (funType tTyBool [("min-version",tTyString)] <>
      funType tTyBool [("min-version",tTyString),("max-version",tTyString)])
    ["(enforce-pact-version \"2.3\")"]
    "Enforce runtime pact version as greater than or equal MIN-VERSION, and less than or equal MAX-VERSION. \
    \Version values are matched numerically from the left, such that '2', '2.2', and '2.2.3' would all allow '2.2.3'."

    ,defRNative "contains" contains
    (funType tTyBool [("value",a),("list",TyList a)] <>
     funType tTyBool [("key",a),("object",tTyObject (mkSchemaVar "o"))] <>
     funType tTyBool [("value",tTyString),("string",tTyString)])
    [ "(contains 2 [1 2 3])"
    , "(contains 'name { 'name: \"Ted\", 'age: 72 })"
    , "(contains \"foo\" \"foobar\")"
    ]
    "Test that LIST or STRING contains VALUE, or that OBJECT has KEY entry."

    ,defNative "constantly" constantly
     (funType a [("value",a),("ignore1",b)] <>
      funType a [("value",a),("ignore1",b),("ignore2",c)] <>
      funType a [("value",a),("ignore1",b),("ignore2",c),("ignore3",d)])
     ["(filter (constantly true) [1 2 3])"]
     "Lazily ignore arguments IGNORE* and return VALUE."
    ,defRNative "identity" identity (funType a [("value",a)])
     ["(map (identity) [1 2 3])"] "Return provided value."
    ,strToIntDef
    ,hashDef
    ,defineNamespaceDef
    ,namespaceDef
    ,chainDataDef
    ])
    where
          d = mkTyVar "d" []
          row = mkSchemaVar "row"
          yieldv = TySchema TyObject (mkSchemaVar "y") def

obj :: Type n
obj = tTyObject (mkSchemaVar "o")

listStringA :: Type n
listStringA = mkTyVar "a" [TyList (mkTyVar "l" []),TyPrim TyString]

takeDrop :: FunTypes n
takeDrop = funType listStringA [("count",tTyInteger),("list",listStringA)] <>
           funType obj [("keys",TyList tTyString),("object",obj)]

lam :: Type v -> Type v -> Type v
lam x y = TyFun $ funType' y [("x",x)]

lam2 :: Type v -> Type v -> Type v -> Type v
lam2 x y z = TyFun $ funType' z [("x",x),("y",y)]

a, b, c :: Type n
a = mkTyVar "a" []
b = mkTyVar "b" []
c = mkTyVar "c" []

map' :: NativeFun e
map' i as@[app@TApp {},l] = gasUnreduced i as $ reduce l >>= \l' -> case l' of
           TList ls _ _ -> (\b' -> TList b' TyAny def) <$> forM ls (apply (_tApp app) . pure)
           t -> evalError' i $ "map: expecting list: " <> pretty (abbrev t)
map' i as = argsError' i as

list :: RNativeFun e
list i as = return $ TList (V.fromList as) TyAny (_faInfo i) -- TODO, could set type here

makeList :: RNativeFun e
makeList i [TLitInteger len,value] = case typeof value of
  Right ty -> return $ toTList ty def $ replicate (fromIntegral len) value
  Left ty -> evalError' i $ "make-list: invalid value type: " <> pretty ty
makeList i as = argsError i as

reverse' :: RNativeFun e
reverse' _ [l@TList{}] = return $ over tList V.reverse l
reverse' i as = argsError i as

fold' :: NativeFun e
fold' i as@[app@TApp {},initv,l] = gasUnreduced i as $ reduce l >>= \l' -> case l' of
           TList ls _ _ -> reduce initv >>= \initv' ->
                         foldM (\r a' -> apply (_tApp app) [r,a']) initv' ls
           t -> evalError' i $ "fold: expecting list: " <> pretty (abbrev t)
fold' i as = argsError' i as


filter' :: NativeFun e
filter' i as@[app@TApp {},l] = gasUnreduced i as $ reduce l >>= \l' -> case l' of
           TList ls lt _ -> fmap (toTListV lt def) $ (`V.filterM` ls) $ \a' -> do
                           t <- apply (_tApp app) [a']
                           case t of
                             (TLiteral (LBool bo) _) -> return bo
                             _ -> return False -- hmm, too permissive here, select is stricter
           t -> evalError' i $ "filter: expecting list: " <> pretty (abbrev t)
filter' i as = argsError' i as

length' :: RNativeFun e
length' _ [TList ls _ _] = return $ toTerm (length ls)
length' _ [TLitString s] = return $ toTerm (T.length s)
length' _ [TObject (Object ps _ _ _) _] = return $ toTerm (length ps)
length' i as = argsError i as

take' :: RNativeFun e
take' _ [TLitInteger c',TList l t _] = return $ TList (tordV V.take c' l) t def
take' _ [TLitInteger c',TLitString l] = return $ toTerm $ pack $ tordL take c' (unpack l)
take' _ [TList {..},TObject (Object (ObjectMap o) oTy _ _) _] = asKeyList _tList >>= \l ->
  return $ toTObjectMap oTy def $ ObjectMap $ M.restrictKeys o l

take' i as = argsError i as

drop' :: RNativeFun e
drop' _ [TLitInteger c',TList l t _] = return $ TList (tordV V.drop c' l) t def
drop' _ [TLitInteger c',TLitString l] = return $ toTerm $ pack $ tordL drop c' (unpack l)
drop' _ [TList {..},TObject (Object (ObjectMap o) oTy _ _) _] = asKeyList _tList >>= \l ->
  return $ toTObjectMap oTy def $ ObjectMap $ M.withoutKeys o l
drop' i as = argsError i as

asKeyList :: V.Vector (Term Name) -> Eval e (S.Set FieldKey)
asKeyList l = fmap (S.fromList . V.toList) . V.forM l $ \t -> case t of
  TLitString k -> return $ FieldKey k
  _ -> evalError (_tInfo t) "String required"

-- | "take or drop" handling negative "take/drop from reverse"
tord :: (l -> l) -> (Int -> l -> l) -> Integer -> l -> l
tord rev f c' l | c' >= 0 = f (fromIntegral c') l
                | otherwise = rev $ f (fromIntegral (negate c')) (rev l)

tordV
  :: (Int -> V.Vector a -> V.Vector a)
     -> Integer -> V.Vector a -> V.Vector a
tordV = tord V.reverse
tordL :: (Int -> [a] -> [a]) -> Integer -> [a] -> [a]
tordL = tord reverse

at' :: RNativeFun e
at' _ [li@(TLitInteger idx),TList ls _ _] =
    case ls V.!? fromIntegral idx of
      Just t -> return t
      Nothing -> evalError (_tInfo li) $ "at: bad index " <>
        pretty idx <> ", length " <> pretty (length ls)
at' _ [idx,TObject (Object ls _ _ _) _] = lookupObj idx ls
at' i as = argsError i as

lookupObj :: Term n -> ObjectMap (Term n) -> Eval m (Term n)
lookupObj t@(TLitString idx) (ObjectMap ls) = case M.lookup (FieldKey idx) ls of
  Just v -> return v
  Nothing -> evalError (_tInfo t) $ "key not found in object: " <> pretty idx
lookupObj t _ = evalError (_tInfo t) $ "object lookup only supported with strings"

remove :: RNativeFun e
remove _ [TLitString key,TObject (Object (ObjectMap ps) t _ _) _] =
  return $ toTObjectMap t def $ ObjectMap $ M.delete (FieldKey key) ps
remove i as = argsError i as

compose :: NativeFun e
compose i as@[appA@TApp {},appB@TApp {},v] = gasUnreduced i as $ do
  v' <- reduce v
  a' <- apply (_tApp appA) [v']
  apply (_tApp appB) [a']
compose i as = argsError' i as




readMsg :: RNativeFun e
readMsg i [TLitString key] = fromPactValue <$> parseMsgKey i "read-msg" key
readMsg i [] = fromPactValue <$> parseMsgKey' i "read-msg" Nothing
readMsg i as = argsError i as


readInteger :: RNativeFun e
readInteger i [TLitString key] = do
  (ParsedInteger a') <- parseMsgKey i "read-integer" key
  return $ toTerm a'
readInteger i as = argsError i as


pactId :: RNativeFun e
pactId i [] = toTerm <$> getPactId i
pactId i as = argsError i as

bind :: NativeFun e
bind i as@[src,TBinding ps bd (BindSchema _) bi] = gasUnreduced i as $
  reduce src >>= bindObjectLookup >>= bindReduce ps bd bi
bind i as = argsError' i as

bindObjectLookup :: Term Name -> Eval e (Text -> Maybe (Term Name))
bindObjectLookup (TObject (Object (ObjectMap o) _ _ _) _) =
  return $ \s -> M.lookup (FieldKey s) o
bindObjectLookup t = evalError (_tInfo t) $
  "bind: expected object: " <> pretty t

typeof'' :: RNativeFun e
typeof'' _ [t] = return $ tStr $ typeof' t
typeof'' i as = argsError i as

listModules :: RNativeFun e
listModules i _ = do
  mods <- keys (_faInfo i) Modules
  return $ toTermList tTyString $ map asString mods

yield :: RNativeFun e
yield i as = case as of
  [u@(TObject t _)] -> go Nothing t u
  [u@(TObject t _), (TLitString cid)] -> go (Just $ ChainId cid) t u
  _ -> argsError i as
  where
    go tid (Object o _ _ _) u = do
      eym <- use evalPactExec
      case eym of
        Nothing -> evalError' i "Yield not in defpact context"
        Just PactExec{..} -> do
          o' <- enforcePactValue' o
          y <- case tid of
            Nothing -> return $ Yield o' Nothing
            Just t -> fmap (Yield o') $ endorseM' i t
          evalPactExec . _Just . peYield .= Just y
          return u

resume :: NativeFun e
resume i as = case as of
  [TBinding ps bd (BindSchema _) bi] -> gasUnreduced i as $ do
    rm <- preview $ eePactStep . _Just . psResume . _Just
    case rm of
      Nothing -> evalError' i "Resume: no yielded value in context"
      Just (Yield o _) -> do
        let m = fmap fromPactValue o
        l <- bindObjectLookup (toTObjectMap TyAny def m)
        bindReduce ps bd bi l
  _ -> argsError' i as

where' :: NativeFun e
where' i as@[k',app@TApp{},r'] = gasUnreduced i as $ ((,) <$> reduce k' <*> reduce r') >>= \kr -> case kr of
  (k,r@TObject {}) -> lookupObj k (_oObject $ _tObject r) >>= \v -> apply (_tApp app) [v]
  _ -> argsError' i as
where' i as = argsError' i as


sort' :: RNativeFun e
sort' _ [l@(TList v _ _)] | V.null v = pure l
sort' _ [TList{..}] = liftIO $ do
  m <- V.thaw _tList
  (`V.sortBy` m) $ \x y -> case (x,y) of
    (TLiteral xl _,TLiteral yl _) -> xl `compare` yl
    _ -> EQ
  toTListV _tListType def <$> V.freeze m
sort' _ [TList fields _ fi,l@(TList vs lty _)]
  | V.null fields = evalError fi "Empty fields list"
  | V.null vs = return l
  | otherwise = do
      fields' <- asKeyList fields
      liftIO $ do
        m <- V.thaw vs
        (`V.sortBy` m) $ \x y -> case (x,y) of
          (TObject (Object (ObjectMap xo) _ _ _) _,TObject (Object (ObjectMap yo) _ _ _) _) ->
            let go field EQ = case (M.lookup field xo, M.lookup field yo) of
                  (Just (TLiteral lx _), Just (TLiteral ly _)) -> lx `compare` ly
                  _ -> EQ
                go _ ne = ne
            in foldr go EQ fields'
          _ -> EQ
        toTListV lty def <$> V.freeze m
sort' i as = argsError i as


enforceVersion :: RNativeFun e
enforceVersion i as = case as of
  [TLitString minVersion] -> doMin minVersion >> return (toTerm True)
  [TLitString minVersion,TLitString maxVersion] ->
    doMin minVersion >> doMax maxVersion >> return (toTerm True)
  _ -> argsError i as
  where
    doMin = doMatch "minimum" (>) (<)
    doMax = doMatch "maximum" (<) (>)
    doMatch msg failCmp succCmp fullV =
      foldM_ matchPart False $ zip (T.splitOn "." pactVersion) (T.splitOn "." fullV)
      where
        parseNum orgV s = case AP.parseOnly (AP.many1 AP.digit) s of
          Left _ -> evalError' i $ "Invalid version component: " <> pretty (orgV,s)
          Right v -> return v
        matchPart True _ = return True
        matchPart _ (pv,mv)  = do
          pv' <- parseNum pactVersion pv
          mv' <- parseNum fullV mv
          when (mv' `failCmp` pv') $ evalError' i $
            "Invalid pact version " <> pretty pactVersion <>
            ", " <> msg <> " allowed: " <> pretty fullV
          return (mv' `succCmp` pv')

contains :: RNativeFun e
contains _i [val,TList {..}] = return $ toTerm $ searchTermList val _tList
contains _i [TLitString k,TObject (Object (ObjectMap o) _ _ _) _] =
  return $ toTerm $ M.member (FieldKey k) o
contains _i [TLitString s,TLitString t] = return $ toTerm $ T.isInfixOf s t
contains i as = argsError i as

searchTermList :: (Foldable t, Eq n) => Term n -> t (Term n) -> Bool
searchTermList val = foldl search False
  where search True _ = True
        search _ t = t `termEq` val


constantly :: NativeFun e
constantly i (v:_) = gasUnreduced i [v] $ reduce v
constantly i as = argsError' i as

identity :: RNativeFun e
identity _ [a'] = return a'
identity i as = argsError i as

strToInt :: RNativeFun e
strToInt i as =
  case as of
    [TLitString s] -> go 10 s
    [TLitInteger base, TLitString s] -> go base s
    _ -> argsError i as
  where
    go base' txt =
      if T.all isHexDigit txt
      then
        if T.length txt <= 128
        then case baseStrToInt base' txt of
          Left _ -> argsError i as
          Right n -> return (toTerm n)
        else evalError' i $ "Invalid input: unsupported string length: " <> pretty txt
      else evalError' i $ "Invalid input: supplied string is not hex: " <> pretty txt

txHash :: RNativeFun e
txHash _ [] = (tStr . asString) <$> view eeHash
txHash i as = argsError i as

-- | Change of base for Text-based representations of integrals. Only bases
-- 2 through 16 are supported, for non-empty text of length <= 128
--
-- e.g.
--   -- hexadecimal to decimal
--   baseStrToInt 10 "abcdef123456" = 188900967593046
--
baseStrToInt :: Integer -> Text -> Either Text Integer
baseStrToInt base t =
  if base <= 1 || base > 16
  then Left $ "baseStrToInt - unsupported base: " `T.append` asString base
  else
    if T.null t
    then Left $ "baseStrToInt - empty text: " `T.append` asString t
    else foldM go 0 $ T.unpack t
  where
    go :: Integer -> Char -> Either Text Integer
    go acc c' =
      let val = fromIntegral . digitToInt $ c'
      in if val < base
         then pure $ base * acc + val
         else Left $ "baseStrToInt - character '" <> T.singleton c' <>
                "' is out of range for base " <> tShow base <> ": " <> t
{-# INLINE baseStrToInt #-}

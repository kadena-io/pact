{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- TODO This is to hide a warning involving `enforce'`, which has a typeclass
-- constraint unused in the function itself, but is critical for preventing misuse
-- by a caller. There is probably a better way to enforce this restriction,
-- allowing us to remove this warning suppression.
-- See: https://github.com/kadena-io/pact/pull/206/files#r215468087
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

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
  ) where

import Control.Arrow hiding (app)
import Control.Lens hiding (parts,Fold,contains)
import Control.Monad
import Control.Monad.Reader (asks)
import Control.Monad.Catch

import Data.Char (isHexDigit)
import Data.Default
import qualified Data.Attoparsec.Text as AP
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T (isInfixOf, length, all, splitOn)
import Data.Foldable
import Data.Aeson hiding ((.=))
import Data.Decimal
import Data.List
import Data.Function (on)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding

import Safe

import Pact.Eval
import Pact.Native.Db
import Pact.Native.Internal
import Pact.Native.Time
import Pact.Native.Ops
import Pact.Native.Keysets
import Pact.Types.Runtime
import Pact.Parse
import Pact.Types.Version
import Pact.Types.Hash (hash, hexStringToInteger, basedStringToInteger)


-- | All production native modules.
natives :: [NativeModule]
natives =
  [ langDefs
  , dbDefs
  , timeDefs
  , opDefs
  , keyDefs
  ]

-- | Production native modules as a dispatch map.
nativeDefs :: M.HashMap Name Ref
nativeDefs = mconcat $ map moduleToMap natives

moduleToMap :: NativeModule -> M.HashMap Name Ref
moduleToMap = M.fromList . map (((`Name` def) . asString) *** Direct) . snd

langDefs :: NativeModule
langDefs =
    ("General",[
     defNative "if" if' (funType a [("cond",tTyBool),("then",a),("else",a)])
     "Test COND, if true evaluate THEN, otherwise evaluate ELSE. \
     \`(if (= (+ 2 2) 4) \"Sanity prevails\" \"Chaos reigns\")`"

    ,defNative "map" map'
     (funType (TyList a) [("app",lam b a),("list",TyList b)])
     "Apply elements in LIST as last arg to APP, returning list of results. \
     \`(map (+ 1) [1 2 3])`"

    ,defNative "fold" fold'
     (funType a [("app",lam2 a b a),("init",a),("list",TyList b)])
     "Iteratively reduce LIST by applying APP to last result and element, starting with INIT. \
     \`(fold (+) 0 [100 10 5])`"

    ,defRNative "list" list
     (funType (TyList TyAny) [("elems",TyAny)])
     "Create list from ELEMS. Deprecated in Pact 2.1.1 with literal list support. `(list 1 2 3)`"

    ,defRNative "make-list" makeList (funType (TyList a) [("length",tTyInteger),("value",a)])
     "Create list by repeating VALUE LENGTH times. `(make-list 5 true)`"

    ,defRNative "reverse" reverse' (funType (TyList a) [("l",TyList a)])
     "Reverse a list. `(reverse [1 2 3])`"

    ,defNative "filter" filter'
     (funType (TyList a) [("app",lam a tTyBool),("list",TyList a)])
     "Filter LIST by applying APP to each element to get a boolean determining inclusion.\
     \`(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])`"

    ,defRNative "sort" sort'
     (funType (TyList a) [("values",TyList a)] <>
      funType (TyList (tTyObject (mkSchemaVar "o"))) [("fields",TyList tTyString),("values",TyList (tTyObject (mkSchemaVar "o")))])
     "Sort monotyped list of primitive VALUES, or objects using supplied FIELDS list. \
     \`(sort [3 1 2])` `(sort ['age] [{'name: \"Lin\",'age: 30} {'name: \"Val\",'age: 25}])`"

    ,defNative (specialForm Where) where'
     (funType tTyBool [("field",tTyString),("app",lam a tTyBool),("value",tTyObject (mkSchemaVar "row"))])
     "Utility for use in 'filter' and 'select' applying APP to FIELD in VALUE. \
     \`(filter (where 'age (> 20)) [{'name: \"Mary\",'age: 30} {'name: \"Juan\",'age: 15}])`"

     ,defNative "compose" compose (funType c [("x",lam a b),("y", lam b c),("value",a)])
     "Compose X and Y, such that X operates on VALUE, and Y on the results of X. \
     \`(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])`"

     ,defRNative "length" length' (funType tTyInteger [("x",listA)])
     "Compute length of X, which can be a list, a string, or an object.\
     \`(length [1 2 3])` `(length \"abcdefgh\")` `(length { \"a\": 1, \"b\": 2 })`"

    ,defRNative "take" take' takeDrop
     "Take COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, take from end. \
     \`(take 2 \"abcd\")` `(take (- 3) [1 2 3 4 5])` `(take ['name] { 'name: \"Vlad\", 'active: false})`"

    ,defRNative "drop" drop' takeDrop
     "Drop COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, drop from end.\
     \`(drop 2 \"vwxyz\")` `(drop (- 2) [1 2 3 4 5])` `(drop ['name] { 'name: \"Vlad\", 'active: false})`"

    ,defRNative "remove" remove (funType (tTyObject (mkSchemaVar "o")) [("key",tTyString),("object",tTyObject (mkSchemaVar "o"))])
     "Remove entry for KEY from OBJECT. `(remove \"bar\" { \"foo\": 1, \"bar\": 2 })`"

    ,defRNative "at" at' (funType a [("idx",tTyInteger),("list",TyList (mkTyVar "l" []))] <>
                          funType a [("idx",tTyString),("object",tTyObject (mkSchemaVar "o"))])
     "Index LIST at IDX, or get value with key IDX from OBJECT. \
     \`(at 1 [1 2 3])` `(at \"bar\" { \"foo\": 1, \"bar\": 2 })`"

    ,defNative "enforce" enforce (funType tTyBool [("test",tTyBool),("msg",tTyString)])
     "Fail transaction with MSG if pure function TEST fails, or returns true. \
     \`!(enforce (!= (+ 2 2) 4) \"Chaos reigns\")`"
    ,defNative "enforce-one" enforceOne (funType tTyBool [("msg",tTyString),("tests",TyList tTyBool)])
     "Run TESTS in order (in pure context, plus keyset enforces). If all fail, fail transaction. Short-circuits on first success. \
     \`(enforce-one \"Should succeed on second test\" [(enforce false \"Skip me\") (enforce (= (+ 2 2) 4) \"Chaos reigns\")])`"

    ,defRNative "format" format (funType tTyString [("template",tTyString),("vars",TyList TyAny)])
     "Interpolate VARS into TEMPLATE using {}. \
     \`(format \"My {} has {}\" [\"dog\" \"fleas\"])`"

    ,defRNative "pact-id" pactId (funType tTyInteger [])
     "Return ID if called during current pact execution, failing if not."

    ,defRNative "read-decimal" readDecimal (funType tTyDecimal [("key",tTyString)])
     "Parse KEY string or number value from top level of message data body as decimal.\
     \`$(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))`"

    ,defRNative "read-integer" readInteger (funType tTyInteger [("key",tTyString)])
     "Parse KEY string or number value from top level of message data body as integer. `$(read-integer \"age\")`"

    ,defRNative "read-msg" readMsg (funType a [] <> funType a [("key",tTyString)])
     "Read KEY from top level of message data body, or data body itself if not provided. \
     \Coerces value to pact type: String -> string, Number -> integer, Boolean -> bool, \
     \List -> value, Object -> value. NB value types are not introspectable in pact. \
     \`$(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))`"

    ,defRNative "tx-hash" transactionHash (funType tTyString [])
     "Obtain hash of current transaction as a string. `(tx-hash)`"

    ,defNative (specialForm Bind) bind
     (funType a [("src",tTyObject row),("binding",TySchema TyBinding row)])
     "Special form evaluates SRC to an object which is bound to with BINDINGS over subsequent body statements. \
     \`(bind { \"a\": 1, \"b\": 2 } { \"a\" := a-value } a-value)`"

    ,defRNative "typeof" typeof'' (funType tTyString [("x",a)])
     "Returns type of X as string. `(typeof \"hello\")`"

    ,defRNative "list-modules" listModules (funType (TyList tTyString) []) "List modules available for loading."

    ,defRNative "yield" yield (funType yieldv [("OBJECT",yieldv)])
     "Yield OBJECT for use with 'resume' in following pact step. The object is similar to database row objects, in that \
     \only the top level can be binded to in 'resume'; nested objects are converted to opaque JSON values. \
     \`$(yield { \"amount\": 100.0 })`"

    ,defNative "resume" resume
     (funType a [("binding",TySchema TyBinding (mkSchemaVar "y")),("body",TyAny)])
     "Special form binds to a yielded object value from the prior step execution in a pact."

    ,defRNative "pact-version" (\_ _ -> return $ toTerm pactVersion) (funType tTyString [])
     "Obtain current pact build version. `(pact-version)`"

    ,defRNative "enforce-pact-version" enforceVersion
     (funType tTyBool [("min-version",tTyString)] <>
      funType tTyBool [("min-version",tTyString),("max-version",tTyString)])
    "Enforce runtime pact version as greater than or equal MIN-VERSION, and less than or equal MAX-VERSION. \
    \Version values are matched numerically from the left, such that '2', '2.2', and '2.2.3' would all allow '2.2.3'. \
    \`(enforce-pact-version \"2.3\")`"

    ,defRNative "contains" contains
    (funType tTyBool [("value",a),("list",TyList a)] <>
     funType tTyBool [("key",a),("object",tTyObject (mkSchemaVar "o"))] <>
     funType tTyBool [("value",tTyString),("string",tTyString)])
    "Test that LIST or STRING contains VALUE, or that OBJECT has KEY entry. \
    \`(contains 2 [1 2 3])` `(contains 'name { 'name: \"Ted\", 'age: 72 })` `(contains \"foo\" \"foobar\")`"

    ,defNative "constantly" constantly
     (funType a [("value",a),("ignore1",b)] <>
      funType a [("value",a),("ignore1",b),("ignore2",c)] <>
      funType a [("value",a),("ignore1",b),("ignore2",c),("ignore3",d)])
     "Ignore (lazily) arguments IGNORE* and return VALUE. `(filter (constantly true) [1 2 3])`"
    ,defRNative "identity" identity (funType a [("value",a)])
     "Return provided value. `(map (identity) [1 2 3])`"

    ,defRNative "hash" hash' (funType tTyString [("value",a)])
     "Compute BLAKE2b 512-bit hash of VALUE. Strings are converted directly while other values are \
     \converted using their JSON representation. `(hash \"hello\")` `(hash { 'foo: 1 })`"

    ,defRNative "hex-str-to-int" hexStrToInt (funType tTyInteger [("value", tTyString)])
     "Compute the integer value of a string of length <= 128 chars consisting of hexadecimal \
     \characters. `(hex-str-to-int \"abcdef12345\")`"

    ,defRNative "based-str-to-int" basedStrToInt (funType tTyInteger [("base", tTyInteger), ("value", tTyString)])
     "Compute the integer value after change of base of a string of length <= 128 chars consisting of \
     \base-2 through base-16 (hexadecimal) characters. Only bases 2-16 are supported. `(based-str-to-int 2 \"abcdef\")`"
     
    ])
    where a = mkTyVar "a" []
          b = mkTyVar "b" []
          c = mkTyVar "c" []
          d = mkTyVar "d" []
          row = mkSchemaVar "row"
          yieldv = TySchema TyObject (mkSchemaVar "y")
          obj = tTyObject (mkSchemaVar "o")
          listA = mkTyVar "a" [TyList (mkTyVar "l" []),TyPrim TyString,TySchema TyObject (mkSchemaVar "o")]
          listStringA = mkTyVar "a" [TyList (mkTyVar "l" []),TyPrim TyString]
          takeDrop = funType listStringA [("count",tTyInteger),("list",listStringA)] <>
                     funType obj [("keys",TyList tTyString),("object",obj)]
          lam x y = TyFun $ funType' y [("x",x)]
          lam2 x y z = TyFun $ funType' z [("x",x),("y",y)]


if' :: NativeFun e
if' i as@[cond,then',else'] = gasUnreduced i as $ reduce cond >>= \cm -> case cm of
           TLiteral (LBool c) _ -> reduce (if c then then' else else')
           t -> evalError' i $ "if: conditional not boolean: " ++ show t
if' i as = argsError' i as

map' :: NativeFun e
map' i as@[app@TApp {},l] = gasUnreduced i as $ reduce l >>= \l' -> case l' of
           TList ls _ _ -> (\b -> TList b TyAny def) <$> forM ls (apply' app . pure)
           t -> evalError' i $ "map: expecting list: " ++ abbrev t
map' i as = argsError' i as

list :: RNativeFun e
list i as = return $ TList as TyAny (_faInfo i) -- TODO, could set type here

makeList :: RNativeFun e
makeList i [TLitInteger len,value] = case typeof value of
  Right ty -> return $ toTList ty def $ replicate (fromIntegral len) value
  Left ty -> evalError' i $ "make-list: invalid value type: " ++ show ty
makeList i as = argsError i as

reverse' :: RNativeFun e
reverse' _ [l@TList{}] = return $ over tList reverse l
reverse' i as = argsError i as

fold' :: NativeFun e
fold' i as@[app@TApp {},initv,l] = gasUnreduced i as $ reduce l >>= \l' -> case l' of
           TList ls _ _ -> reduce initv >>= \initv' ->
                         foldM (\r a -> apply' app [r,a]) initv' ls
           t -> evalError' i $ "fold: expecting list: " ++ abbrev t
fold' i as = argsError' i as


filter' :: NativeFun e
filter' i as@[app@TApp {},l] = gasUnreduced i as $ reduce l >>= \l' -> case l' of
           TList ls lt _ -> ((\b -> TList b lt def) . concat) <$>
                         forM ls (\a -> do
                           t <- apply' app [a]
                           case t of
                             (TLiteral (LBool True) _) -> return [a]
                             _ -> return []) -- hmm, too permissive here, select is stricter
           t -> evalError' i $ "filter: expecting list: " ++ abbrev t
filter' i as = argsError' i as

length' :: RNativeFun e
length' _ [TList ls _ _] = return $ toTerm (length ls)
length' _ [TLitString s] = return $ toTerm (T.length s)
length' _ [TObject ps _ _] = return $ toTerm (length ps)
length' i as = argsError i as

take' :: RNativeFun e
take' _ [TLitInteger c,TList l t _] = return $ TList (tord take c l) t def
take' _ [TLitInteger c,TLitString l] = return $ toTerm $ pack $ tord take c (unpack l)
take' _ [l@TList {},TObject {..}] =
  return $ toTObject _tObjectType def $ (`filter` _tObject) $ \(k,_) -> searchTermList k (_tList l)

take' i as = argsError i as

drop' :: RNativeFun e
drop' _ [TLitInteger c,TList l t _] = return $ TList (tord drop c l) t def
drop' _ [TLitInteger c,TLitString l] = return $ toTerm $ pack $ tord drop c (unpack l)
drop' _ [l@TList {},TObject {..}] =
  return $ toTObject _tObjectType def $ (`filter` _tObject) $ \(k,_) -> not $ searchTermList k (_tList l)
drop' i as = argsError i as

tord :: (Int -> [a] -> [a]) -> Integer -> [a] -> [a]
tord f c l | c >= 0 = f (fromIntegral c) l
           | otherwise = reverse $ f (fromIntegral (negate c)) (reverse l)

at' :: RNativeFun e
at' _ [li@(TLitInteger idx),TList ls _ _] =
    case ls `atMay` fromIntegral idx of
      Just t -> return t
      Nothing -> evalError (_tInfo li) $ "at: bad index " ++ show idx ++ ", length " ++ show (length ls)
at' _ [idx,TObject ls _ _] = lookupObj idx ls
at' i as = argsError i as

lookupObj :: (Eq n, Show n) => Term n -> [(Term n, Term n)] -> Eval m (Term n)
lookupObj idx ls = case lookup (unsetInfo idx) (map (first unsetInfo) ls) of
  Just v -> return v
  Nothing -> evalError (_tInfo idx) $ "at: key not found: " ++ show idx

remove :: RNativeFun e
remove _ [key,TObject ps t _] = return $ TObject (filter (\(k,_) -> unsetInfo key /= unsetInfo k) ps) t def
remove i as = argsError i as

compose :: NativeFun e
compose i as@[appA@TApp {},appB@TApp {},v] = gasUnreduced i as $ do
  v' <- reduce v
  a <- apply' appA [v']
  apply' appB [a]
compose i as = argsError' i as



format :: RNativeFun e
format i [TLitString s,TList es _ _] = do
  let parts = T.splitOn "{}" s
      plen = length parts
      rep (TLitString t) = t
      rep t = pack $ show t
  if plen == 1
  then return $ tStr s
  else if plen - length es > 1
       then evalError' i "format: not enough arguments for template"
       else return $ tStr $
            foldl' (\r (e,t) -> r <> rep e <> t)  (head parts) (zip es (tail parts))
format i as = argsError i as



readMsg :: RNativeFun e
readMsg i [TLitString key] = parseMsgKey i "read-msg" key
readMsg _ [] = TValue <$> view eeMsgBody <*> pure def
readMsg i as = argsError i as

-- | One-off type for 'readDecimal', not exported.
newtype ParsedDecimal = ParsedDecimal Decimal
instance FromJSON ParsedDecimal where
  parseJSON (String s) =
    ParsedDecimal <$> case AP.parseOnly (unPactParser number) s of
                        Right (LDecimal d) -> return d
                        Right (LInteger i) -> return (fromIntegral i)
                        _ -> fail $ "Failure parsing decimal string: " ++ show s
  parseJSON (Number n) = return $ ParsedDecimal (fromRational $ toRational n)
  parseJSON v = fail $ "Failure parsing integer: " ++ show v


readDecimal :: RNativeFun e
readDecimal i [TLitString key] = do
  (ParsedDecimal a) <- parseMsgKey i "read-decimal" key
  return $ toTerm a
readDecimal i as = argsError i as

-- | One-off type for 'readInteger', not exported.
newtype ParsedInteger = ParsedInteger Integer
instance FromJSON ParsedInteger where
  parseJSON (String s) =
    ParsedInteger <$> case AP.parseOnly (unPactParser number) s of
                        Right (LInteger i) -> return i
                        _ -> fail $ "Failure parsing integer string: " ++ show s
  parseJSON (Number n) = return $ ParsedInteger (round n)
  parseJSON v = fail $ "Failure parsing integer: " ++ show v

readInteger :: RNativeFun e
readInteger i [TLitString key] = do
  (ParsedInteger a) <- parseMsgKey i "read-integer" key
  return $ toTerm a
readInteger i as = argsError i as

enforce :: NativeFun e
enforce i as = runPure $ gasUnreduced i as $ mapM reduce as >>= enforce' i

enforceOne :: NativeFun e
enforceOne i as@[msg,TList conds _ _] = runPureSys (_faInfo i) $ gasUnreduced i as $ do
  msg' <- reduce msg >>= \m -> case m of
    TLitString s -> return s
    _ -> argsError' i as
  let tryCond r@Just {} _ = return r
      tryCond Nothing cond = catch (Just <$> reduce cond) (\(_ :: SomeException) -> return Nothing)
  r <- foldM tryCond Nothing conds
  case r of
    Nothing -> failTx (_faInfo i) (unpack msg')
    Just b -> return b
enforceOne i as = argsError' i as


enforce' :: PureNoDb e => RNativeFun e
enforce' i [TLiteral (LBool b) _,TLitString msg]
    | b = return $ TLiteral (LBool True) def
    | otherwise = failTx (_faInfo i) $ unpack msg
enforce' i as = argsError i as
{-# INLINE enforce #-}


pactId :: RNativeFun e
pactId i [] = use evalPactExec >>= \pe -> case pe of
  Nothing -> evalError' i "pact-id: not in pact execution"
  Just PactExec{..} -> return $ toTerm _pePactId
pactId i as = argsError i as

bind :: NativeFun e
bind i as@[src,TBinding ps bd (BindSchema _) bi] = gasUnreduced i as $
  reduce src >>= bindObjectLookup >>= bindReduce ps bd bi
bind i as = argsError' i as

bindObjectLookup :: Term Name -> Eval e (Text -> Maybe (Term Ref))
bindObjectLookup TObject {..} = do
  !m <- fmap M.fromList $ forM _tObject $ \(k,v) -> case k of
    TLitString k' -> return (k',liftTerm v)
    tk -> evalError _tInfo $ "Bad object (non-string key) in bind: " ++ show tk
  return (\s -> M.lookup s m)
bindObjectLookup t = evalError (_tInfo t) $ "bind: expected object: " ++ show t

typeof'' :: RNativeFun e
typeof'' _ [t] = return $ tStr $ typeof' t
typeof'' i as = argsError i as

listModules :: RNativeFun e
listModules _ _ = do
  mods <- view $ eeRefStore.rsModules
  return $ toTermList tTyString $ map asString $ M.keys mods

unsetInfo :: Term a -> Term a
unsetInfo a = set tInfo def a
{-# INLINE unsetInfo #-}

yield :: RNativeFun e
yield i [t@TObject {}] = do
  eym <- use evalPactExec
  case eym of
    Nothing -> evalError' i "Yield not in defpact context"
    Just {} -> do
      (evalPactExec . _Just . peYield) .= Just t
      return t
yield i as = argsError i as

resume :: NativeFun e
resume i as@[TBinding ps bd (BindSchema _) bi] = gasUnreduced i as $ do
  rm <- asks $ firstOf $ eePactStep . _Just . psResume . _Just
  case rm of
    Nothing -> evalError' i "Resume: no yielded value in context"
    Just rval -> bindObjectLookup rval >>= bindReduce ps bd bi
resume i as = argsError' i as

where' :: NativeFun e
where' i as@[k',app@TApp{},r'] = gasUnreduced i as $ ((,) <$> reduce k' <*> reduce r') >>= \kr -> case kr of
  (k,r@TObject {}) -> lookupObj k (_tObject r) >>= \v -> apply' app [v]
  _ -> argsError' i as
where' i as = argsError' i as


sort' :: RNativeFun e
sort' _ [TList{..}] = case nub (map typeof _tList) of
  [ty] -> case ty of
    Right rty@(TyPrim pty) -> case pty of
      TyValue -> badTy (show ty)
      TyKeySet -> badTy (show ty)
      _ -> do
        sl <- forM _tList $ \e -> case firstOf tLiteral e of
          Nothing -> evalError _tInfo $ "Unexpected type error, expected literal: " ++ show e
          Just lit -> return (lit,e)
        return $ TList (map snd $ sortBy (compare `on` fst) sl) rty def
    _ -> badTy (show ty)
  ts -> evalError _tInfo $ "sort: non-uniform list: " ++ show ts
  where badTy s = evalError _tInfo $ "sort: bad list type: " ++ s
sort' _ [fields@TList{},l@TList{}]
  | null (_tList fields) = evalError (_tInfo fields) "Empty fields list"
  | otherwise = do
      sortPairs <- forM (_tList l) $ \el -> case firstOf tObject el of
        Nothing -> evalError (_tInfo l) $ "Non-object found: " ++ show el
        Just o -> fmap ((,el) . reverse) $ (\f -> foldM f [] (_tList fields)) $ \lits fld -> do
          v <- lookupObj fld o
          case firstOf tLiteral v of
            Nothing -> evalError (_tInfo l) $ "Non-literal found at field " ++ show fld ++ ": " ++ show el
            Just lit -> return (lit:lits)
      return $ TList (map snd $ sortBy (compare `on` fst) sortPairs) (_tListType l) def

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
          Left _ -> evalError' i $ "Invalid version component: " ++ show (orgV,s)
          Right v -> return v
        matchPart True _ = return True
        matchPart _ (pv,mv)  = do
          pv' <- parseNum pactVersion pv
          mv' <- parseNum fullV mv
          when (mv' `failCmp` pv') $ evalError' i $
            "Invalid pact version " ++ show pactVersion ++ ", " ++ msg ++ " allowed: " ++ show fullV
          return (mv' `succCmp` pv')

contains :: RNativeFun e
contains _i [val,TList {..}] = return $ toTerm $ searchTermList val _tList
contains _i [k,TObject {..}] = return $ toTerm $ foldl search False _tObject
  where search True _ = True
        search _ (t,_) = t `termEq` k
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
identity _ [a] = return a
identity i as = argsError i as

hash' :: RNativeFun e
hash' i as =
  case as of
    [TLitString s] -> go $ encodeUtf8 s
    [a] -> go $ toStrict $ encode a
    _ -> argsError i as
  where go = return . tStr . asString . hash

hexStrToInt :: RNativeFun e
hexStrToInt i as =
  case as of
    [TLitString s] ->
      if T.all isHexDigit s
      then 
        if T.length s <= 128
        then case hexStringToInteger s of
          Left _ -> argsError i as
          Right n -> return . tStr . asString $ n
        else evalError' i $ "Invalid input: unsupported string length: " ++ (unpack s)
      else evalError' i $ "Invalid input: supplied string is not hex: " ++ (unpack s)
    _ -> argsError i as 

basedStrToInt :: RNativeFun e
basedStrToInt i as =
  case as of
    [TLitInteger base, TLitString s] ->
      if T.all isHexDigit s
      then
        if T.length s <= 128
        then case basedStringToInteger base s of
          Left _ -> argsError i as
          Right n -> return . tStr . asString $ n
        else evalError' i $ "Invalid input: unsupported string length: " ++ (unpack s)
      else evalError' i $ "Invalid input: supplied string is not hex: " ++ (unpack s)
    _ -> argsError i as

transactionHash :: RNativeFun e
transactionHash _ [] = (tStr . asString) <$> view eeHash
transactionHash i as = argsError i as


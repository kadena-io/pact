{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      :  Pact.Native
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Pact builtins/standard library.
--

module Pact.Native
    (natives
    ,nativeDefs
    ,moduleToMap
    ,initEvalEnv)
    where

import Control.Concurrent hiding (yield)
import Control.Lens hiding (from,to,parts,Fold)
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.Catch
import Data.Default
import qualified Data.Attoparsec.Text as AP
import Prelude hiding (exp)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Safe
import Control.Arrow hiding (app)
import Data.Foldable
import Data.Aeson hiding ((.=))
import Data.Maybe
import Data.Decimal


import Pact.Eval
import Pact.Native.Db
import Pact.Native.Internal
import Pact.Native.Time
import Pact.Native.Ops
import Pact.Native.Keysets
import Pact.Types.Runtime
import Pact.Parse
import Pact.Types.Version

-- | All production native modules.
natives :: [NativeModule]
natives = [
  langDefs,
  dbDefs,
  timeDefs,
  opDefs,
  keyDefs]


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

    ,defNative (specialForm Map) map'
     (funType (TyList a) [("app",lam b a),("list",TyList b)])
     "Apply elements in LIST as last arg to APP, returning list of results. \
     \`(map (+ 1) [1 2 3])`"

    ,defNative (specialForm Fold) fold'
     (funType a [("app",lam2 b a a),("init",a),("list",TyList b)])
     "Iteratively reduce LIST by applying APP to last result and element, starting with INIT. \
     \`(fold (+) 0 [100 10 5])`"

    ,defRNative "list" list
     (funType (TyList TyAny) [("elems",TyAny)])
     "Create list from ELEMS. Deprecated in Pact 2.1.1 with literal list support. `(list 1 2 3)`"

    ,defNative (specialForm Filter) filter'
     (funType (TyList a) [("app",lam a tTyBool),("list",TyList a)])
     "Filter LIST by applying APP to each element to get a boolean determining inclusion.\
     \`(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])`"

    ,defNative (specialForm Where) where'
     (funType tTyBool [("field",tTyString),("app",lam a tTyBool),("value",mkSchemaVar "row")])
     "Utility for use in 'filter' and 'select' applying APP to FIELD in VALUE. \
     \`(filter (where 'age (> 20)) [{'name: \"Mary\",'age: 30} {'name: \"Juan\",'age: 15}])`"

     ,defNative (specialForm Compose) compose (funType c [("x",lam a b),("y", lam b c),("value",a)])
     "Compose X and Y, such that X operates on VALUE, and Y on the results of X. \
     \`(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])`"

     ,defRNative "length" length' (funType tTyInteger [("x",listA)])
     "Compute length of X, which can be a list, a string, or an object.\
     \`(length [1 2 3])` `(length \"abcdefgh\")` `(length { \"a\": 1, \"b\": 2 })`"

    ,defRNative "take" take' takeDrop
     "Take COUNT values from LIST (or string). If negative, take from end. \
     \`(take 2 \"abcd\")` `(take (- 3) [1 2 3 4 5])`"

    ,defRNative "drop" drop' takeDrop
     "Drop COUNT values from LIST (or string). If negative, drop from end.\
     \`(drop 2 \"vwxyz\")` `(drop (- 2) [1 2 3 4 5])`"

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
     \`(enforce-one \"Should succeed on second test\" (enforce false \"Skip me\") (enforce (= (+ 2 2) 4) \"Chaos reigns\"))`"

    ,defRNative "format" format (funType tTyString [("template",tTyString),("vars",TyAny)])
     "Interpolate VARS into TEMPLATE using {}. \
     \`(format \"My {} has {}\" \"dog\" \"fleas\")`"

    ,defRNative "pact-txid" pactTxId (funType tTyInteger [])
     "Return reference tx id for pact execution."

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

    ,defNative (specialForm Bind) bind
     (funType a [("src",tTyObject row),("binding",TySchema TyBinding row),("body",TyAny)])
     "Special form evaluates SRC to an object which is bound to with BINDINGS to run BODY. \
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
    ])
    where a = mkTyVar "a" []
          b = mkTyVar "b" []
          c = mkTyVar "c" []
          row = mkSchemaVar "row"
          yieldv = TySchema TyObject (mkSchemaVar "y")
          listA = mkTyVar "a" [TyList (mkTyVar "l" []),TyPrim TyString,TySchema TyObject (mkSchemaVar "o")]
          listStringA = mkTyVar "a" [TyList (mkTyVar "l" []),TyPrim TyString]
          takeDrop = funType listStringA [("count",tTyInteger),("list",listStringA)]
          lam x y = TyFun $ funType' y [("x",x)]
          lam2 x y z = TyFun $ funType' z [("x",x),("y",y)]


if' :: NativeFun e
if' i [cond,then',else'] = reduce cond >>= \cm -> case cm of
           TLiteral (LBool c) _ -> reduce (if c then then' else else')
           t -> evalError' i $ "if: conditional not boolean: " ++ show t
if' i as = argsError' i as

map' :: NativeFun e
map' i [TApp af as ai,l] = reduce l >>= \l' -> case l' of
           TList ls _ _ -> (\b -> TList b TyAny def) <$> forM ls (apply af as ai . pure)
           t -> evalError' i $ "map: expecting list: " ++ abbrev t
map' i as = argsError' i as

list :: RNativeFun e
list i as = return $ TList as TyAny (_faInfo i) -- TODO, could set type here



fold' :: NativeFun e
fold' i [TApp af as ai,initv,l] = reduce l >>= \l' -> case l' of
           TList ls _ _ -> reduce initv >>= \initv' ->
                         foldM (\r a -> apply af as ai [r,a]) initv' ls
           t -> evalError' i $ "map: expecting list: " ++ abbrev t
fold' i as = argsError' i as


filter' :: NativeFun e
filter' i [TApp af as ai,l] = reduce l >>= \l' -> case l' of
           TList ls lt _ -> ((\b -> TList b lt def) . concat) <$>
                         forM ls (\a -> do
                           t <- apply af as ai [a]
                           case t of
                             (TLiteral (LBool True) _) -> return [a]
                             _ -> return [])
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
take' i as = argsError i as

drop' :: RNativeFun e
drop' _ [TLitInteger c,TList l t _] = return $ TList (tord drop c l) t def
drop' _ [TLitInteger c,TLitString l] = return $ toTerm $ pack $ tord drop c (unpack l)
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
compose _ [TApp af as ai,TApp bf bs bi,v] = do
  v' <- reduce v
  a <- apply af as ai [v']
  apply bf bs bi [a]
compose i as = argsError' i as



format :: RNativeFun e
format i (TLitString s:es) = do
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
    ParsedDecimal <$> case AP.parseOnly number s of
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
    ParsedInteger <$> case AP.parseOnly number s of
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
enforce i as = runPure (mapM reduce as >>= enforce' i)

enforceOne :: NativeFun e
enforceOne i as@[msg,TList conds _ _] = runPureSys True $ do
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


enforce' :: RNativeFun (Purity e)
enforce' i [TLiteral (LBool b) _,TLitString msg]
    | b = return $ TLiteral (LBool True) def
    | otherwise = failTx (_faInfo i) $ unpack msg
enforce' i as = argsError i as
{-# INLINE enforce #-}


pactTxId :: RNativeFun e
pactTxId _ [] = do
  pm <- view eePactStep
  case pm of
    Just p -> return (toTerm (_psTxId p))
    Nothing -> toTerm . fromMaybe (-1) <$> view eeTxId
pactTxId i as = argsError i as

bind :: NativeFun e
bind _ [src,TBinding ps bd (BindSchema _) bi] =
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


initEvalEnv :: e -> PactDb e -> IO (EvalEnv e)
initEvalEnv e b = do
  mv <- newMVar e
  return $ EvalEnv (RefStore nativeDefs M.empty) def Null def def def mv b


unsetInfo :: Term a -> Term a
unsetInfo a = set tInfo def a
{-# INLINE unsetInfo #-}

yield :: RNativeFun e
yield i [t@TObject {}] = do
  eym <- use evalYield
  case eym of
    Nothing -> evalError' i "Yield not in defpact context"
    Just ey -> do
      evalYield .= Just (set pyYield (Just t) ey)
      return t
yield i as = argsError i as

resume :: NativeFun e
resume i [TBinding ps bd (BindSchema _) bi] = do
  rm <- firstOf (eePactStep . _Just . psResume . _Just) <$> ask
  case rm of
    Nothing -> evalError' i "Resume: no yielded value in context"
    Just rval -> bindObjectLookup rval >>= bindReduce ps bd bi
resume i as = argsError' i as

where' :: NativeFun e
where' i as@[k',app@TApp{},r'] = ((,) <$> reduce k' <*> reduce r') >>= \kr -> case kr of
  (k,r@TObject {}) -> lookupObj k (_tObject r) >>= \v -> apply (_tAppFun app) (_tAppArgs app) (_tInfo app) [v]
  _ -> argsError' i as
where' i as = argsError' i as

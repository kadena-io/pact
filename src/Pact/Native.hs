{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
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
    (nativeDefs,nativesForDocs)
    where

import Control.Lens hiding (from,to,parts)
import Control.Monad
import Data.Default
import qualified Data.Attoparsec.Text as AP
import Data.String
import Prelude hiding (exp)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Text.Parser.Token (natural)
import Safe
import Control.Arrow
import Data.Foldable
import Control.Applicative
import Data.Semigroup ((<>))


import Pact.Eval
import Pact.Native.Db
import Pact.Native.Internal
import Pact.Native.Time
import Pact.Native.Ops
import Pact.Native.Keysets
import Pact.Types
import Pact.Compile


type IsXFun e = FunApp -> Term Name -> Eval e (Term Name)

natives :: Eval e NativeDef
natives = do
  nds <- langDefs
  dds <- dbDefs
  tds <- timeDefs
  ops <- opDefs
  kds <- keyDefs
  return $ nds ++ dds ++ tds ++ ops ++ kds

nativesForDocs :: Eval e [(String,NativeDef)]
nativesForDocs = do
  nds <- ("General",) <$> langDefs
  dds <- ("Database",) <$> dbDefs
  tds <- ("Time",) <$> timeDefs
  ops <- ("Operators",) <$> opDefs
  kds <- ("KeySets",) <$> keyDefs
  return [nds,dds,tds,ops,kds]

langDefs :: Eval e NativeDef
langDefs = foldDefs
    [
     defNative "if" if' (funType a [("cond",TyBool),("then",a),("else",a)])
     "Test COND, if true evaluate THEN, otherwise evaluate ELSE. \
     \`(if (= (+ 2 2) 4) \"Sanity prevails\" \"Chaos reigns\")`"

    ,defNative "map" map'
     (funType (TyList (Just a)) [("app",lam b a),("list",TyList (Just b))])
     "Apply elements in LIST as last arg to APP, returning list of results. \
     \`(map (+ 1) [1 2 3])`"

    ,defNative "fold" fold'
     (funType a [("app",lam2 b b a),("init",a),("list",TyList (Just b))])
     "Iteratively reduce LIST by applying APP to last result and element, starting with INIT. \
     \`(fold (+) 0 [100 10 5])`"

    ,defRNative "list" list
     (funType (TyList Nothing) [("elems",TyRest)])
     "Create list from ELEMS. `(list 1 2 3)`"

    ,defNative "filter" filter'
     (funType (TyList (Just a)) [("app",lam a TyBool),("list",TyList (Just a))])
     "Filter LIST by applying APP to each element to get a boolean determining inclusion.\
     \`(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])`"

     ,defNative "compose" compose (funType c [("x",lam a b),("y", lam b c),("value",a)])
     "Compose X and Y, such that X operates on VALUE, and Y on the results of X. \
     \`(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])`"

     ,defRNative "length" length' (funType TyInteger [("x",listA)])
     "Compute length of X, which can be a list, a string, or an object.\
     \`(length [1 2 3])` `(length \"abcdefgh\")` `(length { \"a\": 1, \"b\": 2 })`"

    ,defRNative "take" take' takeDrop
     "Take COUNT values from LIST (or string). If negative, take from end. \
     \`(take 2 \"abcd\")` `(take (- 3) [1 2 3 4 5])`"

    ,defRNative "drop" drop' takeDrop
     "Drop COUNT values from LIST (or string). If negative, drop from end.\
     \`(drop 2 \"vwxyz\")` `(drop (- 2) [1 2 3 4 5])`"

    ,defRNative "remove" remove (funType (TyObject Nothing) [("key",TyString),("object",TyObject Nothing)])
     "Remove entry for KEY from OBJECT. `(remove \"bar\" { \"foo\": 1, \"bar\": 2 })`"

    ,defRNative "at" at' (funType a [("idx",TyInteger),("list",TyList (Just a))] <>
                          funType a [("idx",TyString),("object",TyObject Nothing)])
     "Index LIST at IDX, or get value with key IDX from OBJECT. \
     \`(at 1 [1 2 3])` `(at \"bar\" { \"foo\": 1, \"bar\": 2 })`"

    ,defRNative "enforce" enforce (funType TyBool [("test",TyBool),("msg",TyString)])
     "Fail transaction with MSG if TEST fails, or returns true. \
     \`!(enforce (!= (+ 2 2) 4) \"Chaos reigns\")`"

    ,defRNative "format" format (funType TyString [("template",TyString),("vars",TyRest)])
     "Interpolate VARS into TEMPLATE using {}. \
     \`(format \"My {} has {}\" \"dog\" \"fleas\")`"

    ,defRNative "is-string" (isX isString) (isTy TyString)
     "Return VAL, enforcing string type. `!(is-string 123)` `(is-string \"abc\")`"
    ,defRNative "is-integer" (isX isInteger) (isTy TyInteger)
     "Return VAL, enforcing integer type `(is-integer 123)` `!(is-integer \"abc\")`"
    ,defRNative "is-bool" (isX isBool) (isTy TyBool)
     "Return VAL, enforcing boolean type. `(is-bool true)`"
    ,defRNative "is-decimal" (isX isDecimal) (isTy TyDecimal)
     "Return VAL, enforcing decimal type. `(is-decimal 123.45)`"
    ,defRNative "is-time" (isX isTime) (isTy TyTime)
     "Return VAL, enforcing time type. `(is-time (time \"2016-07-22T11:26:35Z\"))`"

    ,defRNative "pact-txid" pactTxId (funType TyInteger [])
     "Return reference tx id for pact execution."

    ,defRNative "read-decimal" readDecimal (funType TyDecimal [("key",TyString)])
     "Parse KEY string value from message data body as decimal.\
     \`$(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))`"
    ,defRNative "read-integer" readInteger (funType TyInteger [("key",TyString)])
     "Parse KEY string value from message data body as integer. `$(read-integer \"age\")`"
    ,defRNative "read-msg" readMsg (funType a [("key",TyString)])
     "Read KEY from message data body. Will recognize JSON types as corresponding Pact type.\
     \`$(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))`"

    ,defNative (specialForm Bind) bind (funType a [("src",TyObject Nothing),("bindings",TyBinding),("body",TyRest)])
     "Special form evaluates SRC to an object which is bound to with BINDINGS to run BODY. \
     \`(bind { \"a\": 1, \"b\": 2 } { \"a\" := a-value } a-value)`"
    ,defRNative "typeof" typeof' (funType TyString [("x",a)])
     "Returns type of X as string. `(typeof \"hello\")`"
    ,defRNative "list-modules" listModules (funType (TyList (Just TyString)) []) "List modules available for loading."
    ]
    where a = TyVar "a" []
          b = TyVar "b" []
          c = TyVar "c" []
          listA = TyVar "a" [TyList Nothing,TyString,TyObject Nothing]
          listStringA = TyVar "a" [TyList Nothing,TyString]
          takeDrop = funType listStringA [("count",TyInteger),("list",listStringA)]
          lam x y = TyFun $ funType' y [("x",x)]
          lam2 x y z = TyFun $ funType' z [("x",x),("y",y)]
          isTy t = funType t [("val",t)]


-- | Symbol map must be made within target monad
nativeDefs :: Eval e (M.HashMap String Ref)
nativeDefs = (M.map Direct . M.fromList) <$> natives

if' :: NativeFun e
if' i [cond,then',else'] = reduce cond >>= \cm -> case cm of
           TLiteral (LBool c) _ -> reduce (if c then then' else else')
           t -> evalError' i $ "if: conditional not boolean: " ++ show t
if' i as = argsError' i as

apply :: Term Ref -> [Term Ref] -> Info -> [Term Name] ->  Eval e (Term Name)
apply f as i as' = reduce (TApp f (as ++ map liftTerm as') i)

map' :: NativeFun e
map' i [TApp af as ai,l] = reduce l >>= \l' -> case l' of
           TList ls _ _ -> (\b -> TList b def def) <$> forM ls (apply af as ai . pure)
           t -> evalError' i $ "map: expecting list: " ++ abbrev t
map' i as = argsError' i as

list :: RNativeFun e
list i as = return $ TList as def (_faInfo i)

liftTerm :: Term Name -> Term Ref
liftTerm a = TVar (Direct a) def


fold' :: NativeFun e
fold' i [TApp af as ai,initv,l] = reduce l >>= \l' -> case l' of
           TList ls _ _ -> reduce initv >>= \initv' ->
                         foldM (\r a -> apply af as ai [r,a]) initv' ls
           t -> evalError' i $ "map: expecting list: " ++ abbrev t
fold' i as = argsError' i as


filter' :: NativeFun e
filter' i [TApp af as ai,l] = reduce l >>= \l' -> case l' of
           TList ls _ _ -> ((\b -> TList b def def) . concat) <$>
                         forM ls (\a -> do
                           t <- apply af as ai [a]
                           case t of
                             (TLiteral (LBool True) _) -> return [a]
                             _ -> return [])
           t -> evalError' i $ "filter: expecting list: " ++ abbrev t
filter' i as = argsError' i as

length' :: RNativeFun e
length' _ [TList ls _ _] = return $ toTerm (length ls)
length' _ [TLitString s] = return $ toTerm (length s)
length' _ [TObject ps _ _] = return $ toTerm (length ps)
length' i as = argsError i as

take' :: RNativeFun e
take' _ [TLitInteger c,TList l t _] = return $ TList (tord take c l) t def
take' _ [TLitInteger c,TLitString l] = return $ toTerm $ tord take c l
take' i as = argsError i as

drop' :: RNativeFun e
drop' _ [TLitInteger c,TList l t _] = return $ TList (tord drop c l) t def
drop' _ [TLitInteger c,TLitString l] = return $ toTerm $ tord drop c l
drop' i as = argsError i as

tord :: (Int -> [a] -> [a]) -> Integer -> [a] -> [a]
tord f c l | c >= 0 = f (fromIntegral c) l
           | otherwise = reverse $ f (fromIntegral (negate c)) (reverse l)

at' :: RNativeFun e
at' i [TLitInteger idx,TList ls _ _] =
    case ls `atMay` fromIntegral idx of
      Just t -> return t
      Nothing -> evalError' i $ "at: bad index " ++ show idx ++ ", length " ++ show (length ls)
at' i [idx,TObject ls _ _] =
    case lookup (unsetInfo idx) (map (first unsetInfo) ls) of
      Just v -> return v
      Nothing -> evalError' i $ "at: key not found: " ++ show idx
at' i as = argsError i as

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
  let parts = T.splitOn "{}" $ T.pack s
      plen = length parts
      rep (TLitString t) = t
      rep t = show t
  if plen == 1
  then return $ tStr s
  else if plen - length es > 1
       then evalError' i "format: not enough arguments for template"
       else return $ tStr $ T.unpack $
            foldl' (\r (e,t) -> r <> T.pack (rep e) <> t)  (head parts) (zip es (tail parts))
format i as = argsError i as



readMsg :: RNativeFun e
readMsg i [TLitString key] = parseMsgKey i "read-msg" key
readMsg i as = argsError i as


readDecimal :: RNativeFun e
readDecimal i [TLitString key] = do
  (t :: T.Text) <- parseMsgKey i "read-decimal" key
  case AP.parseOnly (negatable dec) t of
    Right (n,r) -> return $ toTerm $ maybe r (const $ negate r) n
    _ -> evalError' i $ "read-decimal: parse failure, expecting decimal: " ++ show t
readDecimal i as = argsError i as

readInteger :: RNativeFun e
readInteger i [TLitString key] = do
  (t :: T.Text) <- parseMsgKey i "read-integer" key
  case AP.parseOnly (negatable natural) t of
    Right (n,r) -> return $ toTerm (maybe r (const (negate r)) n)
    _ -> evalError' i $ "read-integer: parse failure, expecting integer: " ++ show t
readInteger i as = argsError i as

negatable :: AP.Parser a -> AP.Parser (Maybe Char,a)
negatable p = (,) <$> optional (AP.char '-') <*> p

enforce :: RNativeFun e
enforce _ [TLiteral (LBool b) _,TLitString msg]
    | b = return $ TLiteral (LBool True) def
    | otherwise = failTx msg
enforce i as = argsError i as
{-# INLINE enforce #-}

isX :: IsXFun e -> RNativeFun e
isX fun i [t] = fun i t
isX  _ i as = argsError i as

isString :: IsXFun e
isString _ t@TLitString {} = return t
isString i t = evalError' i $ "Not string: " ++ show t

isInteger :: IsXFun e
isInteger _ t@TLitInteger {} = return t
isInteger i t = evalError' i $ "Not integer: " ++ show t

isDecimal :: IsXFun e
isDecimal _ t@(TLiteral LDecimal {} _) = return t
isDecimal i t = evalError' i $ "Not decimal: " ++ show t

isBool :: IsXFun e
isBool _ t@(TLiteral LBool {} _) = return t
isBool i t = evalError' i $ "Not boolean: " ++ show t

isTime :: IsXFun e
isTime _ t@(TLiteral LTime {} _) = return t
isTime i t = evalError' i $ "Not time: " ++ show t

pactTxId :: RNativeFun e
pactTxId _ [] = do
  pm <- view eePactStep
  case pm of
    Just p -> return (toTerm (_psTxId p))
    Nothing -> toTerm <$> view eeTxId
pactTxId i as = argsError i as

bind :: NativeFun e
bind i [src,TBinding ps bd BindKV bi] = reduce src >>= \st -> case st of
  TObject o _ _ -> do
    !m <- fmap M.fromList $ forM o $ \(k,v) -> case k of
             TLitString k' -> return (k',liftTerm v)
             tk -> evalError' i $ "Bad object (non-string key) in bind: " ++ show tk
    bindReduce ps bd bi $ \s -> M.lookup s m
  t -> evalError' i $ "bind: source expression must evaluate to object: " ++ show t
bind i as = argsError' i as

typeof' :: RNativeFun e
typeof' _ [t] = return $ tStr $ either id show $ typeof t
typeof' i as = argsError i as

listModules :: RNativeFun e
listModules _ _ = do
  mods <- view $ eeRefStore.rsModules
  return $ toTermList $ map asString $ M.keys mods

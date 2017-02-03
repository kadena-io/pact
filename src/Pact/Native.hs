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
    (natives,nativeDefs,moduleToMap)
    where

import Control.Lens hiding (from,to,parts,Fold)
import Control.Monad
import Data.Default
import qualified Data.Attoparsec.Text as AP
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
moduleToMap = M.fromList . map ((Name . asString) *** Direct) . snd



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
     (funType a [("app",lam2 b b a),("init",a),("list",TyList b)])
     "Iteratively reduce LIST by applying APP to last result and element, starting with INIT. \
     \`(fold (+) 0 [100 10 5])`"

    ,defRNative "list" list
     (funType (TyList TyAny) [("elems",TyAny)])
     "Create list from ELEMS. `(list 1 2 3)`"

    ,defNative (specialForm Filter) filter'
     (funType (TyList a) [("app",lam a tTyBool),("list",TyList a)])
     "Filter LIST by applying APP to each element to get a boolean determining inclusion.\
     \`(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])`"

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

    ,defRNative "enforce" enforce (funType tTyBool [("test",tTyBool),("msg",tTyString)])
     "Fail transaction with MSG if TEST fails, or returns true. \
     \`!(enforce (!= (+ 2 2) 4) \"Chaos reigns\")`"

    ,defRNative "format" format (funType tTyString [("template",tTyString),("vars",TyAny)])
     "Interpolate VARS into TEMPLATE using {}. \
     \`(format \"My {} has {}\" \"dog\" \"fleas\")`"

    ,defRNative "pact-txid" pactTxId (funType tTyInteger [])
     "Return reference tx id for pact execution."

    ,defRNative "read-decimal" readDecimal (funType tTyDecimal [("key",tTyString)])
     "Parse KEY string value from message data body as decimal.\
     \`$(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))`"
    ,defRNative "read-integer" readInteger (funType tTyInteger [("key",tTyString)])
     "Parse KEY string value from message data body as integer. `$(read-integer \"age\")`"
    ,defRNative "read-msg" readMsg (funType a [] <> funType a [("key",tTyString)])
     "Read KEY from message data body, or data body itself if not provided. \
     \Will recognize JSON types as corresponding Pact type.\
     \`$(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))`"

    ,defNative (specialForm Bind) bind
     (funType a [("src",tTyObject row),("binding",TySchema TyBinding row),("body",TyAny)])
     "Special form evaluates SRC to an object which is bound to with BINDINGS to run BODY. \
     \`(bind { \"a\": 1, \"b\": 2 } { \"a\" := a-value } a-value)`"
    ,defRNative "typeof" typeof' (funType tTyString [("x",a)])
     "Returns type of X as string. `(typeof \"hello\")`"
    ,defRNative "list-modules" listModules (funType (TyList tTyString) []) "List modules available for loading."
    ])
    where a = mkTyVar "a" []
          b = mkTyVar "b" []
          c = mkTyVar "c" []
          row = mkSchemaVar "row"
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

apply :: Term Ref -> [Term Ref] -> Info -> [Term Name] ->  Eval e (Term Name)
apply f as i as' = reduce (TApp f (as ++ map liftTerm as') i)

map' :: NativeFun e
map' i [TApp af as ai,l] = reduce l >>= \l' -> case l' of
           TList ls _ _ -> (\b -> TList b TyAny def) <$> forM ls (apply af as ai . pure)
           t -> evalError' i $ "map: expecting list: " ++ abbrev t
map' i as = argsError' i as

list :: RNativeFun e
list i as = return $ TList as TyAny (_faInfo i) -- TODO, could set type here

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
readMsg _ [] = TValue <$> view eeMsgBody <*> pure def
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


pactTxId :: RNativeFun e
pactTxId _ [] = do
  pm <- view eePactStep
  case pm of
    Just p -> return (toTerm (_psTxId p))
    Nothing -> toTerm <$> view eeTxId
pactTxId i as = argsError i as

bind :: NativeFun e
bind i [src,TBinding ps bd (BindSchema _) bi] = reduce src >>= \st -> case st of
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
  return $ toTermList tTyString $ map asString $ M.keys mods

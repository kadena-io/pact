{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Pact.Native.Internal
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Internal functions for built-ins.
--

module Pact.Native.Internal

    where

import Control.Monad
import Prelude hiding (exp)
import Pact.Types
import Data.Default
import Pact.Eval
import Unsafe.Coerce
import Control.Lens
import Data.Aeson
import Control.Arrow
import qualified Data.Aeson.Lens as A
import qualified Data.Text as T
import Bound


-- | Native function with un-reduced arguments. Must fire call stack.
type NativeFun e = FunApp -> [Term Ref] -> Eval e (Term Name)

-- | Native function with pre-reduced arguments, call stack fired.
type RNativeFun e = FunApp -> [Term Name] -> Eval e (Term Name)


type NativeDef = [(String,Term Name)]


void' :: (ToTerm t,Functor m) => t -> m a -> m (Term Name)
void' = fmap . const . toTerm

success :: Functor m => String -> m a -> m (Term Name)
success = void'

callNative :: Term Ref -> FunApp -> [Term Name] -> Eval e a -> Eval e a
callNative t (FunApp i d) as = call (StackFrame (maybeModuleName t) i (defName d) (zip (_dArgs d) (map abbrev as)))


unsetInfo :: Term a -> Term a
unsetInfo a = set tInfo def a
{-# INLINE unsetInfo #-}


parseMsgKey :: (FromJSON t) => FunApp -> String -> String -> Eval e t
parseMsgKey i msg key = do
  vm <- firstOf (A.key (T.pack key)) <$> view eeMsgBody
  case vm of
    Nothing -> evalError' i $ "No such key in message: " ++ show key
    Just v -> case fromJSON v of
                Success t -> return t
                Error e -> evalError' i $ msg ++ ": parse failed: " ++ e ++ ": " ++ show v


bindReduce :: [(String,Term Ref)] -> Scope Int Term Ref -> Info -> (String -> Maybe (Term Ref)) -> Eval e (Term Name)
bindReduce ps bd bi lkpFun = do
  !(vs :: [(String,Term Ref)]) <- forM ps $ \(k,var) -> do
          var' <- reduce var
          case var' of
            (TLitString s) -> case lkpFun s of
                                Nothing -> evalError bi $ "Bad column in binding: " ++ s
                                Just v -> return $! (k,v)
            t -> evalError bi $ "Invalid column identifier in binding: " ++ show t
  let bd'' = instantiate (resolveArg bi (map snd vs)) bd
  call (StackFrame Nothing bi "<binding>" (map (second abbrev) vs)) $! reduce bd''



-- TODO: call stack??

defNative :: NativeDefName -> NativeFun e -> [String] -> String -> Eval e (String,Term Name)
defNative pactName fun args docs =
    return (asString pactName,
     TNative (DefData (asString pactName) Defun Nothing args (Just docs)) (NativeDFun pactName (unsafeCoerce fun)) def def)

defRNative :: NativeDefName -> RNativeFun e -> [String] -> String -> Eval e (String,Term Name)
defRNative name fun = defNative name (reduced fun)
    where reduced f fi as = mapM reduce as >>= \as' -> f fi as'

foldDefs :: Monad m => [m a] -> m [a]
foldDefs = foldM (\r d -> d >>= \d' -> return (d':r)) []

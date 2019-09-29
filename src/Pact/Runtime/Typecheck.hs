{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


-- |
-- Module      :  Pact.Runtime.Typecheck
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Runtime typechecking of 'Term' arguments.
--

module Pact.Runtime.Typecheck
  ( typecheck
  , checkUserType
  , typecheckDef
  , typecheckArgs
  ) where


import Control.Arrow hiding (app)
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Pact.Types.Pretty
import Pact.Types.Runtime


-- | Runtime input typecheck, enforced on let bindings, consts, user defun app args.
-- Output checking -- app return values -- left to static TC.
-- Native funs not checked here, as they use pattern-matching etc.
typecheck :: [(Arg (Term Name),Term Name)] -> Eval e ()
typecheck ps = foldM_ tvarCheck M.empty ps where
  tvarCheck m (Arg {..},t) = do
    r <- typecheckTerm _aInfo _aType t
    case r of
      Nothing -> return m
      Just (v,ty) -> case M.lookup v m of
        Nothing -> return $ M.insert v ty m
        Just prevTy | prevTy == ty -> return m
                    | otherwise ->
                        evalError (_tInfo t) $ "Type error: values for variable " <> pretty _aType <>
                        " do not match: " <> pretty (prevTy,ty)

-- | Typecheck a def against a declared fun type.
typecheckDef :: Def Ref -> FunType (Term Name) -> FunType (Term Name) -> Eval e ()
typecheckDef def defTy funTy = validateArgCount >> tcReturn >> tcArgs
  where
    defArgCount = length $ _ftArgs defTy
    ftyArgCount = length $ _ftArgs funTy
    validateArgCount
      | defArgCount == ftyArgCount = return ()
      | otherwise = evalError' def $ "Bad arg count in '" <> pretty (_dDefName def) <>
                    "', expected " <> pretty ftyArgCount <>
                    ", found " <> pretty defArgCount
    tcReturn = tc def (_ftReturn defTy) (_ftReturn funTy)
    tcArgs = zipWithM_ tcArg (_ftArgs defTy) (_ftArgs defTy)
    tcArg a specA = tc a (_aType a) (_aType specA)
    tc i ty specTy = unless (ty `canUnifyWith` specTy) $
      typecheckFailed i ty specTy


-- | Typecheck applied args against a FunType from some Def.
typecheckArgs
  :: HasInfo i
  => i
  -> DefName
  -> FunType (Term Name)
  -> [Term Name]
  -> Eval e ()
typecheckArgs i defName ft' as' = do
  let params = _ftArgs ft'
  when (length params /= length as') $
    evalError' i $ pretty defName <> ": Incorrect number of arguments (" <>
      pretty (length as') <> ") supplied; expected " <> pretty (length params)
  typecheck (zip params as')


typecheckFailed :: (HasInfo i, Pretty a, Pretty b) => i -> a -> b -> Eval e c
typecheckFailed i found spec = evalError' i $
  "Type error: expected " <> pretty spec <> ", found " <> pretty found

-- | 'typecheckTerm i spec t' checks a Term 't' against a specified type 'spec'.
-- Returns `Nothing` on successful check against concrete/untyped,
-- or `Just` a pair for successful check against a type variable, where
-- the pair is the type variable itself and the term type.
typecheckTerm :: forall e . Info -> Type (Term Name) -> Term Name
       -> Eval e (Maybe (TypeVar (Term Name),Type (Term Name)))
typecheckTerm i spec t = do

  ty <- case typeof t of
    Left s -> evalError i $ "Invalid type in value location: " <> pretty s
    Right r -> return r

  let

    tcFail found = typecheckFailed i found spec

    tcOK = return Nothing

    -- | check container parameterized type.
    -- 'paramCheck pspec pty check' check specified param ty 'pspec' with
    -- value param ty 'pty'. If not trivially equal, use 'check'
    -- to determine actual container value type, and compare for equality
    -- with specified.
    paramCheck :: Type (Term Name)
               -> Type (Term Name)
               -> (Type (Term Name) -> Eval e (Type (Term Name)))
               -> Eval e (Maybe (TypeVar (Term Name),Type (Term Name)))
    paramCheck TyAny _ _ = tcOK -- no spec
    paramCheck pspec pty check
      | pspec == pty = tcOK -- equality OK
      | otherwise = do
          -- run check function to get actual content type
          checked <- check pspec
          -- final check expects full match with toplevel 'spec'
          if checked == spec then tcOK else tcFail checked

    -- | infer list value type
    checkList es lty = return $ TyList $
                    case nub (map typeof $ V.toList es) of
                      [Right a] -> a -- uniform value type: return it
                      [] -> lty -- empty: return specified
                      _ -> TyAny -- otherwise untyped

  case (spec,ty,t) of
    (_,_,_) | spec == ty -> tcOK -- identical types always OK
    (TyAny,_,_) -> tcOK -- var args are untyped
    (TyVar {..},_,_) ->
      if spec `canUnifyWith` ty
      then return $ Just (_tyVar,ty) -- collect found types under vars
      else tcFail ty -- constraint failed
    -- check list
    (TyList lspec,TyList lty,TList {..}) ->
      paramCheck lspec lty (checkList _tList)
    -- check object
    (TySchema TyObject ospec specPartial,TySchema TyObject oty _,TObject {..}) ->
      paramCheck ospec oty (checkUserType specPartial i (_oObject _tObject))
    (TyPrim (TyGuard a),TyPrim (TyGuard b),_) -> case (a,b) of
      (Nothing,Just _) -> tcOK
      (Just _,Nothing) -> tcOK
      (c,d) -> if c == d then tcOK else tcFail ty
    _ -> tcFail ty

-- | check object args. Used in 'typecheckTerm' above and also in DB writes.
-- Total flag allows for partial row types if False.
checkUserType :: SchemaPartial -> Info -> ObjectMap (Term Name) -> Type (Term Name) -> Eval e (Type (Term Name))
checkUserType partial i (ObjectMap ps) (TyUser tu@TSchema {..}) = do
  -- fields is lookup from name to arg.
  -- TODO consider OMap or equivalent for schema fields
  let fields = M.fromList . map (FieldKey . _aName &&& id) $ _tFields
  aps <- forM (M.toList ps) $ \(k,v) -> case M.lookup k fields of
      Nothing -> evalError i $ "Invalid field for {" <> pretty _tSchemaName <> "}: " <> pretty k
      Just a -> return (a,v)
  let findMissing fs = do
        let missing = M.difference fs (M.fromList (map (first $ FieldKey . _aName) aps))
        unless (M.null missing) $ evalError i $
          "Missing fields for {" <> pretty _tSchemaName <> "}: " <> prettyList (M.elems missing)
  case partial of
    FullSchema -> findMissing fields
    PartialSchema fs -> findMissing (M.restrictKeys fields (S.map FieldKey fs))
    AnySubschema -> return ()
  typecheck aps
  return $ TySchema TyObject (TyUser tu) partial
checkUserType _ i _ t = evalError i $ "Invalid reference in user type: " <> pretty t

{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Types.Typecheck
-- Copyright   :  (C) 2017 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Types for Pact typechecker.
--

module Pact.Types.Typecheck where

import Control.Monad.Catch
import Control.Lens hiding (pre,List)
import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Aeson hiding (Object, (.=))
import Data.Foldable
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>),(<>))
import Data.Monoid

import Pact.Types.Lang
import Pact.Types.Util
import Pact.Native.Internal


data CheckerException = CheckerException Info String deriving (Eq,Ord)

instance Exception CheckerException
instance Show CheckerException where show (CheckerException i s) = renderInfo i ++ ": " ++ s

-- | Model a user type. Currently only Schemas are supported.
data UserType = Schema {
  _utName :: TypeName,
  _utModule :: ModuleName,
  _utFields :: [Arg UserType],
  _utInfo :: Info
  } deriving (Eq,Ord)
instance Show UserType where
  show Schema {..} = "{" ++ asString _utModule ++ "." ++ asString _utName ++ " " ++ show _utFields ++ "}"
instance Pretty UserType where
  pretty Schema {..} = braces (pretty _utModule <> dot <> pretty _utName)

-- | An ID for an AST node.
data TcId = TcId {
  _tiInfo :: Info,
  _tiName :: String,
  _tiId :: Int
  }

instance Eq TcId where
  a == b = _tiId a == _tiId b && _tiName a == _tiName b
instance Ord TcId where
  a <= b = _tiId a < _tiId b || (_tiId a == _tiId b && _tiName a <= _tiName b)
-- show instance is important, used as variable name
instance Show TcId where show TcId {..} = _tiName ++ show _tiId
instance Pretty TcId where pretty = string . show


-- | Role of an AST in an overload.
data VarRole = ArgVar Int | RetVar
  deriving (Eq,Show,Ord)

-- | Combine an AST id with a role.
data Overload = Overload { _oRole :: VarRole, _oOverApp :: TcId }
 deriving (Eq,Ord)

instance Show Overload where
  show (Overload r ts) = show ts ++ "?" ++ (case r of ArgVar i -> show i; RetVar -> "r")

-- | Data structure to track the latest substituted type and any associated overloads.
data Types = Types {
  _tsType :: Type UserType,
  _tsOverloads :: [Overload]
  } deriving (Eq,Ord)

instance Show Types  where
  show (Types p []) = show p
  show (Types p os) = show p ++ " " ++ show os
instance Pretty Types where
  pretty (Types p []) = pretty p
  pretty (Types p os) = pretty p <+> pshow os

data Failure = Failure TcId String deriving (Eq,Ord,Show)
type Failures = S.Set Failure

-- | Typechecker state.
data TcState = TcState {
  _doDebug :: Bool,
  _tcSupply :: Int,
  -- | Maps native app AST to an overloaded function type, and stores result of solver.
  _tcOverloads :: M.Map TcId (Either (FunTypes UserType) (FunType UserType)),
  _tcFailures :: Failures,
  -- | Maps ASTs to a type var.
  _tcAstToVar :: M.Map TcId (TypeVar UserType),
  -- | Maps type vars to types.
  _tcVarToTypes :: M.Map (TypeVar UserType) Types
  } deriving (Eq,Show)

mkTcState :: Int -> Bool -> TcState
mkTcState sup dbg = TcState dbg sup def def def def

instance Pretty TcState where
  pretty TcState {..} =
    "Overloads:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <> string "?" <+> colon <+>
                           align (vsep (map (string . show) (toList v)))) $ M.toList _tcOverloads) <$$>
    "AstToVar:" <$$>
    indent 2 (vsep (map (\(k,v) -> pretty k <> colon <+> pretty v) (M.toList _tcAstToVar))) <$$>
    "VarToTypes:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pshow k <> colon <+> pretty v) $ M.toList _tcVarToTypes) <$$>
    prettyFails _tcFailures
    <> hardline

prettyFails :: Failures -> Doc
prettyFails fs = "Failures:" <$$>
    indent 2 (vsep $ map (string.show) (toList fs))


-- | Typechecker monad.
newtype TC a = TC { unTC :: StateT TcState IO a }
  deriving (Functor,Applicative,Monad,MonadState TcState,MonadIO,MonadThrow,MonadCatch)



makeLenses ''TcState
makeLenses ''Types


-- | Storage for literal values.
data PrimValue =
  PrimLit Literal |
  PrimKeySet KeySet |
  PrimValue Value
  deriving (Eq,Show)
instance Pretty PrimValue where
  pretty (PrimLit l) = text (show l)
  pretty (PrimKeySet k) = text (show k)
  pretty (PrimValue v) = text (show v)


-- | A top-level module production.
data TopLevel t =
  TopFun {
    _tlFun :: Fun t
    } |
  TopConst {
    _tlInfo :: Info,
    _tlName :: String,
    _tlType :: Type UserType,
    _tlConstVal :: AST t
    } |
  TopTable {
    _tlInfo :: Info,
    _tlName :: String,
    _tlType :: Type UserType
  } |
  TopUserType {
    _tlInfo :: Info,
    _tlUserType :: UserType
  }
  deriving (Eq,Functor,Foldable,Traversable,Show)
instance Pretty t => Pretty (TopLevel t) where
  pretty (TopFun f) = "Fun" <$$> pretty f
  pretty (TopConst _i n t v) =
    "Const" <+> pretty n <> colon <> pretty t <$$>
    indent 2 (pretty v)
  pretty (TopTable _i n t) =
    "Table" <+> pretty n <> colon <> pretty t
  pretty (TopUserType _i t) = "UserType" <+> pretty t

-- | Special-form handling (with-read, map etc)
data Special t =
  SPartial |
  SBinding (AST t)
  deriving (Eq,Show,Functor,Foldable,Traversable)


-- | A native or user function.
data Fun t =
  FNative {
    _fInfo :: Info,
    _fName :: String,
    _fTypes :: FunTypes UserType,
    _fSpecial :: Maybe (SpecialForm,Special t)
    } |
  FDefun {
    _fInfo :: Info,
    _fName :: String,
    _fType :: FunType UserType,
    _fArgs :: [Named t],
    _fBody :: [AST t],
    _fDocs :: Maybe String
    }
  deriving (Eq,Functor,Foldable,Traversable,Show)

instance Pretty t => Pretty (Fun t) where
  pretty FNative {..} = "(native " <> text _fName <$$>
    indent 2 ("::" <+> align (vsep (map pretty (toList _fTypes)))) <>
      (case _fSpecial of
         Nothing -> mempty
         Just (_,SBinding bod) -> mempty <$$> indent 2 (pretty bod)
         _ -> mempty) <$$>
      ")"
  pretty FDefun {..} = "(defun " <> text _fName <$$>
    indent 2 ("::" <+> pretty _fType) <$$>
    indent 2 ("(" <$$>
              indent 2 (vsep (map pretty _fArgs)) <$$> ")") <$$>
    indent 2 (vsep (map pretty _fBody)) <$$>
    ")"


-- | Pair an AST with its type.
data Node = Node {
  _aId :: TcId,
  _aTy :: Type UserType
  } deriving (Eq,Ord)
instance Show Node where
  show (Node i t) = show i ++ "::" ++ show t
instance Pretty Node where
  pretty (Node i t) = pretty i <> "::" <> pretty t

-- | Pair an unescaped, unmangled "bare" name with something.
data Named i = Named {
  _nnName :: String,
  _nnNamed :: i
  } deriving (Eq,Ord,Functor,Foldable,Traversable)
instance (Show i) => Show (Named i) where
  show (Named na no) = show na ++ "(" ++ show no ++ ")"
instance (Pretty i) => Pretty (Named i) where pretty (Named na no) = dquotes (pretty na) <+> parens (pretty no)

-- | Inlined AST.
data AST n =
  App {
  _aNode :: n,
  _aAppFun :: Fun n,
  _aAppArgs :: [AST n]
  } |
  Binding {
  _aNode :: n,
  _aBindings :: [(Named n,AST n)],
  _aBody :: [AST n],
  _aBindType :: BindType n
  } |
  List {
  _aNode :: n,
  _aList :: [AST n]
  } |
  Object {
  _aNode :: n,
  _aObject :: [(AST n,AST n)]
  } |
  Prim {
  _aNode :: n,
  _aPrimValue :: PrimValue
  } |
  Var {
  _aNode :: n
  } |
  Table {
  _aNode :: n
  } |
  Step {
  _aNode :: n,
  _aEntity :: AST n,
  _aExec :: AST n,
  _aRollback :: Maybe (AST n)
  }

  deriving (Eq,Functor,Foldable,Traversable,Show)

instance Pretty t => Pretty (AST t) where
  pretty a = case a of
     Prim {..} -> pn <+> equals <+> pretty _aPrimValue
     Var {..} -> pn
     Object {..} -> pn <$$> "{" <$$>
       indent 2 (vsep (map (\(k,v) -> pretty k <> text ":" <$$> indent 4 (pretty v)) _aObject)) <$$>
       "}"
     List {..} -> pn <$$> "[" <$$> indent 2 (vsep (map pretty _aList)) <$$> "]"
     Binding {..} -> pn <$$> "(" <> pretty _aBindType <$$>
       indent 2 (vsep (map (\(k,v) ->
                              "(" <$$>
                              indent 2 (pretty k <+> colon <$$>
                                        indent 2 (pretty v)) <$$>
                              ")" ) _aBindings)) <$$>
       indent 2 (vsep (map pretty _aBody)) <$$> ")"
     App {..} -> pn <$$>
       indent 2 ("(" <$$> indent 2 (vsep (map pretty _aAppArgs)) <$$> ")") <$$>
       indent 2 (pretty _aAppFun)
     Table {..} -> pn
     Step {..} ->
       let rb = case _aRollback of
                  Nothing -> (<> empty)
                  Just r -> (<$$> "Rollback:" <$$> indent 2 (pretty r))
       in rb (pn <$$> indent 2 ("Entity" <> colon <+> pretty _aEntity) <$$>
              indent 2 (pretty _aExec))
   where pn = pretty (_aNode a)



makeLenses ''AST
makeLenses ''Fun
makeLenses ''TopLevel
makeLenses ''Node
makeLenses ''TcId


-- | Run monad providing supply seed and debug.
runTC :: Int -> Bool -> TC a -> IO (a, TcState)
runTC sup dbg a = runStateT (unTC a) (mkTcState sup dbg)

-- | Pre-visit or post-visit specification.
data Visit = Pre | Post deriving (Eq,Show)
-- | Type that can walk AST nodes with 'walkAST'
type Visitor m n = Visit -> AST n -> m (AST n)

-- | Storage for overload solving.
data SolveOverload o = SO {
  _soOverload :: o,
  _soRoles :: M.Map VarRole (TypeVar UserType),
  _soSolution :: Maybe (FunType UserType)
  } deriving (Eq,Ord,Show,Functor,Foldable,Traversable)
makeLenses ''SolveOverload

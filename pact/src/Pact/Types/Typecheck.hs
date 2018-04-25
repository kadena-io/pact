{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Types.Typecheck
-- Copyright   :  (C) 2017 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Types for Pact typechecker.
--

module Pact.Types.Typecheck
  (
    CheckerException (..),
    UserType (..),
    TcId (..),tiInfo,tiName,tiId,
    VarRole (..),
    OverloadSpecial (..),
    Overload (..),oRoles,oTypes,oSolved,oSpecial,oFunName,
    Failure (..),prettyFails,
    TcState (..),tcDebug,tcSupply,tcOverloads,tcFailures,tcAstToVar,tcVarToTypes,
    TC (..), runTC,
    PrimValue (..),
    TopLevel (..),tlFun,tlInfo,tlName,tlType,tlConstVal,tlUserType,
    Special (..),
    Fun (..),fInfo,fName,fTypes,fSpecial,fType,fArgs,fBody,fDocs,
    Node (..),aId,aTy,
    Named (..),
    AST (..),aNode,aAppFun,aAppArgs,aBindings,aBody,aBindType,aList,aObject,aPrimValue,aEntity,aExec,aRollback,aTableName,
    Visit(..),Visitor
  ) where

import Control.Monad.Catch
import Control.Lens hiding (List)
import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Aeson hiding (Object)
import Data.Foldable
import Text.PrettyPrint.ANSI.Leijen hiding ((<$$>),(<>))
import Data.Monoid

import Pact.Types.Lang
import Pact.Types.Util
import Pact.Types.Native


data CheckerException = CheckerException Info String deriving (Eq,Ord)

instance Exception CheckerException
instance Show CheckerException where show (CheckerException i s) = renderInfo i ++ ": " ++ s

-- | Model a user type. Currently only Schemas are supported..
data UserType = Schema {
  _utName :: TypeName,
  _utModule :: ModuleName,
  _utFields :: [Arg UserType],
  _utInfo :: Info
  } deriving (Eq,Ord)
instance Show UserType where
  show Schema {..} = "{" ++ unpack (asString _utModule) ++ "." ++ unpack (asString _utName) ++ " " ++ show _utFields ++ "}"
instance Pretty UserType where
  pretty Schema {..} = braces (pretty _utModule <> dot <> pretty _utName)

-- | An ID for an AST node.
data TcId = TcId {
  _tiInfo :: Info,
  _tiName :: Text,
  _tiId :: Int
  }

instance Eq TcId where
  a == b = _tiId a == _tiId b && _tiName a == _tiName b
instance Ord TcId where
  a <= b = _tiId a < _tiId b || (_tiId a == _tiId b && _tiName a <= _tiName b)
-- show instance is important, used as variable name
instance Show TcId where show TcId {..} = unpack _tiName ++ show _tiId
instance Pretty TcId where pretty = string . show


-- | Role of an AST in an overload.
data VarRole = ArgVar Int | RetVar
  deriving (Eq,Show,Ord)

data OverloadSpecial = OAt | OSelect deriving (Eq,Show,Enum,Ord)

-- | Combine an AST id with a role.
data Overload m = Overload {
  _oFunName :: Text,
  _oRoles :: M.Map VarRole m,
  _oTypes :: FunTypes UserType,
  _oSolved :: Maybe (FunType UserType),
  _oSpecial :: Maybe OverloadSpecial
    }
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Pretty m => Pretty (Overload m) where
  pretty Overload {..} =
    "Types:" <$$> indent 2 (vsep (map pshow $ toList _oTypes)) <$$>
    "Roles:" <$$> indent 2 (vsep (map (\(k,v) -> pshow k <> colon <+> pretty v) (M.toList _oRoles))) <$$>
    "Solution:" <+> pshow _oSolved <$$>
    "Special:" <+> pshow _oSpecial

data Failure = Failure TcId String deriving (Eq,Ord,Show)

-- | Typechecker state.
data TcState = TcState {
  _tcDebug :: Bool,
  _tcSupply :: Int,
  -- | Maps native app AST to an overloaded function type, and stores result of solver.
  _tcOverloads :: M.Map TcId (Overload (AST Node)),
  _tcFailures :: S.Set Failure,
  -- | Maps ASTs to a type var.
  _tcAstToVar :: M.Map TcId (TypeVar UserType),
  -- | Maps type vars to types.
  _tcVarToTypes :: M.Map (TypeVar UserType) (Type UserType)
  } deriving (Eq,Show)

mkTcState :: Int -> Bool -> TcState
mkTcState sup dbg = TcState dbg sup def def def def

instance Pretty TcState where
  pretty TcState {..} =
    "Overloads:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pretty k <> colon <+> pretty v) $ M.toList _tcOverloads) <$$>
    "AstToVar:" <$$>
    indent 2 (vsep (map (\(k,v) -> pretty k <> colon <+> pretty v) (M.toList _tcAstToVar))) <$$>
    "VarToTypes:" <$$>
    indent 2 (vsep $ map (\(k,v) -> pshow k <> colon <+> pretty v) $ M.toList _tcVarToTypes) <$$>
    prettyFails _tcFailures
    <> hardline

prettyFails :: Foldable f => f Failure -> Doc
prettyFails fs = "Failures:" <$$>
    indent 2 (vsep $ map (string.show) (toList fs))


-- | Typechecker monad.
newtype TC a = TC { unTC :: StateT TcState IO a }
  deriving (Functor,Applicative,Monad,MonadState TcState,MonadIO,MonadThrow,MonadCatch)




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
    _tlName :: Text,
    _tlType :: Type UserType,
    _tlConstVal :: AST t
    } |
  TopTable {
    _tlInfo :: Info,
    _tlName :: Text,
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
    _fName :: Text,
    _fTypes :: FunTypes UserType,
    _fSpecial :: Maybe (SpecialForm,Special t)
    } |
  FDefun {
    _fInfo :: Info,
    _fName :: Text,
    _fType :: FunType UserType,
    _fArgs :: [Named t],
    _fBody :: [AST t],
    _fDocs :: Maybe Text
    }
  deriving (Eq,Functor,Foldable,Traversable,Show)

instance Pretty t => Pretty (Fun t) where
  pretty FNative {..} = "(native " <> pretty _fName <$$>
    indent 2 ("::" <+> align (vsep (map pretty (toList _fTypes)))) <>
      (case _fSpecial of
         Nothing -> mempty
         Just (_,SBinding bod) -> mempty <$$> indent 2 (pretty bod)
         _ -> mempty) <$$>
      ")"
  pretty FDefun {..} = "(defun " <> pretty _fName <$$>
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
  _nnName :: Text,
  _nnNamed :: i,
  _nnId :: TcId
  } deriving (Eq,Ord,Functor,Foldable,Traversable)
instance (Show i) => Show (Named i) where
  show (Named na no _) = show na ++ "(" ++ show no ++ ")"
instance (Pretty i) => Pretty (Named i) where pretty (Named na no _) = dquotes (pretty na) <+> parens (pretty no)

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
  _aNode :: n,
  _aTableName :: TableName
  } |
  Step {
  _aNode :: n,
  _aEntity :: Maybe (AST n),
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




-- | Run monad providing supply seed and debug.
runTC :: Int -> Bool -> TC a -> IO (a, TcState)
runTC sup dbg a = runStateT (unTC a) (mkTcState sup dbg)

-- | Pre-visit or post-visit specification.
data Visit = Pre | Post deriving (Eq,Show)
-- | Type that can walk AST nodes with 'walkAST'
type Visitor m n = Visit -> AST n -> m (AST n)

------------------------------------------------------------------------------
--makeLenses ''AST
aAppArgs :: forall n_an6b. Traversal' (AST n_an6b) [AST n_an6b]
aAppArgs f_auhX (App x1_auhY x2_auhZ x3_aui0)
  = fmap (\ y1_aui1 -> App x1_auhY x2_auhZ y1_aui1) (f_auhX x3_aui0)
aAppArgs _ (Binding x1_aui2 x2_aui3 x3_aui4 x4_aui5)
  = pure (Binding x1_aui2 x2_aui3 x3_aui4 x4_aui5)
aAppArgs _ (List x1_aui6 x2_aui7) = pure (List x1_aui6 x2_aui7)
aAppArgs _ (Object x1_aui8 x2_aui9) = pure (Object x1_aui8 x2_aui9)
aAppArgs _ (Prim x1_auia x2_auib) = pure (Prim x1_auia x2_auib)
aAppArgs _ (Var x1_auic) = pure (Var x1_auic)
aAppArgs _ (Table x1_auid x2_auie) = pure (Table x1_auid x2_auie)
aAppArgs _ (Step x1_auif x2_auig x3_auih x4_auii)
  = pure (Step x1_auif x2_auig x3_auih x4_auii)
{-# INLINE aAppArgs #-}
aAppFun :: forall n_an6b. Traversal' (AST n_an6b) (Fun n_an6b)
aAppFun f_auij (App x1_auik x2_auil x3_auim)
  = fmap (\ y1_auin -> App x1_auik y1_auin x3_auim) (f_auij x2_auil)
aAppFun _ (Binding x1_auio x2_auip x3_auiq x4_auir)
  = pure (Binding x1_auio x2_auip x3_auiq x4_auir)
aAppFun _ (List x1_auis x2_auit) = pure (List x1_auis x2_auit)
aAppFun _ (Object x1_auiu x2_auiv) = pure (Object x1_auiu x2_auiv)
aAppFun _ (Prim x1_auiw x2_auix) = pure (Prim x1_auiw x2_auix)
aAppFun _ (Var x1_auiy) = pure (Var x1_auiy)
aAppFun _ (Table x1_auiz x2_auiA) = pure (Table x1_auiz x2_auiA)
aAppFun _ (Step x1_auiB x2_auiC x3_auiD x4_auiE)
  = pure (Step x1_auiB x2_auiC x3_auiD x4_auiE)
{-# INLINE aAppFun #-}
aBindType ::
  forall n_an6b. Traversal' (AST n_an6b) (BindType n_an6b)
aBindType _ (App x1_auiF x2_auiG x3_auiH)
  = pure (App x1_auiF x2_auiG x3_auiH)
aBindType f_auiI (Binding x1_auiJ x2_auiK x3_auiL x4_auiM)
  = fmap
      (\ y1_auiN -> Binding x1_auiJ x2_auiK x3_auiL y1_auiN)
      (f_auiI x4_auiM)
aBindType _ (List x1_auiO x2_auiP) = pure (List x1_auiO x2_auiP)
aBindType _ (Object x1_auiQ x2_auiR)
  = pure (Object x1_auiQ x2_auiR)
aBindType _ (Prim x1_auiS x2_auiT) = pure (Prim x1_auiS x2_auiT)
aBindType _ (Var x1_auiU) = pure (Var x1_auiU)
aBindType _ (Table x1_auiV x2_auiW) = pure (Table x1_auiV x2_auiW)
aBindType _ (Step x1_auiX x2_auiY x3_auiZ x4_auj0)
  = pure (Step x1_auiX x2_auiY x3_auiZ x4_auj0)
{-# INLINE aBindType #-}
aBindings ::
  forall n_an6b. Traversal' (AST n_an6b) [(Named n_an6b, AST n_an6b)]
aBindings _ (App x1_auj1 x2_auj2 x3_auj3)
  = pure (App x1_auj1 x2_auj2 x3_auj3)
aBindings f_auj4 (Binding x1_auj5 x2_auj6 x3_auj7 x4_auj8)
  = fmap
      (\ y1_auj9 -> Binding x1_auj5 y1_auj9 x3_auj7 x4_auj8)
      (f_auj4 x2_auj6)
aBindings _ (List x1_auja x2_aujb) = pure (List x1_auja x2_aujb)
aBindings _ (Object x1_aujc x2_aujd)
  = pure (Object x1_aujc x2_aujd)
aBindings _ (Prim x1_auje x2_aujf) = pure (Prim x1_auje x2_aujf)
aBindings _ (Var x1_aujg) = pure (Var x1_aujg)
aBindings _ (Table x1_aujh x2_auji) = pure (Table x1_aujh x2_auji)
aBindings _ (Step x1_aujj x2_aujk x3_aujl x4_aujm)
  = pure (Step x1_aujj x2_aujk x3_aujl x4_aujm)
{-# INLINE aBindings #-}
aBody :: forall n_an6b. Traversal' (AST n_an6b) [AST n_an6b]
aBody _ (App x1_aujn x2_aujo x3_aujp)
  = pure (App x1_aujn x2_aujo x3_aujp)
aBody f_aujq (Binding x1_aujr x2_aujs x3_aujt x4_auju)
  = fmap
      (\ y1_aujv -> Binding x1_aujr x2_aujs y1_aujv x4_auju)
      (f_aujq x3_aujt)
aBody _ (List x1_aujw x2_aujx) = pure (List x1_aujw x2_aujx)
aBody _ (Object x1_aujy x2_aujz) = pure (Object x1_aujy x2_aujz)
aBody _ (Prim x1_aujA x2_aujB) = pure (Prim x1_aujA x2_aujB)
aBody _ (Var x1_aujC) = pure (Var x1_aujC)
aBody _ (Table x1_aujD x2_aujE) = pure (Table x1_aujD x2_aujE)
aBody _ (Step x1_aujF x2_aujG x3_aujH x4_aujI)
  = pure (Step x1_aujF x2_aujG x3_aujH x4_aujI)
{-# INLINE aBody #-}
aEntity ::
  forall n_an6b. Traversal' (AST n_an6b) (Maybe (AST n_an6b))
aEntity _ (App x1_aujJ x2_aujK x3_aujL)
  = pure (App x1_aujJ x2_aujK x3_aujL)
aEntity _ (Binding x1_aujM x2_aujN x3_aujO x4_aujP)
  = pure (Binding x1_aujM x2_aujN x3_aujO x4_aujP)
aEntity _ (List x1_aujQ x2_aujR) = pure (List x1_aujQ x2_aujR)
aEntity _ (Object x1_aujS x2_aujT) = pure (Object x1_aujS x2_aujT)
aEntity _ (Prim x1_aujU x2_aujV) = pure (Prim x1_aujU x2_aujV)
aEntity _ (Var x1_aujW) = pure (Var x1_aujW)
aEntity _ (Table x1_aujX x2_aujY) = pure (Table x1_aujX x2_aujY)
aEntity f_aujZ (Step x1_auk0 x2_auk1 x3_auk2 x4_auk3)
  = fmap
      (\ y1_auk4 -> Step x1_auk0 y1_auk4 x3_auk2 x4_auk3)
      (f_aujZ x2_auk1)
{-# INLINE aEntity #-}
aExec :: forall n_an6b. Traversal' (AST n_an6b) (AST n_an6b)
aExec _ (App x1_auk5 x2_auk6 x3_auk7)
  = pure (App x1_auk5 x2_auk6 x3_auk7)
aExec _ (Binding x1_auk8 x2_auk9 x3_auka x4_aukb)
  = pure (Binding x1_auk8 x2_auk9 x3_auka x4_aukb)
aExec _ (List x1_aukc x2_aukd) = pure (List x1_aukc x2_aukd)
aExec _ (Object x1_auke x2_aukf) = pure (Object x1_auke x2_aukf)
aExec _ (Prim x1_aukg x2_aukh) = pure (Prim x1_aukg x2_aukh)
aExec _ (Var x1_auki) = pure (Var x1_auki)
aExec _ (Table x1_aukj x2_aukk) = pure (Table x1_aukj x2_aukk)
aExec f_aukl (Step x1_aukm x2_aukn x3_auko x4_aukp)
  = fmap
      (\ y1_aukq -> Step x1_aukm x2_aukn y1_aukq x4_aukp)
      (f_aukl x3_auko)
{-# INLINE aExec #-}
aList :: forall n_an6b. Traversal' (AST n_an6b) [AST n_an6b]
aList _ (App x1_aukr x2_auks x3_aukt)
  = pure (App x1_aukr x2_auks x3_aukt)
aList _ (Binding x1_auku x2_aukv x3_aukw x4_aukx)
  = pure (Binding x1_auku x2_aukv x3_aukw x4_aukx)
aList f_auky (List x1_aukz x2_aukA)
  = fmap (\ y1_aukB -> List x1_aukz y1_aukB) (f_auky x2_aukA)
aList _ (Object x1_aukC x2_aukD) = pure (Object x1_aukC x2_aukD)
aList _ (Prim x1_aukE x2_aukF) = pure (Prim x1_aukE x2_aukF)
aList _ (Var x1_aukG) = pure (Var x1_aukG)
aList _ (Table x1_aukH x2_aukI) = pure (Table x1_aukH x2_aukI)
aList _ (Step x1_aukJ x2_aukK x3_aukL x4_aukM)
  = pure (Step x1_aukJ x2_aukK x3_aukL x4_aukM)
{-# INLINE aList #-}
aNode :: forall n_an6b. Lens' (AST n_an6b) n_an6b
aNode f_aukN (App x1_aukO x2_aukP x3_aukQ)
  = fmap (\ y1_aukR -> App y1_aukR x2_aukP x3_aukQ) (f_aukN x1_aukO)
aNode f_aukS (Binding x1_aukT x2_aukU x3_aukV x4_aukW)
  = fmap
      (\ y1_aukX -> Binding y1_aukX x2_aukU x3_aukV x4_aukW)
      (f_aukS x1_aukT)
aNode f_aukY (List x1_aukZ x2_aul0)
  = fmap (\ y1_aul1 -> List y1_aul1 x2_aul0) (f_aukY x1_aukZ)
aNode f_aul2 (Object x1_aul3 x2_aul4)
  = fmap (\ y1_aul5 -> Object y1_aul5 x2_aul4) (f_aul2 x1_aul3)
aNode f_aul6 (Prim x1_aul7 x2_aul8)
  = fmap (\ y1_aul9 -> Prim y1_aul9 x2_aul8) (f_aul6 x1_aul7)
aNode f_aula (Var x1_aulb)
  = fmap (\ y1_aulc -> Var y1_aulc) (f_aula x1_aulb)
aNode f_auld (Table x1_aule x2_aulf)
  = fmap (\ y1_aulg -> Table y1_aulg x2_aulf) (f_auld x1_aule)
aNode f_aulh (Step x1_auli x2_aulj x3_aulk x4_aull)
  = fmap
      (\ y1_aulm -> Step y1_aulm x2_aulj x3_aulk x4_aull)
      (f_aulh x1_auli)
{-# INLINE aNode #-}
aObject ::
  forall n_an6b. Traversal' (AST n_an6b) [(AST n_an6b, AST n_an6b)]
aObject _ (App x1_auln x2_aulo x3_aulp)
  = pure (App x1_auln x2_aulo x3_aulp)
aObject _ (Binding x1_aulq x2_aulr x3_auls x4_ault)
  = pure (Binding x1_aulq x2_aulr x3_auls x4_ault)
aObject _ (List x1_aulu x2_aulv) = pure (List x1_aulu x2_aulv)
aObject f_aulw (Object x1_aulx x2_auly)
  = fmap (\ y1_aulz -> Object x1_aulx y1_aulz) (f_aulw x2_auly)
aObject _ (Prim x1_aulA x2_aulB) = pure (Prim x1_aulA x2_aulB)
aObject _ (Var x1_aulC) = pure (Var x1_aulC)
aObject _ (Table x1_aulD x2_aulE) = pure (Table x1_aulD x2_aulE)
aObject _ (Step x1_aulF x2_aulG x3_aulH x4_aulI)
  = pure (Step x1_aulF x2_aulG x3_aulH x4_aulI)
{-# INLINE aObject #-}
aPrimValue :: forall n_an6b. Traversal' (AST n_an6b) PrimValue
aPrimValue _ (App x1_aulJ x2_aulK x3_aulL)
  = pure (App x1_aulJ x2_aulK x3_aulL)
aPrimValue _ (Binding x1_aulM x2_aulN x3_aulO x4_aulP)
  = pure (Binding x1_aulM x2_aulN x3_aulO x4_aulP)
aPrimValue _ (List x1_aulQ x2_aulR) = pure (List x1_aulQ x2_aulR)
aPrimValue _ (Object x1_aulS x2_aulT)
  = pure (Object x1_aulS x2_aulT)
aPrimValue f_aulU (Prim x1_aulV x2_aulW)
  = fmap (\ y1_aulX -> Prim x1_aulV y1_aulX) (f_aulU x2_aulW)
aPrimValue _ (Var x1_aulY) = pure (Var x1_aulY)
aPrimValue _ (Table x1_aulZ x2_aum0) = pure (Table x1_aulZ x2_aum0)
aPrimValue _ (Step x1_aum1 x2_aum2 x3_aum3 x4_aum4)
  = pure (Step x1_aum1 x2_aum2 x3_aum3 x4_aum4)
{-# INLINE aPrimValue #-}
aRollback ::
  forall n_an6b. Traversal' (AST n_an6b) (Maybe (AST n_an6b))
aRollback _ (App x1_aum5 x2_aum6 x3_aum7)
  = pure (App x1_aum5 x2_aum6 x3_aum7)
aRollback _ (Binding x1_aum8 x2_aum9 x3_auma x4_aumb)
  = pure (Binding x1_aum8 x2_aum9 x3_auma x4_aumb)
aRollback _ (List x1_aumc x2_aumd) = pure (List x1_aumc x2_aumd)
aRollback _ (Object x1_aume x2_aumf)
  = pure (Object x1_aume x2_aumf)
aRollback _ (Prim x1_aumg x2_aumh) = pure (Prim x1_aumg x2_aumh)
aRollback _ (Var x1_aumi) = pure (Var x1_aumi)
aRollback _ (Table x1_aumj x2_aumk) = pure (Table x1_aumj x2_aumk)
aRollback f_auml (Step x1_aumm x2_aumn x3_aumo x4_aump)
  = fmap
      (\ y1_aumq -> Step x1_aumm x2_aumn x3_aumo y1_aumq)
      (f_auml x4_aump)
{-# INLINE aRollback #-}
aTableName :: forall n_an6b. Traversal' (AST n_an6b) TableName
aTableName _ (App x1_aumr x2_aums x3_aumt)
  = pure (App x1_aumr x2_aums x3_aumt)
aTableName _ (Binding x1_aumu x2_aumv x3_aumw x4_aumx)
  = pure (Binding x1_aumu x2_aumv x3_aumw x4_aumx)
aTableName _ (List x1_aumy x2_aumz) = pure (List x1_aumy x2_aumz)
aTableName _ (Object x1_aumA x2_aumB)
  = pure (Object x1_aumA x2_aumB)
aTableName _ (Prim x1_aumC x2_aumD) = pure (Prim x1_aumC x2_aumD)
aTableName _ (Var x1_aumE) = pure (Var x1_aumE)
aTableName f_aumF (Table x1_aumG x2_aumH)
  = fmap (\ y1_aumI -> Table x1_aumG y1_aumI) (f_aumF x2_aumH)
aTableName _ (Step x1_aumJ x2_aumK x3_aumL x4_aumM)
  = pure (Step x1_aumJ x2_aumK x3_aumL x4_aumM)
{-# INLINE aTableName #-}


------------------------------------------------------------------------------
--makeLenses ''Fun
fArgs :: forall t_an6d. Traversal' (Fun t_an6d) [Named t_an6d]
fArgs _ (FNative x1_auxZ x2_auy0 x3_auy1 x4_auy2)
  = pure (FNative x1_auxZ x2_auy0 x3_auy1 x4_auy2)
fArgs
  f_auy3
  (FDefun x1_auy4 x2_auy5 x3_auy6 x4_auy7 x5_auy8 x6_auy9)
  = fmap
      (\ y1_auya
         -> FDefun x1_auy4 x2_auy5 x3_auy6 y1_auya x5_auy8 x6_auy9)
      (f_auy3 x4_auy7)
{-# INLINE fArgs #-}
fBody :: forall t_an6d. Traversal' (Fun t_an6d) [AST t_an6d]
fBody _ (FNative x1_auyb x2_auyc x3_auyd x4_auye)
  = pure (FNative x1_auyb x2_auyc x3_auyd x4_auye)
fBody
  f_auyf
  (FDefun x1_auyg x2_auyh x3_auyi x4_auyj x5_auyk x6_auyl)
  = fmap
      (\ y1_auym
         -> FDefun x1_auyg x2_auyh x3_auyi x4_auyj y1_auym x6_auyl)
      (f_auyf x5_auyk)
{-# INLINE fBody #-}
fDocs :: forall t_an6d. Traversal' (Fun t_an6d) (Maybe Text)
fDocs _ (FNative x1_auyn x2_auyo x3_auyp x4_auyq)
  = pure (FNative x1_auyn x2_auyo x3_auyp x4_auyq)
fDocs
  f_auyr
  (FDefun x1_auys x2_auyt x3_auyu x4_auyv x5_auyw x6_auyx)
  = fmap
      (\ y1_auyy
         -> FDefun x1_auys x2_auyt x3_auyu x4_auyv x5_auyw y1_auyy)
      (f_auyr x6_auyx)
{-# INLINE fDocs #-}
fInfo :: forall t_an6d. Lens' (Fun t_an6d) Info
fInfo f_auyz (FNative x1_auyA x2_auyB x3_auyC x4_auyD)
  = fmap
      (\ y1_auyE -> FNative y1_auyE x2_auyB x3_auyC x4_auyD)
      (f_auyz x1_auyA)
fInfo
  f_auyF
  (FDefun x1_auyG x2_auyH x3_auyI x4_auyJ x5_auyK x6_auyL)
  = fmap
      (\ y1_auyM
         -> FDefun y1_auyM x2_auyH x3_auyI x4_auyJ x5_auyK x6_auyL)
      (f_auyF x1_auyG)
{-# INLINE fInfo #-}
fName :: forall t_an6d. Lens' (Fun t_an6d) Text
fName f_auyN (FNative x1_auyO x2_auyP x3_auyQ x4_auyR)
  = fmap
      (\ y1_auyS -> FNative x1_auyO y1_auyS x3_auyQ x4_auyR)
      (f_auyN x2_auyP)
fName
  f_auyT
  (FDefun x1_auyU x2_auyV x3_auyW x4_auyX x5_auyY x6_auyZ)
  = fmap
      (\ y1_auz0
         -> FDefun x1_auyU y1_auz0 x3_auyW x4_auyX x5_auyY x6_auyZ)
      (f_auyT x2_auyV)
{-# INLINE fName #-}
fSpecial ::
  forall t_an6d.
  Traversal' (Fun t_an6d) (Maybe (SpecialForm, Special t_an6d))
fSpecial f_auz1 (FNative x1_auz2 x2_auz3 x3_auz4 x4_auz5)
  = fmap
      (\ y1_auz6 -> FNative x1_auz2 x2_auz3 x3_auz4 y1_auz6)
      (f_auz1 x4_auz5)
fSpecial _ (FDefun x1_auz7 x2_auz8 x3_auz9 x4_auza x5_auzb x6_auzc)
  = pure (FDefun x1_auz7 x2_auz8 x3_auz9 x4_auza x5_auzb x6_auzc)
{-# INLINE fSpecial #-}
fType :: forall t_an6d. Traversal' (Fun t_an6d) (FunType UserType)
fType _ (FNative x1_auzd x2_auze x3_auzf x4_auzg)
  = pure (FNative x1_auzd x2_auze x3_auzf x4_auzg)
fType
  f_auzh
  (FDefun x1_auzi x2_auzj x3_auzk x4_auzl x5_auzm x6_auzn)
  = fmap
      (\ y1_auzo
         -> FDefun x1_auzi x2_auzj y1_auzo x4_auzl x5_auzm x6_auzn)
      (f_auzh x3_auzk)
{-# INLINE fType #-}
fTypes ::
  forall t_an6d. Traversal' (Fun t_an6d) (FunTypes UserType)
fTypes f_auzp (FNative x1_auzq x2_auzr x3_auzs x4_auzt)
  = fmap
      (\ y1_auzu -> FNative x1_auzq x2_auzr y1_auzu x4_auzt)
      (f_auzp x3_auzs)
fTypes _ (FDefun x1_auzv x2_auzw x3_auzx x4_auzy x5_auzz x6_auzA)
  = pure (FDefun x1_auzv x2_auzw x3_auzx x4_auzy x5_auzz x6_auzA)
{-# INLINE fTypes #-}


------------------------------------------------------------------------------
--makeLenses ''TopLevel
tlConstVal ::
  forall t_an6f. Traversal' (TopLevel t_an6f) (AST t_an6f)
tlConstVal _ (TopFun x1_auDc) = pure (TopFun x1_auDc)
tlConstVal f_auDd (TopConst x1_auDe x2_auDf x3_auDg x4_auDh)
  = fmap
      (\ y1_auDi -> TopConst x1_auDe x2_auDf x3_auDg y1_auDi)
      (f_auDd x4_auDh)
tlConstVal _ (TopTable x1_auDj x2_auDk x3_auDl)
  = pure (TopTable x1_auDj x2_auDk x3_auDl)
tlConstVal _ (TopUserType x1_auDm x2_auDn)
  = pure (TopUserType x1_auDm x2_auDn)
{-# INLINE tlConstVal #-}
tlFun :: forall t_an6f. Traversal' (TopLevel t_an6f) (Fun t_an6f)
tlFun f_auDo (TopFun x1_auDp)
  = fmap (\ y1_auDq -> TopFun y1_auDq) (f_auDo x1_auDp)
tlFun _ (TopConst x1_auDr x2_auDs x3_auDt x4_auDu)
  = pure (TopConst x1_auDr x2_auDs x3_auDt x4_auDu)
tlFun _ (TopTable x1_auDv x2_auDw x3_auDx)
  = pure (TopTable x1_auDv x2_auDw x3_auDx)
tlFun _ (TopUserType x1_auDy x2_auDz)
  = pure (TopUserType x1_auDy x2_auDz)
{-# INLINE tlFun #-}
tlInfo :: forall t_an6f. Traversal' (TopLevel t_an6f) Info
tlInfo _ (TopFun x1_auDA) = pure (TopFun x1_auDA)
tlInfo f_auDB (TopConst x1_auDC x2_auDD x3_auDE x4_auDF)
  = fmap
      (\ y1_auDG -> TopConst y1_auDG x2_auDD x3_auDE x4_auDF)
      (f_auDB x1_auDC)
tlInfo f_auDH (TopTable x1_auDI x2_auDJ x3_auDK)
  = fmap
      (\ y1_auDL -> TopTable y1_auDL x2_auDJ x3_auDK) (f_auDH x1_auDI)
tlInfo f_auDM (TopUserType x1_auDN x2_auDO)
  = fmap (\ y1_auDP -> TopUserType y1_auDP x2_auDO) (f_auDM x1_auDN)
{-# INLINE tlInfo #-}
tlName :: forall t_an6f. Traversal' (TopLevel t_an6f) Text
tlName _ (TopFun x1_auDQ) = pure (TopFun x1_auDQ)
tlName f_auDR (TopConst x1_auDS x2_auDT x3_auDU x4_auDV)
  = fmap
      (\ y1_auDW -> TopConst x1_auDS y1_auDW x3_auDU x4_auDV)
      (f_auDR x2_auDT)
tlName f_auDX (TopTable x1_auDY x2_auDZ x3_auE0)
  = fmap
      (\ y1_auE1 -> TopTable x1_auDY y1_auE1 x3_auE0) (f_auDX x2_auDZ)
tlName _ (TopUserType x1_auE2 x2_auE3)
  = pure (TopUserType x1_auE2 x2_auE3)
{-# INLINE tlName #-}
tlType ::
  forall t_an6f. Traversal' (TopLevel t_an6f) (Type UserType)
tlType _ (TopFun x1_auE4) = pure (TopFun x1_auE4)
tlType f_auE5 (TopConst x1_auE6 x2_auE7 x3_auE8 x4_auE9)
  = fmap
      (\ y1_auEa -> TopConst x1_auE6 x2_auE7 y1_auEa x4_auE9)
      (f_auE5 x3_auE8)
tlType f_auEb (TopTable x1_auEc x2_auEd x3_auEe)
  = fmap
      (\ y1_auEf -> TopTable x1_auEc x2_auEd y1_auEf) (f_auEb x3_auEe)
tlType _ (TopUserType x1_auEg x2_auEh)
  = pure (TopUserType x1_auEg x2_auEh)
{-# INLINE tlType #-}
tlUserType :: forall t_an6f. Traversal' (TopLevel t_an6f) UserType
tlUserType _ (TopFun x1_auEi) = pure (TopFun x1_auEi)
tlUserType _ (TopConst x1_auEj x2_auEk x3_auEl x4_auEm)
  = pure (TopConst x1_auEj x2_auEk x3_auEl x4_auEm)
tlUserType _ (TopTable x1_auEn x2_auEo x3_auEp)
  = pure (TopTable x1_auEn x2_auEo x3_auEp)
tlUserType f_auEq (TopUserType x1_auEr x2_auEs)
  = fmap (\ y1_auEt -> TopUserType x1_auEr y1_auEt) (f_auEq x2_auEs)
{-# INLINE tlUserType #-}


------------------------------------------------------------------------------
--makeLenses ''Node
aId :: Lens' Node TcId
aId f_auI3 (Node x1_auI4 x2_auI5)
  = fmap (\ y1_auI6 -> Node y1_auI6 x2_auI5) (f_auI3 x1_auI4)
{-# INLINE aId #-}
aTy :: Lens' Node (Type UserType)
aTy f_auI7 (Node x1_auI8 x2_auI9)
  = fmap (\ y1_auIa -> Node x1_auI8 y1_auIa) (f_auI7 x2_auI9)
{-# INLINE aTy #-}


------------------------------------------------------------------------------
--makeLenses ''TcId
tiId :: Lens' TcId Int
tiId f_auIQ (TcId x1_auIR x2_auIS x3_auIT)
  = fmap (\ y1_auIU -> TcId x1_auIR x2_auIS y1_auIU) (f_auIQ x3_auIT)
{-# INLINE tiId #-}
tiInfo :: Lens' TcId Info
tiInfo f_auIV (TcId x1_auIW x2_auIX x3_auIY)
  = fmap (\ y1_auIZ -> TcId y1_auIZ x2_auIX x3_auIY) (f_auIV x1_auIW)
{-# INLINE tiInfo #-}
tiName :: Lens' TcId Text
tiName f_auJ0 (TcId x1_auJ1 x2_auJ2 x3_auJ3)
  = fmap (\ y1_auJ4 -> TcId x1_auJ1 y1_auJ4 x3_auJ3) (f_auJ0 x2_auJ2)
{-# INLINE tiName #-}


------------------------------------------------------------------------------
--makeLenses ''TcState
tcAstToVar :: Lens' TcState (M.Map TcId (TypeVar UserType))
tcAstToVar
  f_auK1
  (TcState x1_auK2 x2_auK3 x3_auK4 x4_auK5 x5_auK6 x6_auK7)
  = fmap
      (\ y1_auK8
         -> TcState x1_auK2 x2_auK3 x3_auK4 x4_auK5 y1_auK8 x6_auK7)
      (f_auK1 x5_auK6)
{-# INLINE tcAstToVar #-}
tcDebug :: Lens' TcState Bool
tcDebug
  f_auK9
  (TcState x1_auKa x2_auKb x3_auKc x4_auKd x5_auKe x6_auKf)
  = fmap
      (\ y1_auKg
         -> TcState y1_auKg x2_auKb x3_auKc x4_auKd x5_auKe x6_auKf)
      (f_auK9 x1_auKa)
{-# INLINE tcDebug #-}
tcFailures :: Lens' TcState (S.Set Failure)
tcFailures
  f_auKh
  (TcState x1_auKi x2_auKj x3_auKk x4_auKl x5_auKm x6_auKn)
  = fmap
      (\ y1_auKo
         -> TcState x1_auKi x2_auKj x3_auKk y1_auKo x5_auKm x6_auKn)
      (f_auKh x4_auKl)
{-# INLINE tcFailures #-}
tcOverloads :: Lens' TcState (M.Map TcId (Overload (AST Node)))
tcOverloads
  f_auKp
  (TcState x1_auKq x2_auKr x3_auKs x4_auKt x5_auKu x6_auKv)
  = fmap
      (\ y1_auKw
         -> TcState x1_auKq x2_auKr y1_auKw x4_auKt x5_auKu x6_auKv)
      (f_auKp x3_auKs)
{-# INLINE tcOverloads #-}
tcSupply :: Lens' TcState Int
tcSupply
  f_auKx
  (TcState x1_auKy x2_auKz x3_auKA x4_auKB x5_auKC x6_auKD)
  = fmap
      (\ y1_auKE
         -> TcState x1_auKy y1_auKE x3_auKA x4_auKB x5_auKC x6_auKD)
      (f_auKx x2_auKz)
{-# INLINE tcSupply #-}
tcVarToTypes ::
  Lens' TcState (M.Map (TypeVar UserType) (Type UserType))
tcVarToTypes
  f_auKF
  (TcState x1_auKG x2_auKH x3_auKI x4_auKJ x5_auKK x6_auKL)
  = fmap
      (\ y1_auKM
         -> TcState x1_auKG x2_auKH x3_auKI x4_auKJ x5_auKK y1_auKM)
      (f_auKF x6_auKL)
{-# INLINE tcVarToTypes #-}


------------------------------------------------------------------------------
--makeLenses ''Overload
oFunName :: forall m_an6h. Lens' (Overload m_an6h) Text
oFunName f_auMz (Overload x1_auMA x2_auMB x3_auMC x4_auMD x5_auME)
  = fmap
      (\ y1_auMF -> Overload y1_auMF x2_auMB x3_auMC x4_auMD x5_auME)
      (f_auMz x1_auMA)
{-# INLINE oFunName #-}
oRoles ::
  forall m_an6h m_auMy.
  Lens (Overload m_an6h) (Overload m_auMy) (M.Map VarRole m_an6h) (M.Map VarRole m_auMy)
oRoles f_auMG (Overload x1_auMH x2_auMI x3_auMJ x4_auMK x5_auML)
  = fmap
      (\ y1_auMM -> Overload x1_auMH y1_auMM x3_auMJ x4_auMK x5_auML)
      (f_auMG x2_auMI)
{-# INLINE oRoles #-}
oSolved ::
  forall m_an6h. Lens' (Overload m_an6h) (Maybe (FunType UserType))
oSolved f_auMN (Overload x1_auMO x2_auMP x3_auMQ x4_auMR x5_auMS)
  = fmap
      (\ y1_auMT -> Overload x1_auMO x2_auMP x3_auMQ y1_auMT x5_auMS)
      (f_auMN x4_auMR)
{-# INLINE oSolved #-}
oSpecial ::
  forall m_an6h. Lens' (Overload m_an6h) (Maybe OverloadSpecial)
oSpecial f_auMU (Overload x1_auMV x2_auMW x3_auMX x4_auMY x5_auMZ)
  = fmap
      (\ y1_auN0 -> Overload x1_auMV x2_auMW x3_auMX x4_auMY y1_auN0)
      (f_auMU x5_auMZ)
{-# INLINE oSpecial #-}
oTypes ::
  forall m_an6h. Lens' (Overload m_an6h) (FunTypes UserType)
oTypes f_auN1 (Overload x1_auN2 x2_auN3 x3_auN4 x4_auN5 x5_auN6)
  = fmap
      (\ y1_auN7 -> Overload x1_auN2 x2_auN3 y1_auN7 x4_auN5 x5_auN6)
      (f_auN1 x3_auN4)
{-# INLINE oTypes #-}

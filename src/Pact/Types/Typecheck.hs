{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

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
    Failure (..),prettyFails,renderTcFailure,
    TcState (..),tcDebug,tcSupply,tcOverloads,tcOverloadOrder,tcFailures,tcAstToVar,
    tcVarToTypes,tcYieldResume,tcDynEnv,
    DynEnv,
    TC (..), runTC, runTCState, mkTcState,
    PrimValue (..),
    TopLevel (..),tlFun,tlInfo,tlName,tlType,tlConstVal,tlUserType,tlMeta,tlDoc,toplevelInfo,
    Special (..),
    Fun (..),fInfo,fModule,fName,fTypes,fSpecial,fType,fArgs,fBody,fDefType,fRetId,
    Node (..),aId,aTy,
    Named (..),
    AstBindType (..),
    AST (..),aNode,aAppFun,aAppArgs,aBindings,aBody,aBindType,aList,aObject,
    aPrimValue,aEntity,aExec,aRollback,aTableName,aYieldResume,aModel,aDynMember,
    aDynModRef,aModRefSpec,aModRefName,
    Visit(..),Visitor,
    YieldResume(..),yrYield,yrResume,yrCrossChain,
    Schema(..),
    ModSpec(..)
  ) where

import Control.Monad.Catch
import Control.Lens hiding (List)
import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Foldable
import Data.Text (Text, unpack, pack)

import Pact.Types.Lang hiding (App,Object,Step,ModRef)
import Pact.Types.PactError
import Pact.Types.Runtime (ModuleData(..))
import Pact.Types.Pretty
import Pact.Types.Native


data CheckerException = CheckerException Info String deriving (Eq,Ord)

instance Exception CheckerException
instance Show CheckerException where show (CheckerException i s) = renderInfo i ++ ": " ++ s

data Schema = Schema
  { _schName :: TypeName
  , _schModule :: Maybe ModuleName
  , _schFields :: [Arg UserType]
  , _schInfo :: Info
  } deriving (Eq, Ord)

newtype ModSpec = ModSpec { _specModName :: ModuleName }
  deriving (Eq, Ord)

-- | Model a user type. Currently only Schemas are supported..
data UserType = UTSchema Schema | UTModSpec ModSpec
  deriving (Eq,Ord)
instance Show UserType where
  show (UTSchema Schema {..}) = "{" ++ unpack (maybe "" ((<>) "." . asString) _schModule) ++ unpack (asString _schName) ++ " " ++ show _schFields ++ "}"
  show (UTModSpec (ModSpec mn)) = show mn
instance Pretty UserType where
  pretty (UTSchema Schema {..}) = braces (pretty _schModule <> dot <> pretty _schName)
  pretty (UTModSpec (ModSpec mn)) = pretty mn

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
instance Pretty TcId where pretty = viaShow
instance HasInfo TcId where getInfo = _tiInfo


-- | Role of an AST in an overload.
data VarRole = ArgVar Int | RetVar
  deriving (Eq,Show,Ord)

instance Pretty VarRole where pretty = viaShow

data OverloadSpecial = OAt | OSelect deriving (Eq,Show,Enum,Ord)

-- | Combine an AST id with a role.
data Overload m = Overload {
  _oFunName :: Text,
  _oRoles :: M.Map VarRole m,
  _oTypes :: FunTypes UserType,
  _oSolved :: Maybe (FunType UserType),
  _oSpecial :: Maybe OverloadSpecial }
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Pretty m => Pretty (Overload m) where
  pretty Overload {..} = vsep
    [ "Types:"
    , indent 2 $ vsep $ map viaShow $ toList _oTypes
    , "Roles:"
    , indent 2 $ vsep $ map (\(k,v) -> viaShow k <> colon <+> pretty v) (M.toList _oRoles)
    , "Solution:" <+> viaShow _oSolved
    , "Special:" <+> viaShow _oSpecial
    ]

data Failure = Failure TcId String deriving (Eq,Ord,Show)

renderTcFailure :: Failure -> RenderedOutput
renderTcFailure (Failure t m) = RenderedOutput (pack m) (_tiInfo t) OutputFailure

data YieldResume n = YieldResume
  { _yrYield :: Maybe n
  , _yrResume :: Maybe n
  , _yrCrossChain :: !Bool }
  deriving (Eq,Show,Functor,Foldable,Traversable)
instance Default (YieldResume n) where def = YieldResume def def False

-- | Environment for specializing interface dynamic references.
type DynEnv = M.Map ModuleName (ModuleData Ref)

-- | Typechecker state.
data TcState = TcState {
  _tcDebug :: Bool,
  _tcSupply :: Int,
  -- | Maps native app AST to an overloaded function type, and stores result of solver.
  _tcOverloads :: M.Map TcId (Overload (AST Node)),
  _tcOverloadOrder :: [TcId],
  _tcFailures :: S.Set Failure,
  -- | Maps ASTs to a type var.
  _tcAstToVar :: M.Map TcId (TypeVar UserType),
  -- | Maps type vars to types.
  _tcVarToTypes :: M.Map (TypeVar UserType) (Type UserType),
  -- | Used in AST walk to track step yields and resumes.
  _tcYieldResume :: Maybe (YieldResume Node),
  _tcDynEnv :: DynEnv
  } deriving (Eq,Show)

mkTcState :: Int -> Bool -> DynEnv -> TcState
mkTcState sup dbg dynEnv = TcState dbg sup def def def def def def dynEnv

instance Pretty TcState where
  pretty TcState {..} = vsep
    [ "Overloads:"
    , indent 2 $ vsep $ map (\(k,v) -> pretty k <> colon <+> pretty v) $ M.toList _tcOverloads
    , "AstToVar:"
    , indent 2 $ vsep $ map (\(k,v) -> pretty k <> colon <+> pretty v) $ M.toList _tcAstToVar
    , "VarToTypes:"
    , indent 2 $ vsep $ map (\(k,v) -> viaShow k <> colon <+> pretty v) $ M.toList _tcVarToTypes
    , prettyFails _tcFailures
    ] <> hardline

prettyFails :: Foldable f => f Failure -> Doc
prettyFails fs = vsep
  [ "Failures:"
  , indent 2 $ vsep $ map viaShow $ toList fs
  ]


-- | Typechecker monad.
newtype TC a = TC { unTC :: StateT TcState IO a }
  deriving (Functor,Applicative,Monad,MonadState TcState,MonadIO,MonadThrow,MonadCatch)




-- | Storage for literal values.
data PrimValue g =
  PrimLit Literal |
  PrimGuard (Guard g)
  deriving (Eq,Show,Functor,Foldable,Traversable)
instance (Pretty n) => Pretty (PrimValue n) where
  pretty (PrimLit   l) = viaShow l
  pretty (PrimGuard k) = pretty k


-- | A top-level module production.
data TopLevel t =
  TopFun {
    _tlFun :: Fun t,
    _tlMeta :: Meta
    } |
  TopConst {
    _tlInfo :: Info,
    _tlName :: Text,
    _tlType :: Type UserType,
    _tlConstVal :: AST t,
    _tlDoc :: Maybe Text
    } |
  TopTable {
    _tlInfo :: Info,
    _tlName :: Text,
    _tlType :: Type UserType,
    _tlMeta :: Meta
  } |
  TopUserType {
    _tlInfo :: Info,
    _tlUserType :: UserType,
    _tlDoc :: Maybe Text
  }
  deriving (Eq,Functor,Foldable,Traversable,Show)
instance Pretty t => Pretty (TopLevel t) where
  pretty (TopFun f _m) = vsep [ "Fun",  pretty f ]
  pretty (TopConst _i n t v _m) =
    "Const" <+> pretty n <> colon <> vsep [ pretty t, indent 2 (pretty v) ]
  pretty (TopTable _i n t _m) =
    "Table" <+> pretty n <> colon <> pretty t
  pretty (TopUserType _i t _m) = "UserType" <+> pretty t

toplevelInfo :: TopLevel t -> Info
toplevelInfo (TopFun fun _) = _fInfo fun
toplevelInfo TopConst{_tlInfo} = _tlInfo
toplevelInfo TopTable{_tlInfo} = _tlInfo
toplevelInfo TopUserType{_tlInfo} = _tlInfo

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
    _fInfo   :: Info,
    _fModule :: ModuleName,
    _fName   :: Text,
    _fDefType :: DefType,
    _fType   :: FunType UserType,
    _fArgs   :: [Named t],
    _fBody   :: [AST t],
    _fRetId :: TcId
    }
  deriving (Eq,Functor,Foldable,Traversable,Show)

instance Pretty t => Pretty (Fun t) where
  pretty FNative {..} = parensSep
    [ "(native " <> pretty _fName
    , indent 2 ("::" <+> align (vsep (map pretty (toList _fTypes)))) <>
        (case _fSpecial of
           Nothing -> mempty
           Just (_,SBinding bod) -> line <> indent 2 (pretty bod)
           _ -> mempty)
    ]
  pretty FDefun {..} = parensSep
    [ pretty _fDefType <> " " <> pretty _fName
    , indent 2 $ "::" <+> pretty _fType
    , indent 2 $ sep [ "(", indent 2 (sep (map pretty _fArgs)), ")" ]
    , indent 2 $ vsep (map pretty _fBody)
    , ""
    ]


-- | Pair an AST with its type.
data Node = Node {
  _aId :: TcId,
  _aTy :: Type UserType
  } deriving (Eq,Ord)
instance Show Node where
  show (Node i t) = show i ++ "::" ++ show t
instance Pretty Node where
  pretty (Node i t) = pretty i <> "::" <> pretty t
instance HasInfo Node where getInfo = getInfo . _aId

-- | Pair an unescaped, unmangled "bare" name with something.
data Named i = Named {
  _nnName :: Text,
  _nnNamed :: i,
  _nnId :: TcId
  } deriving (Eq,Ord,Functor,Foldable,Traversable)
instance (Show i) => Show (Named i) where
  show (Named na no _) = show na ++ "(" ++ show no ++ ")"
instance (Pretty i) => Pretty (Named i) where pretty (Named na no _) = dquotes (pretty na) <+> parens (pretty no)

data AstBindType n =
  -- | Normal "let" bind
  AstBindLet |
  -- | Schema-style binding, with string value for key
  AstBindSchema n |
  -- | Synthetic binding for function call arguments introduced during inlining
  -- to force call-by-value semantics
  AstBindInlinedCallArgs
  deriving (Eq,Functor,Foldable,Traversable,Ord)

instance (Show n) => Show (AstBindType n) where
  show AstBindLet = "let"
  show (AstBindSchema b) = "bind" ++ show b
  show AstBindInlinedCallArgs = "inlinedCallArgs"
instance (Pretty n) => Pretty (AstBindType n) where
  pretty AstBindLet = "let"
  pretty (AstBindSchema b) = "bind" <> pretty b
  pretty AstBindInlinedCallArgs = "inlinedCallArgs"

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
  _aBindType :: AstBindType n
  } |
  List {
  _aNode :: n,
  _aList :: [AST n]
  } |
  Object {
  _aNode :: n,
  _aObject :: ObjectMap (AST n)
  } |
  Prim {
  _aNode :: n,
  _aPrimValue :: PrimValue (AST n)
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
  _aRollback :: Maybe (AST n),
  _aYieldResume :: Maybe (YieldResume n),
  _aModel :: ![Exp Info]
  } |
  Dynamic {
  _aNode :: n,
  _aDynModRef :: AST n,
  _aDynMember :: Fun n
  } |
  ModRef {
  _aNode :: n,
  _aModRefName :: ModuleName,
  _aModRefSpec :: Maybe [ModuleName]
  }
  deriving (Eq,Functor,Foldable,Traversable,Show)

instance Pretty t => Pretty (AST t) where
  pretty a = case a of
     Prim {..} -> go "Prim" [ equals, pretty _aPrimValue ]
     Var {} -> go "Var" []
     Object {..} -> go "Object" [ pretty _aObject ]
     List {..} -> go "List"
       [ bracketsSep [ indent 2 $ vsep $ map pretty _aList ] ]
     Binding {..} -> go "Binding"
       [ parensSep
         [ pretty _aBindType
         , indent 2 $ vsep $ _aBindings <&> \(k,v) -> parensSep
           [ indent 2 $ pretty k <+> sep [ colon, indent 2 (pretty v) ]
           ]
         , indent 2 $ vsep $ map pretty _aBody
         ]
       ]
     App {..} -> go "App"
       [ indent 2 $ parensSep [ indent 2 $ vsep $ map pretty _aAppArgs ]
       , indent 2 $ pretty _aAppFun
       ]
     Table {} -> go "Table" []
     Step {..} -> go "Step" $
      may _aEntity (\e -> ["Entity" <> colon <> pretty e]) ++
      [ indent 2 $ pretty _aExec ] ++
      may _aRollback (\r -> ["Rollback:", indent 2 (pretty r)])
     Dynamic{..} -> go "Dynamic" [pretty _aDynModRef, pretty _aDynMember]
     ModRef{..} -> go "ModRef" [pretty _aModRefName, pretty _aModRefSpec]
   where
     go :: Text -> [Doc] -> Doc
     go n is = sep (pretty n:pretty (_aNode a):is)
     may Nothing _ = []
     may (Just v) f = f v


makeLenses ''AST
makeLenses ''Fun
makeLenses ''TopLevel
makeLenses ''Node
makeLenses ''TcId
makeLenses ''YieldResume

makeLenses ''TcState
makeLenses ''Overload



-- | Run monad providing supply seed and debug.
runTC :: Int -> Bool -> TC a -> IO (a, TcState)
runTC sup dbg a = runTCState (mkTcState sup dbg def) a

-- | Run monad providing supply seed and debug.
runTCState :: TcState -> TC a -> IO (a, TcState)
runTCState s a = runStateT (unTC a) s

-- | Pre-visit or post-visit specification.
data Visit = Pre | Post deriving (Eq,Show)
-- | Type that can walk AST nodes with 'walkAST'
type Visitor m n = Visit -> AST n -> m (AST n)

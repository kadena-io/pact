{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}

module Pact.Analyze.TypesV2 where

import Control.Monad.Trans.RWS.Strict
--import Control.Monad.IO.Class (liftIO)
import Control.Lens hiding ((.=), op)
import Pact.Typecheck
import Pact.Types
import Pact.Repl
import Data.Either
--import Data.Decimal
import Data.Aeson hiding (Object)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Default
import GHC.Generics
import Control.Exception
import Data.Thyme.Clock
import Data.Thyme.Clock.POSIX

import SmtLib.Syntax.Syntax
import qualified SmtLib.Syntax.Syntax as Smt
import qualified SmtLib.Syntax.ShowSL as SmtShow
import qualified SmtLib.Parsers.CommandsParsers as SmtParser

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Yaml as Yaml
import qualified Text.Parsec as Parsec

-- !!! Orphan needed for dev, delete when finished
instance ToJSON Node where
  toJSON = toJSON . show

data SmtCompilerException =
  SmtCompilerException
  { _smteSrc :: String
  , _smteErr :: String }
  deriving (Eq)

instance Show SmtCompilerException where
  show (SmtCompilerException src err) = src ++ ": " ++ err

instance Exception SmtCompilerException

data SymType = SymInteger
  | SymDecimal
  | SymBool
  | SymString
  | SymTime
  deriving (Show, Eq, Ord, Generic, ToJSON)

newtype SymName = SymName { unSymName :: String } deriving (Show, Eq)
instance ToJSON SymName where
  toJSON (SymName s) = toJSON s

data TableAccess =
  TableRead
  { _taKey :: SymName } |
  TableWrite
  { _taKey :: SymName }
  deriving (Show, Eq, Generic, ToJSON)

data OfTableColumn = OfTableColumn
  { _otcTable :: String
  , _otcColumn :: String
  , _otcAccess :: TableAccess
  } deriving (Show, Eq, Generic, ToJSON)

data SymVar = SymVar
  { _svName :: SymName
  , _svNode :: Node
  , _svType :: SymType
  } deriving (Show, Eq, Generic, ToJSON)
makeLenses ''SymVar

data CompilerState = CompilerState
  { _csVars :: Map Node SymVar
  , _csTableAssoc :: Map SymVar TableAccess
  } deriving (Show, Eq)
makeLenses ''CompilerState

data SmtOperator where
  SingleLevelOp :: QualIdentifier -> SmtOperator
  DoubleLevelOp :: QualIdentifier -> QualIdentifier -> SmtOperator

data CompiledSmt = CompiledSMT
  { _smtCmd :: Command
  , _smtComment :: Maybe String
  } deriving (Show, Eq)

type SmtCompiler a = RWST () [CompiledSmt] CompilerState IO a

class SmtEncoding a where
  encodeSmt :: a -> String

instance SmtEncoding CompiledSmt where
  encodeSmt CompiledSMT{..} = case _smtComment of
    Nothing -> SmtShow.showSL _smtCmd
    Just cmt -> SmtShow.showSL _smtCmd ++ " ; \"" ++ cmt ++ "\""

isCmpOperator :: String -> Bool
isCmpOperator s = Set.member s $ Set.fromList [">", "<", ">=", "<=", "="]

isLogicalOperator :: String -> Bool
isLogicalOperator s = Set.member s $ Set.fromList ["=", "and", "or", "not"]

isNumericalOperator :: String -> Bool
isNumericalOperator s = Set.member s $ Set.fromList ["+", "-", "*", "/"]

isBasicOperator :: String -> Bool
isBasicOperator s = isCmpOperator s || isLogicalOperator s || isNumericalOperator s

basicOperatorToQualId :: String -> SmtOperator
basicOperatorToQualId o
  | o == ">" = SingleLevelOp $ QIdentifier $ ISymbol ">"
  | o == ">=" = SingleLevelOp $ QIdentifier $ ISymbol ">="
  | o == "<" = SingleLevelOp $ QIdentifier $ ISymbol "<"
  | o == "<=" = SingleLevelOp $ QIdentifier $ ISymbol "<="
  | o == "=" = SingleLevelOp $ QIdentifier $ ISymbol "="
  | o == "and" = SingleLevelOp $ QIdentifier $ ISymbol "and"
  | o == "or" = SingleLevelOp $ QIdentifier $ ISymbol "or"
  | o == "not" = SingleLevelOp $ QIdentifier $ ISymbol "not"
  | o == "+" = SingleLevelOp $ QIdentifier $ ISymbol "+"
  | o == "-" = SingleLevelOp $ QIdentifier $ ISymbol "-"
  | o == "*" = SingleLevelOp $ QIdentifier $ ISymbol "*"
  | o == "/" = SingleLevelOp $ QIdentifier $ ISymbol "/"
  | o == "!=" = DoubleLevelOp (QIdentifier $ ISymbol "not") (QIdentifier $ ISymbol "=")
  | otherwise = throw $ SmtCompilerException "basicOperatorToQualId" $ "operator not supported -> " ++ o

isAppView :: AST Node -> (Bool, AST Node)
isAppView app@(App _ _ _) = (True, app)
isAppView _ = (False, undefined)

isInsertOrUpdate :: Fun Node -> Maybe String
isInsertOrUpdate (NativeFunc "insert") = Just "insert"
isInsertOrUpdate (NativeFunc "update") = Just "update"
isInsertOrUpdate _ = Nothing

ofPrimType :: Node -> Maybe PrimType
ofPrimType (Node _ (TyPrim ty)) = Just ty
ofPrimType _ = Nothing

tcIdToUniqueId :: TcId -> String
tcIdToUniqueId (TcId _ name' nonce') = name' ++ show nonce'

tcIdToSymName :: TcId -> SymName
tcIdToSymName = SymName . tcIdToUniqueId

nodeToTerm :: Node -> SmtCompiler Smt.Term
nodeToTerm n = do
  csVars' <- use csVars
  case Map.lookup n csVars' of
    Nothing -> do
      throw $ SmtCompilerException "nodeToTerm" ("Variable " ++ show n ++ "not found in: " ++ (show csVars'))
    Just SymVar{..} -> return $ TermQualIdentifier $ QIdentifier $ ISymbol $ unSymName _svName

pattern OfPrimType pType <- (ofPrimType -> Just pType)

pattern RawTableName t <- (Table (Node (TcId _ t _) _))

-- pattern Obj_Key_Val key' val' <- (Prim _ (PrimLit (LString key')), val')

pattern NativeFunc f <- (FNative _ f _ _)
--pattern FDEFUN args bdy <- (FDefun _ _ _ args bdy _)
pattern NativeFuncSpecial f bdy <- (FNative _ f _ (Just (_,SBinding bdy)))

pattern AST_Lit lit <- (Prim _ (PrimLit lit))
pattern AST_Obj objNode kvs <- (Object objNode kvs)

-- pattern Args_Var var <- [Var var]
pattern Args_Lit lit <- [AST_Lit lit]
pattern Args_Var var1 <- [(Var var1)]
pattern Args_Var_Var var1 var2 <- [(Var var1),(Var var2)]
pattern Args_Var_Lit var lit <- [(Var var),AST_Lit lit]
pattern Args_Lit_Var lit var <- [AST_Lit lit,(Var var)]
pattern Args_App_App app1 app2 <- [(isAppView -> (True, app1)),(isAppView -> (True, app2))]
-- pattern Args_App_Lit_Var app' lit var <- [(isAppView -> (True,app')),AST_Lit lit,(Var var)]
pattern Args_App_Lit app' lit' <- [(isAppView -> (True,app')),AST_Lit lit']
pattern Args_App_Lit_Lit app' lit1 lit2 <- [(isAppView -> (True,app')),AST_Lit lit1,AST_Lit lit2]

--pattern AppFDefun fDefArgs fDefBdy appArgs <- (App _ (FDEFUN fDefArgs fDefBdy) appArgs)
pattern NegativeVar var' <- (App _ (NativeFunc "-") (Args_Var var'))
pattern NegativeLit lit' <- (App _ (NativeFunc "-") (Args_Lit lit'))
pattern NativeFunc_Lit_Var f lit' var' <- (App _ (NativeFunc f) (Args_Lit_Var lit' var'))
pattern NativeFunc_Var_Lit f var' lit' <- (App _ (NativeFunc f) (Args_Var_Lit var' lit'))
pattern NativeFunc_Var_Var f var1 var2 <- (App _ (NativeFunc f) (Args_Var_Var var1 var2))
pattern NativeFunc_App_App f app1 app2 <- (App _ (NativeFunc f) (Args_App_App app1 app2))
pattern IF_App_Lit_Lit app' lit1 lit2 <- (App _ (NativeFunc "if") (Args_App_Lit_Lit app' lit1 lit2))
pattern ENFORCE_App_msg app' msg' <- (App _ (NativeFunc "enforce") (Args_App_Lit app' (LString msg')))
pattern BINDING bindings' bdy' <- (Binding _ bindings' bdy' _)
pattern ENFORCEKEYSET keyset' <- (App _ (NativeFunc "enforce-keyset") (Args_Lit (LString keyset')))
pattern INSERT_or_UPDATE fnName' table' key' kvs' <- (App _ (isInsertOrUpdate -> (Just fnName')) [RawTableName table', key', AST_Obj _ kvs'])
pattern WITHREAD table' key' bindings' bdy' <- (App _ (NativeFuncSpecial "with-read" (BINDING bindings' bdy')) [RawTableName table', key'])
-- Unsupported currently
pattern READ <- (App _ (NativeFunc "read") _)

literalToTerm :: Literal -> Smt.Term
literalToTerm (LBool v) = TermQualIdentifier $ QIdentifier $ ISymbol (if v then "true" else "false")
literalToTerm (LString v) = TermSpecConstant $ SpecConstantString $ show v
literalToTerm (LInteger v) = TermSpecConstant $ SpecConstantNumeral v
literalToTerm (LDecimal v) = TermSpecConstant $ SpecConstantDecimal $ show v
literalToTerm (LTime t) = TermSpecConstant $ SpecConstantDecimal $ init $ show (t ^. posixTime)

compileAst :: Maybe Node -> [AST Node] -> SmtCompiler ()
compileAst Nothing [] = return ()
compileAst (Just retNode') (ast':[]) = undefined -- do compilation, (assert (= retNode' <final thing>))
compileAst mNode (ast':rest) = undefined -- tell (toSmtCommand $ compile ast') >> compileAst mNode rest

--TopFun
--  { _tlFun =
--      FDefun
--      { _fInfo = "(defun tricky1 (a:integer b:integer) (enforce (and (> a b) (enf-gt a b))))"
--      , _fName = "analyze-tests.tricky1"
--      , _fType = "(a:integer b:integer)-><n>"
--      , _fArgs = ["a"(analyze-tests.tricky1_a0::integer),"b"(analyze-tests.tricky1_b1::integer)]
--      , _fBody =
--        [ App { _aNode = appNenforce2::bool
--              , _aAppFun = FNative
--                { _fInfo = ""
--                , _fName = "enforce"
--                , _fTypes = "(test:bool msg:string)->bool :| []"
--                , _fSpecial = Nothing}
--              , _aAppArgs =
--                [ App { _aNode = appNand3::bool
--                      , _aAppFun = FNative
--                        { _fInfo = ""
--                        , _fName = "and"
--                        , _fTypes = "(x:bool y:bool)->bool :| []"
--                        , _fSpecial = Nothing}
--                      , _aAppArgs =
--                        [ App { _aNode = appN>4::bool
--                              , _aAppFun = FNative {_fInfo = "", _fName = ">", _fTypes = "(x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]>)->bool :| []", _fSpecial = Nothing}
--                              , _aAppArgs = [Var {_aNode = analyze-tests.tricky1_a0::integer},Var {_aNode = analyze-tests.tricky1_b1::integer}]
--                              }
--                        , App { _aNode = appDanalyze-tests.enf-gt9::bool
--                              , _aAppFun = FDefun { _fInfo = "(defun enf-gt (a:integer b:integer) (enforce (> a b)))"
--                                                  , _fName = "analyze-tests.enf-gt"
--                                                  , _fType = "(a:integer b:integer)-><m>"
--                                                  , _fArgs = ["a"(analyze-tests.enf-gt_a5::integer),"b"(analyze-tests.enf-gt_b6::integer)]
--                                                  , _fBody = [App {_aNode = appNenforce7::bool
--                                                                  , _aAppFun = FNative {_fInfo = "", _fName = "enforce", _fTypes = "(test:bool msg:string)->bool :| []", _fSpecial = Nothing}
--                                                                  , _aAppArgs =
--                                                                    [ App {_aNode = appN>8::bool
--                                                                          , _aAppFun = FNative {_fInfo = "", _fName = ">"
--                                                                                               , _fTypes = "(x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]>)->bool :| []"
--                                                                                               , _fSpecial = Nothing}
--                                                                          , _aAppArgs = [Var {_aNode = analyze-tests.tricky1_a0::integer},Var {_aNode = analyze-tests.tricky1_b1::integer}]
--                                                                          }
--                                                                    ]
--                                                                  }
--                                                             ]
--                                                  , _fDocs = Nothing}
--                              , _aAppArgs = [Var {_aNode = analyze-tests.tricky1_a0::integer},Var {_aNode = analyze-tests.tricky1_b1::integer}]
--                              }
--                        ]
--                      }
--                ]
--              }
--        ]
--      , _fDocs = Nothing}}

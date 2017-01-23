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
--import Pact.Repl
--import Data.Either
--import Data.Decimal
import Data.Aeson hiding (Object)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
--import qualified Data.HashMap.Strict as HM
--import Data.Default
import GHC.Generics
import Control.Exception
--import Data.Thyme.Clock
import Data.Thyme.Clock.POSIX

import SmtLib.Syntax.Syntax
import qualified SmtLib.Syntax.Syntax as Smt
import qualified SmtLib.Syntax.ShowSL as SmtShow
--import qualified SmtLib.Parsers.CommandsParsers as SmtParser

--import qualified Data.ByteString.Char8 as BS8
--import qualified Data.Yaml as Yaml
--import qualified Text.Parsec as Parsec

import qualified Pact.Analyze.Types as TypesV1

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

data CompiledSmt = CompiledSmt
  { _smtCmd :: Command
  , _smtComment :: Maybe String
  } deriving (Show, Eq)

type SmtCompiler a = RWST () [CompiledSmt] CompilerState IO a

class SmtEncoding a where
  encodeSmt :: a -> String

instance SmtEncoding CompiledSmt where
  encodeSmt CompiledSmt{..} = case _smtComment of
    Nothing -> SmtShow.showSL _smtCmd
    Just cmt -> SmtShow.showSL _smtCmd ++ " ; \"" ++ cmt ++ "\""

isCmpOperator :: String -> Bool
isCmpOperator s = Set.member s $ Set.fromList [">", "<", ">=", "<=", "="]

isLogicalOperator :: String -> Bool
isLogicalOperator s = Set.member s $ Set.fromList ["=", "and", "or", "not", "!="]

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

isVarOrPrim :: AST Node -> Bool
isVarOrPrim (Prim _ _) = True
isVarOrPrim (Var _) = True
isVarOrPrim _ = False

ofVarOrPrim :: [AST Node] -> Either [AST Node] [AST Node]
ofVarOrPrim args = if all isVarOrPrim args then Right args else Left args

ofPrimType :: Node -> Maybe PrimType
ofPrimType (Node _ (TyPrim ty)) = Just ty
ofPrimType _ = Nothing

ofBasicOperators :: String -> Either String String
ofBasicOperators s = if isBasicOperator s then Right s else Left s

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

convertType :: Node -> SymType
convertType (OfPrimType TyInteger) = SymInteger
convertType (OfPrimType TyBool) = SymBool
convertType (OfPrimType TyDecimal) = SymDecimal
convertType (OfPrimType TyString) = SymString
convertType (OfPrimType TyTime) = SymTime
convertType n = throw $ SmtCompilerException "convertType" $ "node's type is not supported: " ++ show n

symTypeToSortId :: SymType -> Sort
symTypeToSortId SymInteger = SortId $ ISymbol "Int"
symTypeToSortId SymBool = SortId $ ISymbol "Bool"
symTypeToSortId SymDecimal = SortId $ ISymbol "Real"
symTypeToSortId SymString = SortId $ ISymbol "String"
symTypeToSortId SymTime = SortId $ ISymbol "Real"

mkSymVar :: Node -> SymVar
mkSymVar node'@(Node tcId _) = newVar
  where
    convType = convertType node'
    convName = tcIdToSymName tcId
    newVar = SymVar { _svName = convName, _svNode = node', _svType = convType }

symVarToDeclareConst :: SymVar -> Command
symVarToDeclareConst SymVar{..} = DeclareFun (unSymName _svName) [] (symTypeToSortId _svType)

pattern OfPrimType pType <- (ofPrimType -> Just pType)

pattern RawTableName t <- (Table (Node (TcId _ t _) _))

-- pattern Obj_Key_Val key' val' <- (Prim _ (PrimLit (LString key')), val')

pattern NativeFunc f <- (FNative _ f _ _)
pattern UserFunc args bdy <- (FDefun _ _ _ args bdy _)
pattern NativeFuncSpecial f bdy <- (FNative _ f _ (Just (_,SBinding bdy)))

pattern AST_Var var <- (Var var)
pattern AST_Lit lit <- (Prim _ (PrimLit lit))
pattern AST_NFun node' fn' args' <- (App node' (NativeFunc fn') args')
pattern AST_NFun_Basic fn' args' <- AST_NFun _ (ofBasicOperators -> Right fn') args'
pattern AST_UFun node' bdy' args' <- (App node' (UserFunc _ bdy') args')
pattern AST_Enforce node' app' msg' <- (App node' (NativeFunc "enforce") [app', AST_Lit (LString msg')])
pattern AST_If node' cond' ifTrue' ifFalse' <- (App node' (NativeFunc "if") [cond', ifTrue', ifFalse'])
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
pattern BINDING bindings' bdy' <- (Binding _ bindings' bdy' _)
pattern ENFORCEKEYSET keyset' <- (App _ (NativeFunc "enforce-keyset") (Args_Lit (LString keyset')))
pattern INSERT_or_UPDATE fnName' table' key' kvs' <- (App _ (isInsertOrUpdate -> (Just fnName')) [RawTableName table', key', AST_Obj _ kvs'])
pattern WITHREAD table' key' bindings' bdy' <- (App _ (NativeFuncSpecial "with-read" (BINDING bindings' bdy')) [RawTableName table', key'])
-- Unsupported currently
pattern READ <- (App _ (NativeFunc "read") _)

literalToTerm :: Literal -> Smt.Term
literalToTerm (LBool v) = boolAsTerm v
literalToTerm (LString v) = TermSpecConstant $ SpecConstantString $ show v
literalToTerm (LInteger v) = TermSpecConstant $ SpecConstantNumeral v
literalToTerm (LDecimal v) = TermSpecConstant $ SpecConstantDecimal $ show v
literalToTerm (LTime t) = TermSpecConstant $ SpecConstantDecimal $ init $ show (t ^. posixTime)

boolAsTerm :: Bool -> Smt.Term
boolAsTerm b = TermQualIdentifier $ QIdentifier $ ISymbol (if b then "true" else "false")

eqAsQID :: QualIdentifier
eqAsQID = QIdentifier $ ISymbol "="

negAsQID :: QualIdentifier
negAsQID = QIdentifier $ ISymbol "-"

iteAsQID :: QualIdentifier
iteAsQID = QIdentifier $ ISymbol "ite"

mkIteTerm :: Smt.Term -> Smt.Term -> Smt.Term -> Smt.Term
mkIteTerm cond ifTrue ifFalse = TermQualIdentifierT iteAsQID [cond, ifTrue, ifFalse]

negateTerm :: Smt.Term -> Smt.Term
negateTerm t = TermQualIdentifierT (QIdentifier (ISymbol "not")) [t]

implication :: Smt.Term -> Smt.Term -> Smt.Term
implication t1 t2 = TermQualIdentifierT implQID [t1, t2]
  where
    implQID = QIdentifier (ISymbol "=>")

assertEquality :: InIf -> Smt.Term -> Smt.Term -> Maybe String -> SmtCompiler CompiledSmt
assertEquality inIf t1 t2 cmt = assertTerm inIf (TermQualIdentifierT eqAsQID [t1, t2]) cmt

assertTerm :: InIf -> Smt.Term -> Maybe String -> SmtCompiler CompiledSmt
assertTerm NoIf t1 cmt = return $ CompiledSmt (Assert t1) cmt
assertTerm (IfTrue node) t1 cmt = do
  node' <- nodeToTerm node
  return $ CompiledSmt (Assert $ implication node' t1) cmt
assertTerm (IfFalse node) t1 cmt = do
  node' <- nodeToTerm node
  return $ CompiledSmt (Assert $ implication (negateTerm node') t1) cmt

-- | adds node to SymVars
trackNewNode :: Node -> SmtCompiler ()
trackNewNode n = do
  newSv <- return $ mkSymVar n
  csVars' <- use csVars
  if Map.member n csVars'
  then throw $ SmtCompilerException "trackNewNode" $ "node is already tracked: " ++ show n ++ "/n" ++ show csVars'
  else do
    csVars %= Map.insert n newSv
    tell [CompiledSmt (symVarToDeclareConst newSv) Nothing]

tellCmd :: CompiledSmt -> SmtCompiler ()
tellCmd c = tell [c]

-- | This is used to track the ifCond term: (= d (if a b c)) <-- it tracks a
data InIf = IfTrue Node | IfFalse Node | NoIf deriving (Show, Eq)
data ParentRel = NoRelation | HasRelation Node deriving (Show, Eq)

addToNodeTcIdName :: String -> Node -> Node
addToNodeTcIdName s = over (aId.tiName) (\x -> x ++ "-" ++ s)

mkIteNodes :: Node -> SmtCompiler (Node, Node, Node, Node)
mkIteNodes node'@(Node _ (TyPrim _)) = do
  let ifRes = node'
      ifCond = (addToNodeTcIdName "cond" node') { _aTy = TyPrim TyBool}
      ifTrue = (addToNodeTcIdName "true" node')
      ifFalse = (addToNodeTcIdName "false" node')
  trackNewNode ifRes
  trackNewNode ifCond
  trackNewNode ifTrue
  trackNewNode ifFalse
  return (ifRes, ifCond, ifTrue, ifFalse)
mkIteNodes (Node _ ty) = throw $ SmtCompilerException "mkIteNodes" $ "only ifs that return a primitive type are supported, not: " ++ show ty

-- | the wrapper
compileBody :: InIf -> ParentRel -> [AST Node] -> SmtCompiler ()
compileBody inIf parentRel ast' = do -- do compilation, (assert (= retNode' <final thing>))
  fin <- last <$> mapM (compileNode inIf) ast'
  case parentRel of
    NoRelation -> return ()
    HasRelation retNode -> do
      retNode' <- nodeToTerm retNode
      assertEquality inIf retNode' fin Nothing >>= tellCmd
      return ()

-- | the workhorse of compilation
compileNode :: InIf -> AST Node -> SmtCompiler Smt.Term
-- #BEGIN# Handle Lits and Vars
compileNode _ (AST_Lit lit) = return $ literalToTerm lit
compileNode _ (NegativeLit lit) = do
  return $ TermQualIdentifierT negAsQID [literalToTerm lit]
compileNode _ (AST_Var var) = nodeToTerm var
compileNode _ (NegativeVar var) = do
  v' <- nodeToTerm var
  return $ TermQualIdentifierT negAsQID [v']
-- #END# Handle Lits and Vars

compileNode inIf (AST_If node' cond' ifTrue' ifFalse') = do
  (ifRetNode, ifCondNode, ifTrueNode, ifFalseNode) <- mkIteNodes node'
  ifRetTerm <- nodeToTerm ifRetNode
  ifCondTerm <- nodeToTerm ifCondNode
  ifTrueTerm <- nodeToTerm ifTrueNode
  ifFalseTerm <- nodeToTerm ifFalseNode

  -- this may be verbose, I think we can do without the extra ifTrueTerm and ifFalseTerm
  -- assert relationship between the conditional and the return value
  assertEquality (IfTrue ifCondNode) ifRetTerm ifTrueTerm Nothing >>= tellCmd
  assertEquality (IfFalse ifCondNode) ifRetTerm ifFalseTerm Nothing >>= tellCmd

  -- compile the conditional, set it equal to the ifs cond node
  ifsCond <- compileNode inIf cond'
  assertEquality inIf ifCondTerm ifsCond Nothing >>= tellCmd

  -- compile the left and right side, using the implication of the
  -- ifCondNode to imply only when true or false (so asserts don't bleed out)
  ifsTrueRes <- compileNode (IfTrue ifCondNode) ifTrue'
  assertEquality (IfTrue ifCondNode) ifTrueTerm ifsTrueRes Nothing >>= tellCmd

  ifsFalseRes <- compileNode (IfFalse ifCondNode) ifFalse'
  assertEquality (IfFalse ifCondNode) ifFalseTerm ifsFalseRes Nothing >>= tellCmd

  return ifRetTerm

-- #BEGIN# Handle Natives Section
compileNode inIf (AST_NFun_Basic fn args) = do
  args' <- mapM (compileNode inIf) args
  case basicOperatorToQualId fn of
    SingleLevelOp op' -> return $ TermQualIdentifierT op' args'
    DoubleLevelOp op1 op2 -> return $ TermQualIdentifierT op1 $ [TermQualIdentifierT op2 args']
compileNode inIf (AST_Enforce node' app' msg') = do
  trackNewNode node'
  action <- compileNode inIf app'
  assertTerm inIf action (Just $ "enforces: " ++ msg') >>= tellCmd
  nodeToTerm node'
-- #END# Handle Natives Section

-- #BEGIN# Handle User Functions Section
compileNode inIf (AST_UFun node' bdy' _args') = do
  trackNewNode node'
  compileBody inIf (HasRelation node') bdy'
  nodeToTerm node'
compileNode _ err = throw $ SmtCompilerException "compileNode" $ "unsupported construct: " ++ show err
-- #END# Handle User Functions Section

declareTopLevelVars :: SmtCompiler ()
declareTopLevelVars = do
  csVars' <- use csVars
  mapM_ (\newSv -> tell [CompiledSmt (symVarToDeclareConst newSv) Nothing]) $ Map.elems csVars'

analyzeFunction :: TopLevel Node -> IO (Either SmtCompilerException [String])
analyzeFunction (TopFun (FDefun _ _ _ args' bdy' _)) = try $ do
  let initState = CompilerState
                    { _csVars = (Map.fromList $ (\x -> (x, mkSymVar x)) . _nnNamed <$> args')
                    , _csTableAssoc = Map.empty}
  ((), _cstate, res) <- runRWST (declareTopLevelVars >> compileBody NoIf NoRelation bdy') () initState
  return $ encodeSmt <$> res
analyzeFunction _ = return $ Left $ SmtCompilerException "analyzeFunction" "Top-Level Function analysis can only work on User defined functions (i.e. FDefun)"

runCompiler :: String -> IO ()
runCompiler s = do
  f <- TypesV1._getSampFunc s
  r <- analyzeFunction f
  case r of
    Left err -> putStrLn $ show err
    Right res -> putStrLn $ unlines res

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

--TopFun
--  { _tlFun = FDefun
--    { _fInfo = "defun tricky1 (a:integer b:integer)"
--    , _fName = "analyze-tests.tricky1"
--    , _fType = (a:integer b:integer)-><n>
--    , _fArgs = ["a"(analyze-tests.tricky1_a0::integer),"b"(analyze-tests.tricky1_b1::integer)]
--    , _fBody =
--      [ App { _aNode = appNenforce2::bool
--            , _aAppFun = FNative {_fInfo = "", _fName = "enforce", _fTypes = "(test:bool msg:string)->bool :| []", _fSpecial = Nothing}
--            , _aAppArgs =
--              [ App { _aNode = appNand3::bool, _aAppFun = FNative {_fInfo = "", _fName = "and", _fTypes = "(x:bool y:bool)->bool :| []", _fSpecial = Nothing}
--                    , _aAppArgs =
--                      [App { _aNode = appN>4::bool
--                           , _aAppFun = FNative {_fInfo = "", _fName = ">", _fTypes = "(x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]>)->bool :| []", _fSpecial = Nothing}
--                           , _aAppArgs =
--                             [ Var {_aNode = analyze-tests.tricky1_a0::integer}
--                             , Var {_aNode = analyze-tests.tricky1_b1::integer}]
--                           }
--                      ,App {_aNode = appDanalyze-tests.enf-gt10::bool
--                           , _aAppFun =
--                             FDefun { _fInfo = "(defun enf-gt (a:integer b:integer)"
--                                    , _fName = "analyze-tests.enf-gt"
--                                    , _fType = (a:integer b:integer)-><m>
--                                    , _fArgs = ["a"(analyze-tests.enf-gt_a5::integer),"b"(analyze-tests.enf-gt_b6::integer)]
--                                    , _fBody = [App { _aNode = appNif7::bool
--                                                    , _aAppFun = FNative {_fInfo = ""
--                                                                         , _fName = "if"
--                                                                         , _fTypes = "(cond:bool then:<a> else:<a>)-><a> :| []"
--                                                                         , _fSpecial = Nothing}
--                                                    , _aAppArgs =
--                                                      [ App { _aNode = appN=8::bool
--                                                            , _aAppFun = FNative {_fInfo = , _fName = "=", _fTypes = "(x:<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]> y:<a[integer,string,time,decimal,bool,[<l>],object:<{o}>,keyset]>)->bool :| []", _fSpecial = Nothing}
--                                                            , _aAppArgs = [ Var {_aNode = analyze-tests.tricky1_a0::integer}
--                                                                          , Prim {_aNode = integer9::integer, _aPrimValue = PrimLit 10}]
--                                                            }
--                                                      ,Var {_aNode = analyze-tests.tricky1_a0::integer}
--                                                      ,Var {_aNode = analyze-tests.tricky1_b1::integer}]
--                                                    }
--                                               ]
--                                    , _fDocs = Nothing}
--                           , _aAppArgs = [Var {_aNode = analyze-tests.tricky1_a0::integer},Var {_aNode = analyze-tests.tricky1_b1::integer}]}]},Prim {_aNode = string11::string, _aPrimValue = PrimLit "bar"}]}], _fDocs = Nothing}}

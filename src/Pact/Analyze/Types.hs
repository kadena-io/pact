{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}

module Pact.Analyze.Types
  ( runCompiler
  , runCompilerDebug
  , analyzeFunction
  , SymVar(..), svName, svType, svNode
  , SymName(..)
  , OfTableColumn(..)
  , TableAccess(..)
  , CompiledSmt(..)
  , CompilerState(..), csVars, csTableAssoc
  , SmtEncoding(..)
  , SmtCompilerException(..)
  , symVarToDeclareConst
  , dumpModelsOnSat
  , _parseSmtCmd
  , inferFun
  ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.RWS.Strict
import Control.Exception
import Control.Lens hiding ((.=), op)
import Pact.Typecheck hiding (debug)
import Pact.Types
import Pact.Repl
import Data.Aeson hiding (Object)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Default
import GHC.Generics
import Data.Thyme.Clock.POSIX

import SmtLib.Syntax.Syntax
import qualified SmtLib.Syntax.Syntax as Smt
import qualified SmtLib.Syntax.ShowSL as SmtShow
import qualified SmtLib.Parsers.CommandsParsers as SmtParser

import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse, try)

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

newtype SymName = SymName { unSymName :: String } deriving (Show, Eq, Ord)
instance ToJSON SymName where
  toJSON (SymName s) = toJSON s

data TableAccess =
  TableRead |
  TableWrite
  deriving (Show, Eq, Generic, ToJSON)

data OfTableColumn = OfTableColumn
  { _otcTable :: String
  , _otcColumn :: String
  , _otcKey :: Smt.Term
  , _otcAccess :: TableAccess
  } deriving (Show, Eq, Generic)

data SymVar = SymVar
  { _svName :: SymName
  , _svNode :: Node
  , _svType :: SymType
  } deriving (Show, Eq, Generic, ToJSON, Ord)
makeLenses ''SymVar

data CompilerConf = CompilerConf
  { _enableDebug :: Bool
  } deriving (Show, Eq)
makeLenses ''CompilerConf

data CompilerState = CompilerState
  { _csVars :: Map Node SymVar
  , _csTableAssoc :: Map SymVar OfTableColumn
  } deriving (Show, Eq)
makeLenses ''CompilerState

data SmtOperator where
  SingleLevelOp :: QualIdentifier -> SmtOperator
  DoubleLevelOp :: QualIdentifier -> QualIdentifier -> SmtOperator
  TwoArgOp :: QualIdentifier -> SmtOperator

data CompiledSmt = CompiledSmt
  { _smtCmd :: Command
  , _smtComment :: Maybe String
  } deriving (Show, Eq)

type SmtCompiler a = RWST CompilerConf [CompiledSmt] CompilerState IO a

class SmtEncoding a where
  encodeSmt :: a -> String

instance SmtEncoding CompiledSmt where
  encodeSmt CompiledSmt{..} = case _smtComment of
    Nothing -> SmtShow.showSL _smtCmd
    Just cmt -> SmtShow.showSL _smtCmd ++ " ; \"" ++ cmt ++ "\""

isCmpOperator :: String -> Bool
isCmpOperator s = Set.member s $ Set.fromList [">", "<", ">=", "<=", "=", "!="]

isLogicalOperator :: String -> Bool
isLogicalOperator s = Set.member s $ Set.fromList ["and", "or", "not"]

isNumericalOperator :: String -> Bool
isNumericalOperator s = Set.member s $ Set.fromList ["+", "-", "*", "/", "abs", "^"]

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
  | o == "abs" = SingleLevelOp $ QIdentifier $ ISymbol "abs"
  | o == "+" = SingleLevelOp $ QIdentifier $ ISymbol "+"
  | o == "-" = SingleLevelOp $ QIdentifier $ ISymbol "-"
  | o == "*" = SingleLevelOp $ QIdentifier $ ISymbol "*"
  | o == "/" = SingleLevelOp $ QIdentifier $ ISymbol "/"
  | o == "!=" = DoubleLevelOp (QIdentifier $ ISymbol "not") (QIdentifier $ ISymbol "=")
  | o == "^" = TwoArgOp $ QIdentifier $ ISymbol "^"
  | otherwise = throw $ SmtCompilerException "basicOperatorToQualId" $ "operator not supported -> " ++ o

unsupportedOperators :: String -> Bool
unsupportedOperators s = Set.member s $ Set.fromList ["log", "ln", "ceiling", "floor", "mod"]

isUnsupportedOperator :: String -> Maybe String
isUnsupportedOperator s = if unsupportedOperators s then Just s else Nothing

isInsertOrUpdate :: Fun Node -> Maybe String
isInsertOrUpdate (NativeFunc "insert") = Just "insert"
isInsertOrUpdate (NativeFunc "update") = Just "update"
isInsertOrUpdate _ = Nothing

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

mSymVarFromTerm :: Smt.Term -> SmtCompiler (Maybe (Node,SymVar))
mSymVarFromTerm (TermQualIdentifier (QIdentifier (ISymbol svName'))) = do
  svs <- Map.toList . (Map.filter (\SymVar{..}-> svName' == unSymName _svName)) <$> use csVars
  case svs of
    [] -> return $ Nothing
    [sv] -> return $ Just sv
    err -> throw $ SmtCompilerException "mSymVarFromTerm" $ "found more than one SymVar with the same name!\n" ++ show err
mSymVarFromTerm _ = return Nothing

showTermForFmt :: Smt.Term -> SmtCompiler Smt.Term
showTermForFmt t@(TermQualIdentifier _) = do
  mt <- mSymVarFromTerm t
  case mt of
    Nothing -> throw $ SmtCompilerException "showTermForFmt" $ "unable to lookup the tracked term: " ++ SmtShow.showSL t
    Just (_n,_sv@SymVar{..}) -> return $ case _svType of
        SymInteger -> TermQualIdentifierT (QIdentifier (ISymbol "int.to.str")) [t]
        SymBool -> boolAsStrTerm t
        SymString -> t
        SymTime -> throw $ SmtCompilerException "showTermForFmt" $ "Unsupported: (during format) conversion of Time to String"
        SymDecimal -> throw $ SmtCompilerException "showTermForFmt" $ "Unsupported: (during format) conversion of Decimal to String"
        ty -> throw $ SmtCompilerException "showTermForFmt" $ "Unsupported: (during format) conversion to String from: " ++ show ty
showTermForFmt (TermQualIdentifier (QIdentifier (ISymbol "true"))) = return $ TermSpecConstant (SpecConstantString "\"true\"")
showTermForFmt (TermQualIdentifier (QIdentifier (ISymbol "false"))) = return $ TermSpecConstant (SpecConstantString "\"false\"")
showTermForFmt t@(TermSpecConstant (SpecConstantString _)) = return t
showTermForFmt t@(TermSpecConstant (SpecConstantNumeral _)) = return $ TermQualIdentifierT (QIdentifier (ISymbol "int.to.str")) [t]
showTermForFmt t@(TermSpecConstant (SpecConstantDecimal _)) = throw $ SmtCompilerException "showTermForFmt" $ "Unsupported: (during format) conversion to String from literal: " ++ SmtShow.showSL t


boolAsStrTerm :: Smt.Term -> Smt.Term
boolAsStrTerm t = TermQualIdentifierT (QIdentifier (ISymbol "ite"))
  [ t
  , TermSpecConstant (SpecConstantString "\"true\"")
  , TermSpecConstant (SpecConstantString "\"false\"")]

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

-- | Everything is obvious except for time, which works like this:
-- `\(LTime (t :: UTCTime)) -> (init $ show (t ^. posixTime)` :: Smt.Decimal)
-- Decimals (which are actually Reals) are strings in the SMT parser (dunno why)
-- so we convert to POSIX time and show, getting `<some numbers>.<some more numbers>s` and use init to remove the trailing `s`
literalToTerm :: Literal -> Smt.Term
literalToTerm (LBool v) = boolAsTerm v
literalToTerm (LString v) = TermSpecConstant $ SpecConstantString $ show v
literalToTerm (LInteger v) = TermSpecConstant $ SpecConstantNumeral v
literalToTerm (LDecimal v) = TermSpecConstant $ SpecConstantDecimal $ show v
literalToTerm (LTime t) = TermSpecConstant $ SpecConstantDecimal $ init $ show (t ^. posixTime)

boolAsTerm :: Bool -> Smt.Term
boolAsTerm b = TermQualIdentifier $ QIdentifier $ ISymbol (if b then "true" else "false")

stringAsTerm :: String -> Smt.Term
stringAsTerm = TermSpecConstant . SpecConstantString . show

eqAsQID :: QualIdentifier
eqAsQID = QIdentifier $ ISymbol "="

negAsQID :: QualIdentifier
negAsQID = QIdentifier $ ISymbol "-"

negateTerm :: Smt.Term -> Smt.Term
negateTerm t = TermQualIdentifierT (QIdentifier (ISymbol "not")) [t]

-- | Create an SMT implication from two terms. Used to assert <term2> only if <term1> is true
-- NB: implication functions like saying "if `x == true` then y else <I know nothing new>"
implication :: Smt.Term -> Smt.Term -> Smt.Term
implication t1 t2 = TermQualIdentifierT implQID [t1, t2]
  where
    implQID = QIdentifier (ISymbol "=>")

assertEquality :: InIf -> Smt.Term -> Smt.Term -> Maybe String -> SmtCompiler CompiledSmt
assertEquality inIf t1 t2 cmt = assertTerm inIf (TermQualIdentifierT eqAsQID [t1, t2]) cmt

-- | Assert a Smt.Term. If you're in an If block, then imply the assertion (i.e. the ifCond being true implies Smt.Term)
-- ex: NoIf -> `(assert <term>) ; cmt`
-- ex: IfTrue -> `(assert (=> <ifCond> <term>)) ; cmt`
-- ex: IfFalse -> `(assert (=> (not <ifCond>) <term>)) ; cmt`
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
  debug $ show n
  newSv <- return $ mkSymVar n
  csVars' <- use csVars
  if Map.member n csVars'
  then throw $ SmtCompilerException "trackNewNode" $ "node is already tracked:\n" ++ show n ++ "\n" ++ show csVars'
  else do
    csVars %= Map.insert n newSv
    tell' [CompiledSmt (symVarToDeclareConst newSv) Nothing]

tell' :: [CompiledSmt] -> SmtCompiler ()
tell' s = do
  dbg <- view enableDebug
  when dbg $ liftIO $ putStrLn $ unlines $ encodeSmt <$> s
  tell s

debug :: String -> SmtCompiler ()
debug s = do
  dbg <- view enableDebug
  when dbg $ liftIO $ putStrLn $ "## debug ## " ++ s

tellCmd :: CompiledSmt -> SmtCompiler ()
tellCmd c = tell' [c]

-- | This is used to track the ifCond term: (= d (if a b c)) <-- it tracks a
data InIf =
  NoIf |
  IfTrue Node |
  IfFalse Node
  deriving (Show, Eq)

-- | Used to track if compileBody needs to assert that the last node in [AST Node] needs to be returned equal
-- if so: then `(assert (= parentNodeAsTerm lastAstTerm))`
data ParentRel =
  NoRelation |
  HasRelation Node
  deriving (Show, Eq)

addToNodeTcIdName :: String -> Node -> Node
addToNodeTcIdName s = over (aId.tiName) (\x -> x ++ "-" ++ s)

-- | handling if branches is crazy. Use this example to get a handle on what's going on here.
-- NB: we need to handle if there are assertions of the body of ifTrue etc... hence the implication trick
-- NB: NB: implication works like `onlyApplyAssertionWhenTrue :: Smt.Term -> Assert Smt.Term -> IO ()`
-- ; (if (= a 10) (> b 8) (> b a))
-- (declare-fun ifRet () Bool)
-- (declare-fun ifCond () Bool)
-- (declare-fun ifNTrue () Bool)
-- (declare-fun ifNFalse () Bool)
--
-- (assert (= ifCond (= a 10)))
--
-- (assert (=> ifCond (> b 8)))
-- (assert (=> ifCond (= ifNTrue true)))
-- (assert (=> ifCond (= ifRet ifNTrue)))
--
-- (assert (=> (not ifCond) (> b a)))
-- (assert (=> (not ifCond) (= ifNFalse true)))
-- (assert (=> (not ifCond) (= ifRet ifNFalse)))
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

-- | bind variables encountered in a let
bindNewVar :: InIf -> (Named Node, AST Node) -> SmtCompiler ()
bindNewVar inIf ((Named n node'), ast') = do
  trackNewNode node'
  asTerm <- nodeToTerm node'
  resTerm <- compileNode inIf ast'
  when (asTerm /= resTerm) $ assertEquality inIf asTerm resTerm (Just $ "binding " ++ n) >>= tellCmd

-- | bind variables read from the DB
bindTableVar :: InIf -> TableAccess -> String -> Smt.Term -> (Named Node, AST Node) -> SmtCompiler ()
bindTableVar inIf ta table' key' orig@(Named column' node', _) = do
  let otc' = OfTableColumn { _otcTable = table', _otcColumn = column', _otcKey = key', _otcAccess = ta}
  bindNewVar inIf orig
  symVar' <- Map.lookup node' <$> use csVars
  case symVar' of
    Nothing -> throw $ SmtCompilerException "bindTableVar" $ "failed to find associated symVar: " ++ show node'
    Just s -> do
      csTableAssoc %= Map.insert s otc'

-- | construct names for variables that are to be written to the DB
prepTableBindSite :: String -> Int -> String -> (AST Node, AST Node) -> (Named Node, AST Node)
prepTableBindSite tableId objNonce fn (Prim _ (PrimLit (LString field)), ast') =
  let newNode = (addToNodeTcIdName (tableId ++ "-" ++ fn ++ "-" ++ field) (_aNode ast'))
  in (Named field (set (aId.tiId) objNonce newNode),ast')
prepTableBindSite tableId _ _ (Prim _ _, err) = throw $ SmtCompilerException "prepTableBindSite" $ "prepTableBindSite for table " ++ tableId ++ " given incorrect datatype in snd position: " ++ show err
prepTableBindSite tableId _ _ (err, _) = throw $ SmtCompilerException "prepTableBindSite" $ "prepTableBindSite for table " ++ tableId ++ " given incorrect datatype in first position: " ++ show err

-- helper patterns
pattern OfPrimType pType <- (ofPrimType -> Just pType)
pattern RawTableName t <- (Table (Node (TcId _ t _) _))
pattern NativeFunc f <- (FNative _ f _ _)
pattern UserFunc name' args bdy <- (FDefun _ name' _ args bdy _)
pattern NativeFuncSpecial f bdy <- (FNative _ f _ (Just (_,SBinding bdy)))
pattern NodeNonce i <- (Node (TcId _ _ i) _)

-- compileNode's Patterns
pattern AST_Var var <- (Var var)
pattern AST_Lit lit <- (Prim _ (PrimLit lit))
pattern AST_NegativeVar var' <- (App _ (NativeFunc "-") [AST_Var var'])
pattern AST_NegativeLit lit' <- (App _ (NativeFunc "-") [AST_Lit lit'])
pattern AST_NFun node' fn' args' <- (App node' (NativeFunc fn') args')
pattern AST_NFun_Basic fn' args' <- AST_NFun _ (ofBasicOperators -> Right fn') args'
pattern AST_UFun name' node' bdy' args' <- (App node' (UserFunc name' _ bdy') args')
pattern AST_Enforce node' app' msg' <- (App node' (NativeFunc "enforce") [app', AST_Lit (LString msg')])
pattern AST_If node' cond' ifTrue' ifFalse' <- (App node' (NativeFunc "if") [cond', ifTrue', ifFalse'])
pattern AST_EnforceKeyset node' keyset' <- (App node' (NativeFunc "enforce-keyset") [AST_Lit (LString keyset')])
pattern AST_Binding node' bindings' bdy' <- (Binding node' bindings' bdy' _)
pattern AST_WithRead node' table' key' bindings' bdy' <- (App node' (NativeFuncSpecial "with-read" (AST_Binding _ bindings' bdy')) [RawTableName table', key'])
pattern AST_Obj objNode kvs <- (Object objNode kvs)
pattern AST_InsertOrUpdate node' fnName' table' key' objNonce' kvs' <- (App node' (isInsertOrUpdate -> (Just fnName')) [RawTableName table', key', AST_Obj (NodeNonce objNonce') kvs'])
pattern AST_Format node' fmtStr' args' <- (App node' (NativeFunc "format") (AST_Lit (LString fmtStr'):args'))

-- Unsupported currently
pattern AST_Read <- (App _ (NativeFunc "read") _)
pattern AST_AddTime <- (App _ (NativeFunc "add-time") _)
pattern AST_Days <- (App _ (NativeFunc "days") _)
pattern AST_Bind <- (App _ (NativeFuncSpecial "bind" _) _)
pattern AST_UnsupportedOp s <- (App _ (NativeFunc (isUnsupportedOperator -> Just s)) _ )

-- | the wrapper
compileBody :: InIf -> ParentRel -> [AST Node] -> SmtCompiler ()
compileBody inIf parentRel ast' = do -- do compilation, (assert (= retNode' <final thing>))
  results <- mapM (compileNode inIf) ast'
  if null results
  then return () -- throw $ SmtCompilerException "compileBody" "encountered an empty body"
  else do
    case parentRel of
      NoRelation -> return ()
      HasRelation retNode -> do
        retNode' <- nodeToTerm retNode
        assertEquality inIf retNode' (last results) Nothing >>= tellCmd
        return ()

-- | the workhorse of compilation
compileNode :: InIf -> AST Node -> SmtCompiler Smt.Term
-- #BEGIN# Handle Lit, Var, If
compileNode _ (AST_Lit lit) = return $ literalToTerm lit
compileNode _ (AST_NegativeLit lit) = do
  return $ TermQualIdentifierT negAsQID [literalToTerm lit]
compileNode _ (AST_Var var) = nodeToTerm var
compileNode _ (AST_NegativeVar var) = do
  v' <- nodeToTerm var
  return $ TermQualIdentifierT negAsQID [v']
compileNode inIf (AST_Binding node' bindings' bdy') = do
  trackNewNode node'
  mapM_ (bindNewVar inIf) bindings'
  compileBody inIf (HasRelation node') bdy'
  nodeToTerm node'
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
-- #END# Handle Lit, Var, If

-- #BEGIN# Handle Natives Section
compileNode _ AST_Read = throw $ SmtCompilerException "compileNode does not support `read`" "Pact's SMT compiler is still under construction and does not support <object> return types"
compileNode _ AST_AddTime = throw $ SmtCompilerException "compileNode" "does not yet support add-time"
compileNode _ AST_Days = throw $ SmtCompilerException "compileNode" "does not yet support days"
compileNode _ AST_Bind = throw $ SmtCompilerException "compileNode" "does not yet support bind"
compileNode _ (AST_UnsupportedOp s) = throw $ SmtCompilerException "compileNode" $ "Apologies, the operator " ++ s ++ " is not yet supported"
compileNode inIf (AST_NFun_Basic fn args) = do
  args' <- mapM (compileNode inIf) args
  case basicOperatorToQualId fn of
    SingleLevelOp op' -> return $ TermQualIdentifierT op' args'
    TwoArgOp op' -> return $ TermQualIdentifierT op' args'
    DoubleLevelOp op1 op2 -> return $ TermQualIdentifierT op1 $ [TermQualIdentifierT op2 args']
compileNode inIf (AST_Enforce node' app' msg') = do
  trackNewNode node'
  action <- compileNode inIf app'
  assertTerm inIf action (Just $ "enforces: " ++ msg') >>= tellCmd
  nodeToTerm node'
compileNode inIf (AST_EnforceKeyset node' ks) = do
  let newNode = (addToNodeTcIdName ks node')
  trackNewNode newNode
  asTerm <- nodeToTerm newNode
  assertEquality inIf asTerm (boolAsTerm True) Nothing >>= tellCmd
  return asTerm
compileNode inIf (AST_WithRead node' table' key' bindings' bdy') = do
  trackNewNode node'
  keyTerm <- compileNode inIf key'
  mapM_ (bindTableVar inIf TableRead table' keyTerm) bindings'
  compileBody inIf (HasRelation node') bdy'
  nodeToTerm node'
compileNode inIf (AST_InsertOrUpdate node' fn' table' key' objNonce' kvs') = do
  trackNewNode node'
  asTerm <- nodeToTerm node'
  keyTerm <- compileNode inIf key'
  mapM_ (bindTableVar inIf TableWrite table' keyTerm) (prepTableBindSite table' objNonce' fn' <$> kvs')
  assertEquality inIf asTerm (stringAsTerm $ fn' ++ " succeeded") Nothing >>= tellCmd
  return $ asTerm
compileNode inIf (AST_Format node' fmtStr' args') = do
  trackNewNode node'
  asTerm <- nodeToTerm node'
  prepedFmt <- return $ parseFmtStr fmtStr'
  terms' <- mapM (\n -> compileNode inIf n >>= showTermForFmt) args'
  fmtAsStr <- return $ TermQualIdentifierT (QIdentifier $ ISymbol "str.++") $ constructFmt prepedFmt terms'
  assertEquality inIf asTerm fmtAsStr (Just $ "interpreting " ++ fmtStr') >>= tellCmd
  return $ asTerm
-- #END# Handle Natives Section

-- #BEGIN# Handle User Functions Section
compileNode inIf (AST_UFun name' node' bdy' _args') = do
  debug $ "Entered Defun " ++ name'
  trackNewNode node'
  compileBody inIf (HasRelation node') bdy'
  debug $ "Leaving Defun " ++ name'
  nodeToTerm node'
-- #END# Handle User Functions Section
compileNode _ err = throw $ SmtCompilerException "compileNode" $ "unsupported construct: " ++ show err

declareTopLevelVars :: SmtCompiler ()
declareTopLevelVars = do
  csVars' <- use csVars
  mapM_ (\newSv -> tell' [CompiledSmt (symVarToDeclareConst newSv) Nothing]) $ Map.elems csVars'

analyzeFunction :: TopLevel Node -> Bool -> IO (Either SmtCompilerException (CompilerState, [CompiledSmt]))
analyzeFunction (TopFun (FDefun _ _ _ args' bdy' _)) dbg = try $ do
  let initState = CompilerState
                    { _csVars = (Map.fromList $ (\x -> (x, mkSymVar x)) . _nnNamed <$> args')
                    , _csTableAssoc = Map.empty}
      compConf = CompilerConf { _enableDebug = dbg }
  ((), cstate, res) <- runRWST (declareTopLevelVars >> compileBody NoIf NoRelation bdy') compConf initState
  return $ (cstate, ([dumpModelsOnSat] ++ res))
analyzeFunction _ _ = return $ Left $ SmtCompilerException "analyzeFunction" "Top-Level Function analysis can only work on User defined functions (i.e. FDefun)"

dumpModelsOnSat :: CompiledSmt
dumpModelsOnSat = CompiledSmt (SetOption (OptionAttr (AttributeVal ":dump-models" (AttrValueSymbol "true")))) Nothing

loadModule :: FilePath -> ModuleName -> IO ModuleData
loadModule fp mn = do
  (r,s) <- execScript' (Script fp) fp
  either (die def) (const (return ())) r
  case view (rEnv . eeRefStore . rsModules . at mn) s of
    Just m -> return m
    Nothing -> die def $ "Module not found: " ++ show (fp,mn)

loadFun :: FilePath -> ModuleName -> String -> IO Ref
loadFun fp mn fn = loadModule fp mn >>= \(_,m) -> case HM.lookup fn m of
  Nothing -> die def $ "Function not found: " ++ show (fp,mn,fn)
  Just f -> return f

inferFun :: Bool -> FilePath -> ModuleName -> String -> IO (TopLevel Node, TcState)
inferFun dbg fp mn fn = loadFun fp mn fn >>= \r -> runTC 0 dbg (typecheckTopLevel r)

runCompiler :: String -> String -> String -> IO ()
runCompiler = runCompilerDebug False

runCompilerDebug :: Bool -> String -> String -> String -> IO ()
runCompilerDebug dbg replPath' modName' funcName' = do
  f <- fst <$> inferFun False replPath' (ModuleName modName') funcName'
  r <- analyzeFunction f dbg
  case r of
    Left err -> putStrLn $ show err
    Right (_, res) -> putStrLn $ unlines (encodeSmt <$> res)

-- helper stuff

_parseSmtCmd :: String -> Smt.Command
_parseSmtCmd s = let (Right f) = Parsec.parse SmtParser.parseCommand "" s in f

-- App { _aNode = appNformat9::string
--     , _aAppFun = FNative { _fInfo = ""
--                          , _fName = "format", _fTypes = "(template:string vars:*)->string :| []"
--                          , _fSpecial = Nothing}
--     , _aAppArgs =
--       [ Prim {_aNode = string10::string, _aPrimValue = PrimLit "{}:{}"}
--       , Var {_aNode = cp.issue-inventory_owner0::string},Var {_aNode = cp.issue-inventory_cusip1::string}
--       ]}

data FmtStr = Placeholder
            | FmtStrLit String
            deriving (Show, Eq)

parseFmtStr :: String -> [FmtStr]
parseFmtStr s = case Parsec.parse prs "" s of
  Left err -> throw $ SmtCompilerException "parseFmtStr" $ show err
  Right v -> v
  where
    prs = (`manyTill` eof) $  (Parsec.try $ string "{}" >> return Placeholder)
                          <|> (Parsec.try $ do str <- manyTill anyChar (Parsec.try $ lookAhead $ string "{}")
                                               return $ FmtStrLit str)
                          <|> (Parsec.try $ many anyChar >>= return . FmtStrLit)

constructFmt :: [FmtStr] -> [Smt.Term] -> [Smt.Term]
constructFmt [] [] = []
constructFmt [FmtStrLit s] [] = literalToTerm (LString s) : []
constructFmt (Placeholder:_) [] = throw $ SmtCompilerException "constructFmt" $ "cannot interpret `format`: too many placeholders, not enough args!"
constructFmt [] args = throw $ SmtCompilerException "constructFmt" $ "cannot interpret `format`: too many args, not enough placeholders! " ++ show (SmtShow.showSL <$> args)
constructFmt (Placeholder:restFmt) (t:restArgs) = t : constructFmt restFmt restArgs
constructFmt ((FmtStrLit s):restFmt) args = literalToTerm (LString s) : constructFmt restFmt args

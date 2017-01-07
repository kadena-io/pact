{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Pact.Analyze.Types where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Lens hiding ((.=))
import Pact.Typecheck
import Pact.Types
import Data.Either
import Data.Decimal
import Data.Aeson hiding (Object)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>))
import GHC.Generics

import SmtLib.Syntax.Syntax
import SmtLib.Parsers.CommandsParsers (parseCommand)
import Text.Parsec (parse)
import qualified SmtLib.Syntax.Syntax as Smt
import qualified SmtLib.Syntax.ShowSL as SmtShow

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Yaml as Yaml
import qualified Text.Parsec as Parsec
import qualified SmtLib.Parsers.CommandsParsers as SmtParser

data SymType = SymInteger
  | SymDecimal
  | SymBool
  | SymString
  deriving (Show, Eq, Ord, Generic, ToJSON)

newtype SymName = SymName { unSymName :: String } deriving (Show, Eq)

instance ToJSON SymName where
  toJSON (SymName s) = toJSON s

data TrackingStatus = Tracked
  | Untracked {_tWhy :: String}
  | LostTrack {_tWhy :: String}
  deriving (Show, Eq, Generic, ToJSON)

data SymVar = SymVar
  { _svName :: SymName
  , _svType :: Maybe SymType
  , _svTracked :: TrackingStatus
  } deriving (Show, Eq, Generic, ToJSON)
makeLenses ''SymVar

data ProverCtx = ProverCtx
  { _pcInputArgs :: Map TcId SymVar
  } deriving (Show, Eq)
makeLenses ''ProverCtx

data ProverState = ProverState
  { _psVars :: Map TcId SymVar
  , _psNodeSMT :: [Command]
  } deriving (Show, Eq)
makeLenses ''ProverState

instance ToJSON ProverState where
  toJSON ProverState{..} = do
    let declrs = init . SmtShow.showSL <$> rights (symVarToDeclareConst <$> Map.elems _psVars)
    toJSON $ declrs ++ (SmtShow.showSL <$> _psNodeSMT)

data SymAst =
  IfBranch
    { _ifbrCond :: Smt.Term
    , _ifbrTrue :: SymAst
    , _ifbrFalse :: SymAst
    , _saProverState :: ProverState } |
  EnforceConstraint
    { _ecFailsIf :: String
    , _saRest :: SymAst
    , _saProverState :: ProverState } |
  ErrorLeaf
    { _elWhy :: String
    , _saProverState :: ProverState } |
  ReturnLit
    { _rlResult :: Literal
    , _saProverState :: ProverState } |
  ReturnVar
    { _rvResult :: SymVar
    , _saProverState :: ProverState } |
  ReturnUnit |
  Terminate |
  WithKeyset
    { _wkKeySet :: String
    , _saRest :: SymAst
    , _saProverState :: ProverState } |
  WithRead
      { _wrTableName :: String
      , _wrKey :: String
      , _saRest :: SymAst
      , _saProverState :: ProverState } |
  TableInsert
    { _tiTableName :: String
    , _saRest :: SymAst
    , _saProverState :: ProverState } |
  TableUpdate
    { _tiTableName :: String
    , _saRest :: SymAst
    , _saProverState :: ProverState } |
  CannotAnalyze
    { _elWhy :: String
    , _saProverState :: ProverState }
  deriving (Show, Eq)

instance ToJSON SymAst where
  toJSON IfBranch{..} =
    object [ "node" .= ("if" :: String)
           , "state" .= _saProverState
           , "cond" .= (SmtShow.showSL _ifbrCond)
           , "true" .= _ifbrTrue
           , "false" .= _ifbrFalse
           ]
  toJSON EnforceConstraint{..} =
    object [ "node" .= ("enforce" :: String)
           , "state" .= _saProverState
           , "failsIf" .= _ecFailsIf
           , "rest" .= _saRest
           ]
  toJSON ErrorLeaf{..} =
    object [ "node" .= ("error" :: String)
           , "state" .= _saProverState
           , "why" .= _elWhy
           ]
  toJSON ReturnLit{..} =
    object [ "node" .= ("return-literal" :: String)
           , "state" .= _saProverState
           , "returned_literal" .= _rlResult
           ]
  toJSON ReturnVar{..} =
    object [ "node" .= ("return-variable" :: String)
           , "state" .= _saProverState
           , "returned_variable" .= _rvResult
           ]
  toJSON ReturnUnit =
    object [ "node" .= ("return-unit" :: String)
           ]
  toJSON Terminate =
    object [ "node" .= ("terminate" :: String)
           ]
  toJSON WithKeyset{..} =
    object [ "node" .= ("with-keyset" :: String)
           , "state" .= _saProverState
           , "required_keyset" .= _wkKeySet
           , "rest" .= _saRest
           ]
  toJSON WithRead{..} =
    object [ "node" .= ("with-read" :: String)
           , "state" .= _saProverState
           , "table" .= _wrTableName
           , "lookup_key" .= _wrKey
           , "rest" .= _saRest
           ]
  toJSON TableInsert{..} =
    object [ "node" .= ("Insert" :: String)
           , "state" .= _saProverState
           , "table" .= _tiTableName
           , "rest" .= _saRest
           ]
  toJSON TableUpdate{..} =
    object [ "node" .= ("Insert" :: String)
           , "state" .= _saProverState
           , "table" .= _tiTableName
           , "rest" .= _saRest
           ]
  toJSON CannotAnalyze{..} =
    object [ "node" .= ("CannotAnalyze" :: String)
           , "state" .= _saProverState
           , "why" .= _elWhy
           ]

ppSymAst :: SymAst -> IO ()
ppSymAst = BS8.putStrLn . Yaml.encode

type PactAnalysis a = ReaderT ProverState IO a

data LitVal =
    LVDecimal Decimal
  | LVBool Bool
  | LVInteger Integer


isCmpOperator :: String -> Bool
isCmpOperator s = Set.member s $ Set.fromList [">", "<", ">=", "<=", "="]

isLogicalOperator :: String -> Bool
isLogicalOperator s = Set.member s $ Set.fromList ["=", "and", "or", "not"]

isNumericalOperator :: String -> Bool
isNumericalOperator s = Set.member s $ Set.fromList ["+", "-", "*", "/"]

isBasicOperator :: String -> Bool
isBasicOperator s = isCmpOperator s || isLogicalOperator s || isNumericalOperator s

basicOperatorToQualId :: String -> Either String QualIdentifier
basicOperatorToQualId o
  | o == ">" = Right $ QIdentifier $ ISymbol ">"
  | o == ">=" = Right $ QIdentifier $ ISymbol ">="
  | o == "<" = Right $ QIdentifier $ ISymbol "<"
  | o == "<=" = Right $ QIdentifier $ ISymbol "<="
  | o == "=" = Right $ QIdentifier $ ISymbol "="
  | o == "and" = Right $ QIdentifier $ ISymbol "and"
  | o == "or" = Right $ QIdentifier $ ISymbol "or"
  | o == "not" = Right $ QIdentifier $ ISymbol "not"
  | o == "+" = Right $ QIdentifier $ ISymbol "+"
  | o == "-" = Right $ QIdentifier $ ISymbol "-"
  | o == "*" = Right $ QIdentifier $ ISymbol "*"
  | o == "/" = Right $ QIdentifier $ ISymbol "/"
  | otherwise = Left $ "Operator " ++ o ++ " is not yet supported!"

isAppView :: AST (TcId, Type) -> (Bool, AST (TcId, Type))
isAppView app@(App _ _ _) = (True, app)
isAppView _ = (False, undefined)

isInsertOrUpdate :: Fun (TcId,Type) -> (Bool, String)
isInsertOrUpdate (NativeFunc "insert") = (True, "insert")
isInsertOrUpdate (NativeFunc "update") = (True, "update")
isInsertOrUpdate err = (False, error $ "isInsertOrUpdate view pattern failure, wanted insert or update string, got: " ++ show err)

pattern Obj_Key_Val key' val' <- (Lit _ _ (LVLit (LString key')), val')

pattern NativeFunc f <- (FNative _ f _ _)
pattern NativeFuncSpecial f bdy <- (FNative _ f _ (Just (_,bdy)))

pattern AST_Lit lit <- (Lit _ _ (LVLit lit))
pattern AST_Var var <- (Var _ var)
pattern AST_KeySetName keyset' <- (Lit _ _ (LVLit (LString keyset')))
pattern AST_Obj objName kvs <- (Object objName kvs _)

pattern Args_Var var <- [AST_Var var]
pattern Args_Lit lit <- [AST_Lit lit]
pattern Args_Var_Var var1 var2 <- [(AST_Var var1),(Var _ var2)]
pattern Args_Var_Lit var lit <- [(AST_Var var),AST_Lit lit]
pattern Args_Lit_Var lit var <- [AST_Lit lit,(AST_Var var)]
pattern Args_App_App app1 app2 <- [(isAppView -> (True, app1)),(isAppView -> (True, app2))]
pattern Args_App_Lit_Var app' lit var <- [(isAppView -> (True,app')),AST_Lit lit,(AST_Var var)]
pattern Args_App_Lit app' lit' <- [(isAppView -> (True,app')),AST_Lit lit']
pattern Args_App_Lit_Lit app' lit1 lit2 <- [(isAppView -> (True,app')),AST_Lit lit1,AST_Lit lit2]

pattern NativeFunc_Lit_Var f lit' var' <- (App _ (NativeFunc f) (Args_Lit_Var lit' var'))
pattern NativeFunc_Var_Lit f var' lit' <- (App _ (NativeFunc f) (Args_Var_Lit var' lit'))
pattern NativeFunc_Var_Var f var1 var2 <- (App _ (NativeFunc f) (Args_Var_Var var1 var2))
pattern NativeFunc_App_App f app1 app2 <- (App _ (NativeFunc f) (Args_App_App app1 app2))
pattern IF_App_Lit_Lit app' lit1 lit2 <- (App _ (NativeFunc "if") (Args_App_Lit_Lit app' lit1 lit2))
pattern ENFORCE_App_msg app' msg' <- (App _ (NativeFunc "enforce") (Args_App_Lit app' (LString msg')))
pattern BINDING bindings' bdy' <- (Binding _ bindings' bdy' _)
pattern WITHKEYSET keyset' bdy' <- (App _ (NativeFuncSpecial "with-keyset" bdy') [AST_KeySetName keyset'])
pattern INSERT_or_UPDATE fnName' table' key' objName' kvs' <- (App _ (isInsertOrUpdate -> (True, fnName')) [AST_Lit (LString table'), key', AST_Obj objName' kvs'])
pattern WITHREAD table' key' bindings' bdy' <- (App _ (NativeFuncSpecial "with-read" [BINDING bindings' bdy']) [AST_Lit (LString table'), key'])
-- Unsupported currently
pattern READ <- (App _ (NativeFunc "read") _)

varToTerm :: TcId -> PactAnalysis (Either String Smt.Term)
varToTerm n = do
  sVar <- Map.lookup n <$> view psVars
  return $ case sVar of
    Nothing -> Left $ "Variable " ++ show n ++ "not found!"
    Just SymVar{..} -> case _svTracked of
      Tracked -> Right $ TermQualIdentifier $ QIdentifier $ ISymbol $ unSymName _svName
      err -> Left $ "Variable found but tracking has failed: " ++ show err

literalToTerm :: Literal -> Either String Smt.Term
literalToTerm (LBool v) = Right $ TermQualIdentifier $ QIdentifier $ ISymbol (if v then "true" else "false")
literalToTerm (LString v) = Right $ TermSpecConstant $ SpecConstantString $ show v
literalToTerm (LInteger v) = Right $ TermSpecConstant $ SpecConstantNumeral v
literalToTerm (LDecimal v) = Right $ TermSpecConstant $ SpecConstantDecimal $ show v
literalToTerm (LTime _) = Left $ "Time base proving is currently unsupported"

mkPureEquationTerm :: AST (TcId, Type) -> PactAnalysis (Either String Smt.Term)
mkPureEquationTerm (NativeFunc_Var_Lit f v@(name', _) l)
  | isBasicOperator f = do
      varAsTerm <- varToTerm name'
      litAsTerm <- return $ literalToTerm l
      op <- return $ basicOperatorToQualId f
      case (op, varAsTerm, litAsTerm) of
        (Right op', Right v', Right l') -> return $ Right $ TermQualIdentifierT op' [v', l']
        err -> return $ Left $ "unable to analyze: " ++ show err
  | otherwise = return $ Left $ "Function " ++ show f ++ " is unsupported"
mkPureEquationTerm (NativeFunc_Lit_Var f l v@(name', _))
  | isBasicOperator f = do
      varAsTerm <- varToTerm name'
      litAsTerm <- return $ literalToTerm l
      op <- return $ basicOperatorToQualId f
      case (op, litAsTerm, varAsTerm) of
        (Right op', Right l', Right v') -> return $ Right $ TermQualIdentifierT op' [l',v']
        err -> return $ Left $ "unable to analyze: " ++ show err
  | otherwise = return $ Left $ "Function " ++ show f ++ " is unsupported"
mkPureEquationTerm (NativeFunc_Var_Var f v1@(name1, _) v2@(name2, _))
  | isBasicOperator f = do
      var1AsTerm <- varToTerm name1
      var2AsTerm <- varToTerm name2
      op <- return $ basicOperatorToQualId f
      case (op, var1AsTerm, var2AsTerm) of
        (Right op', Right v1, Right v2) -> return $ Right $ TermQualIdentifierT op' [v1, v2]
        err -> return $ Left $ "unable to analyze: " ++ show err
  | otherwise = return $ Left $ "Function " ++ show f ++ " is unsupported"
mkPureEquationTerm (NativeFunc_App_App f app1 app2)
  | isLogicalOperator f = do
      app1' <- mkPureEquationTerm app1
      app2' <- mkPureEquationTerm app2
      op <- return $ basicOperatorToQualId f
      case (op, app1', app2') of
        (Right op', Right app1'', Right app2'') -> return $ Right $ TermQualIdentifierT op' [app1'', app2'']
        err -> return $ Left $ "unable to analyze: " ++ show err
mkPureEquationTerm (AST_Lit l) = return $ literalToTerm l
mkPureEquationTerm (AST_Var (name',_)) = varToTerm name'
mkPureEquationTerm err = return $ Left $ "Unsupported construct found when constructing pure-equation term:\n" ++ show err

negatePureEquationTerm :: Smt.Term -> Smt.Term
negatePureEquationTerm t = TermQualIdentifierT (QIdentifier (ISymbol "not")) [t]

pureEquationTermToAssertion :: Smt.Term -> Command
pureEquationTermToAssertion t = Assert t

negateAssertion :: Command -> Command
negateAssertion (Assert t) = Assert $ negatePureEquationTerm t

convertName :: TcId -> SymName
convertName = SymName . show

convertType :: Type -> Maybe SymType
convertType TyInteger = Just SymInteger
convertType TyBool = Just SymBool
convertType TyDecimal = Just SymDecimal
convertType TyString = Just SymString
convertType _ = Nothing

symTypeToSortId :: SymType -> Either String Sort
symTypeToSortId SymInteger = Right $ SortId $ ISymbol "Int"
symTypeToSortId SymBool = Right $ SortId $ ISymbol "Bool"
symTypeToSortId SymDecimal = Right $ SortId $ ISymbol "Real"
symTypeToSortId SymString = Right $ SortId $ ISymbol "String"

constructSymVar :: (TcId, Type) -> SymVar
constructSymVar (name', type') = newVar
  where
    convType = convertType type'
    convName = convertName name'
    trackingStatus = case convType of
      Nothing -> Untracked "Unsupported Type"
      Just _ -> Tracked
    newVar = SymVar { _svName = convName, _svType = convType, _svTracked = trackingStatus}

symVarToDeclareConst :: SymVar -> Either String Command
symVarToDeclareConst SymVar{..} = case _svType of
  Nothing -> Left $ "SymVar Type is unsupported"
  Just t' -> symTypeToSortId t' >>= return . DeclareFun (unSymName _svName) []

analyzeFunction :: Fun (TcId, Type) -> IO SymAst
analyzeFunction (FDefun _ name' _ args' bdy') = do
  initialState <- return $ ProverState (Map.fromList $ (\x -> (fst x, constructSymVar x)) <$> args') []
  runReaderT (analyze bdy') initialState

analyze :: [AST (TcId, Type)] -> PactAnalysis SymAst
analyze [] = return $ Terminate
analyze ((IF_App_Lit_Lit app' lit1 lit2):rest) = do
  initialState <- ask
  branchPoint <- mkPureEquationTerm app'
  case branchPoint of
    Left err -> error $ err
    Right smtTerm -> do
      trueAssert <- return $ pureEquationTermToAssertion smtTerm
      falseAssert <- return $ negateAssertion trueAssert
      return $ IfBranch
        { _ifbrCond = smtTerm
        , _ifbrTrue = ReturnLit { _rlResult = lit1
                                         , _saProverState = appendSmtCmds [trueAssert] initialState}
        , _ifbrFalse = ReturnLit { _rlResult = lit2
                                         , _saProverState = appendSmtCmds [falseAssert] initialState}
        , _saProverState = initialState
        }
analyze ((ENFORCE_App_msg app' msg'):rest) = do
  initialState <- ask
  branchPoint <- mkPureEquationTerm app'
  case branchPoint of
    Left err -> error $ "from enfoce: " ++ err
    Right smtTerm -> do
      trueAssert <- return $ pureEquationTermToAssertion smtTerm
      if null rest
        then return $ EnforceConstraint
          { _ecFailsIf = msg'
          , _saProverState = appendSmtCmds [trueAssert] initialState
          , _saRest = ReturnUnit
          }
        else do
          rest' <- local (appendSmtCmds [trueAssert]) $ analyze rest
          return $ EnforceConstraint
            { _ecFailsIf = msg'
            , _saProverState = appendSmtCmds [trueAssert] initialState
            , _saRest = rest'
            }
analyze ((BINDING bindings' ast'):rest) = do
  newState <- bindNewVars bindings'
  local (const newState) $ analyze ast'
analyze (AST_Lit lit':[]) = do
  s <- ask
  return $ ReturnLit
    { _rlResult = lit'
    , _saProverState = s}
analyze (AST_Lit lit':rest) = analyze rest -- this has no effect do just pass
analyze (AST_Var var':[]) = do
  s <- ask
  psVars' <- view psVars
  case Map.lookup (fst var') psVars' of
    Nothing -> error $ "Variable not found: " ++ show var' ++ " in " ++ show psVars'
    Just sVar -> return $ ReturnVar
      { _rvResult = sVar
      , _saProverState = s}
analyze (AST_Var var':rest) = analyze rest -- this has no effect do just pass
analyze (WITHKEYSET keyset' bdy:rest) = do
  block' <- analyze bdy
  state' <- ask
  return $ WithKeyset
    { _wkKeySet = keyset'
    , _saProverState = state'
    , _saRest = block' }
analyze (INSERT_or_UPDATE fnName table key objName kvs:rest) = do
  objKey' <- return $ case key of
    AST_Var (vName,_) -> unSymName $ convertName vName
    AST_Lit v -> show v
    err -> error $ "insert's lookup key must be a literal or a variable and not: " ++ show err
  mangledObjects <- return $ fmap (mangleObjToVar objName) kvs
  newState <- bindNewVars mangledObjects
  rest' <- local (const newState) $ analyze rest
  return $ TableInsert
    { _tiTableName = table
    , _saRest = rest'
    , _saProverState = newState}
analyze (WITHREAD table' key' bindings' ast':rest) = do
  newState <- bindNewVars bindings'
  local (const newState) $ do
    st' <- ask
    rest' <- analyze ast'
    return $ WithRead
      { _wrTableName = table'
      , _wrKey = show $ key'
      , _saRest = rest'
      , _saProverState = st' }
analyze (READ:_rest) = error "Objects are not yet supported, which `read` returns. Please use `with-read` instead"
analyze err = error $ "Pattern match failure: " ++ show err

mangleObjToVar :: TcId -> (AST (TcId, Type), AST (TcId, Type)) -> ((TcId,Type), AST (TcId, Type))
mangleObjToVar objName (AST_Lit (LString field), ast@(AST_Var (_,type'))) =
  let tcId' = TcId undefined ("insert-" ++ show objName ++ "-" ++ field) 0
  in ((tcId',type'),ast)

bindNewVars :: [((TcId,Type), AST (TcId, Type))] -> PactAnalysis ProverState
bindNewVars [] = ask
bindNewVars ((varId@(name',_), ast'):rest) = do
  curVars <- view psVars
  if Map.member name' curVars
    then error $ "Duplicate Variable Declared: " ++ show name' ++ " already in " ++ show curVars
    else do
      newSymVar <- return $ constructSymVar varId
      curSt <- ask
      local (psVars %~ (Map.insert name' newSymVar)) $ do
        relation <- constructVarRelation varId ast'
        local (psNodeSMT %~ (++ [relation])) $ bindNewVars rest

constructVarRelation :: (TcId, Type) -> AST (TcId, Type) -> PactAnalysis Command
constructVarRelation (name',_) ast' = do
  varAsTerm <- varToTerm name'
  relation <- mkPureEquationTerm ast'
  case (varAsTerm, relation) of
    (Right v, Right r) -> return $ pureEquationTermToAssertion (TermQualIdentifierT (QIdentifier $ ISymbol "=") [v,r])
    err -> error $ "cannot construct var relation in: " ++ show err ++ " in\n" ++ show ast'

appendSmtCmds :: [Command] -> ProverState -> ProverState
appendSmtCmds cmds ps@ProverState{..} = ps { _psNodeSMT = _psNodeSMT ++ cmds }

-- helper stuff
_parseSmtCmd :: String -> Smt.Command
_parseSmtCmd s = let (Right f) = Parsec.parse SmtParser.parseCommand "" s in f

_getSampFunc :: String -> IO (Fun (TcId, Type))
_getSampFunc s = do
  (Right f, _) <- _infer "examples/analyze-tests/analyze-tests.repl" "analyze-tests" s
  return f

_analyzeSampFunc :: String -> IO ()
_analyzeSampFunc s = do
  a <- _getSampFunc s
  b <- analyzeFunction a
  ppSymAst b

--FDefun
--  { _fInfo = "(defun create-account (id:string initial-balance:integer)"
--  , _fName = "analyze-tests.create-account"
--  , _fType = "(id:string initial-balance:integer) -> <e>"
--  , _fArgs =
--    [ (analyze-tests.create-account_id0,string)
--    , (analyze-tests.create-account_initial-balance1,integer)
--    ]
--  , _fBody = [
--      App
--        { _aId = appNwith-keyset2
--        , _aAppFun = FNative
--          { _fInfo = ""
--          , _fName = "with-keyset"
--          , _fTypes = "(keyset-or-name:string body:@rest) -> <a> :| []"
--          , _fSpecial =
--            Just ( "with-keyset"
-- pattern INSERT table' key' objName' kvs' <- (App _ (NativeFunc "insert") [AST_Lit (LString table'), key', AST_Obj objName' kvs'])
--                 , [ App { _aId = appNinsert4
--                         , _aAppFun = FNative
--                           { _fInfo = ""
--                           , _fName = "insert"
--                           , _fTypes = "(table:string key:string object:object) -> string :| []"
--                           , _fSpecial = Nothing}
--                         , _aAppArgs =
--                           [ Lit { _aId = string5
--                                 , _aLitType = string
--                                 , _aLitValue = LVLit "payments-table"}
--                           , Var { _aId = analyze-tests.create-account_id0
--                                 , _aVar = (analyze-tests.create-account_id0,string)}
--                           , Object { _aId = object6
--                                    , _aObject =
--                                      [
--                                        (Lit { _aId = string7
--                                             , _aLitType = string
--                                             , _aLitValue = LVLit "balance"}
--                                        , Var { _aId = analyze-tests.create-account_initial-balance1
--                                              , _aVar = (analyze-tests.create-account_initial-balance1,integer)})
--                                      ], _aUserType = Nothing}
--                           ]}
--                   ]
--                 )
--        , _aAppArgs =
--          [ Lit { _aId = string3
--                , _aLitType = string
--                , _aLitValue = LVLit "module-keyset"}
--          ]}
--      ]}

--  (defun pay-update (id:string amount:integer)
--    (update 'payments-table id
--            { "balance": amount })
--  )
--FDefun { _fInfo = "(defun pay-update (id:string amount:integer)"
--       , _fName = "analyze-tests.pay-update"
--       , _fType = "(id:string amount:integer) -> <l>"
--       , _fArgs =
--           [ (analyze-tests.pay-update_id0,string)
--           , (analyze-tests.pay-update_amount1,integer)]
--       , _fBody =
--         [ App { _aId = appNupdate2
--               , _aAppFun = FNative { _fInfo = ""
--                                    , _fName = "update"
--                                    , _fTypes = "(table:string key:string object:object) -> string :| []"
--                                    , _fSpecial = Nothing}
--               , _aAppArgs =
--                 [ Lit {_aId = string3, _aLitType = string, _aLitValue = LVLit "payments-table"}
--                 , Var {_aId = analyze-tests.pay-update_id0, _aVar = (analyze-tests.pay-update_id0,string)}
--                 , Object { _aId = object4
--                          , _aObject =
--                            [ ( Lit {_aId = string5, _aLitType = string, _aLitValue = LVLit "balance"}
--                              , Var {_aId = analyze-tests.pay-update_amount1, _aVar = (analyze-tests.pay-update_amount1,integer)})]
--                          , _aUserType = Nothing}
--                 ]}
--         ]}

--FDefun
--  { _fInfo =  "(defun pay (from:string to:string amount:integer)"
--  , _fName = "analyze-tests.pay"
--  , _fType = "(from:string to:string amount:integer) -> <g>"
--  , _fArgs =
--    [ (analyze-tests.pay_from0,string)
--    , (analyze-tests.pay_to1,string)
--    , (analyze-tests.pay_amount2,integer) ]
--  , _fBody =
--    [ App { _aId = appNwith-read3
--          , _aAppFun = FNative { _fInfo = ""
--                               , _fName = "with-read"
--                               , _fTypes = "(table:string key:string bindings:binding) -> string :| []"
--                               , _fSpecial = Just ("with-read"
--                                                  ,[ Binding { _aId = bind5
--                                                             , _aBindings =
--                                                               [ ( (bind5_from-bal6,integer)
--                                                                 , Var { _aId = bind5_from-bal6
--                                                                       , _aVar = (bind5_from-bal6,integer)}
--                                                                 )
--                                                               ]
--                                                             , _aBody =
--                                                               [ App {_aId = appNwith-read7
--                                                                     , _aAppFun =
--                                                                       FNative { _fInfo = ""
--                                                                               , _fName = "with-read"
--                                                                               , _fTypes = "(table:string key:string bindings:binding) -> string :| []"
--                                                                               , _fSpecial = Just
--                                                                                 ( "with-read"
--                                                                                 , [Binding { _aId = bind9
--                                                                                            , _aBindings = [
--                                                                                                ((bind9_to-bal10,integer)
--                                                                                                ,Var { _aId = bind9_to-bal10
--                                                                                                     , _aVar = (bind9_to-bal10,integer)})
--                                                                                                ]
--                                                                                            , _aBody =
--                                                                                                [ App { _aId = appNenforce11
--                                                                                                      , _aAppFun = FNative {_fInfo = ""
--                                                                                                                           , _fName = "enforce"
--                                                                                                                           , _fTypes = "(test:bool msg:string) -> bool :| []"
--                                                                                                                           , _fSpecial = Nothing }
--                                                                                                      , _aAppArgs =
--                                                                                                        [ App { _aId = appN>=12
--                                                                                                              , _aAppFun = FNative { _fInfo = ""
--                                                                                                                                   , _fName = ">="
--                                                                                                                                   , _fTypes = "(x:<a => (integer|decimal|string|time)> y:<a => (integer|decimal|string|time)>) -> bool :| []"
--                                                                                                                                   , _fSpecial = Nothing}
--                                                                                                              , _aAppArgs =
--                                                                                                                [ Var { _aId = bind5_from-bal6
--                                                                                                                      , _aVar = (bind5_from-bal6,integer)}
--                                                                                                                , Var { _aId = analyze-tests.pay_amount2
--                                                                                                                      , _aVar = (analyze-tests.pay_amount2,integer)}
--                                                                                                                ]
--                                                                                                              }
--                                                                                                        , Lit { _aId = string13
--                                                                                                              , _aLitType = string
--                                                                                                              , _aLitValue = LVLit "Insufficient Funds"}
--                                                                                                        ]}
--                                                                                                , App { _aId = appNupdate14
--                                                                                                      , _aAppFun = FNative { _fInfo = ""
--                                                                                                                           , _fName = "update"
--                                                                                                                           , _fTypes = "(table:string key:string object:object) -> string :| []"
--                                                                                                                           , _fSpecial = Nothing}
--                                                                                                      , _aAppArgs =
--                                                                                                        [ Lit { _aId = string15
--                                                                                                              , _aLitType = string
--                                                                                                              , _aLitValue = LVLit "payments-table"}
--                                                                                                        , Var { _aId = analyze-tests.pay_from0
--                                                                                                              , _aVar = (analyze-tests.pay_from0,string)}
--                                                                                                        , Object { _aId = object16
--                                                                                                                 , _aObject =
--                                                                                                                   [ (Lit { _aId = string17
--                                                                                                                          , _aLitType = string
--                                                                                                                          , _aLitValue = LVLit "balance"}
--                                                                                                                     , App { _aId = appN-18
--                                                                                                                           , _aAppFun = FNative { _fInfo = ""
--                                                                                                                                                , _fName = "-"
--                                                                                                                                                , _fTypes = "(x:<a => (integer|decimal)> y:<a => (integer|decimal)>) -> <a => (integer|decimal)> :| [(x:<a => (integer|decimal)> y:<b => (integer|decimal)>) -> decimal,(x:<a => (integer|decimal)>) -> <a => (integer|decimal)>]"
--                                                                                                                                                , _fSpecial = Nothing}
--                                                                                                                           , _aAppArgs =
--                                                                                                                             [ Var {_aId = bind5_from-bal6, _aVar = (bind5_from-bal6,integer)}
--                                                                                                                             , Var {_aId = analyze-tests.pay_amount2
--                                                                                                                                   , _aVar = (analyze-tests.pay_amount2,integer)}
--                                                                                                                             ]
--                                                                                                                           }
--                                                                                                                     )
--                                                                                                                   ]
--                                                                                                                 , _aUserType = Nothing}
--                                                                                                        ]
--                                                                                                      }
--                                                                                                , App { _aId = appNupdate19
--                                                                                                      , _aAppFun = FNative { _fInfo = ""
--                                                                                                                           , _fName = "update"
--                                                                                                                           , _fTypes = "(table:string key:string object:object) -> string :| []"
--                                                                                                                           , _fSpecial = Nothing }
--                                                                                                      , _aAppArgs =
--                                                                                                        [ Lit {_aId = string20, _aLitType = string, _aLitValue = LVLit "payments-table"}
--                                                                                                        , Var {_aId = analyze-tests.pay_to1, _aVar = (analyze-tests.pay_to1,string)}
--                                                                                                        , Object { _aId = object21
--                                                                                                                 , _aObject =
--                                                                                                                   [ ( Lit { _aId = string22, _aLitType = string, _aLitValue = LVLit "balance"}
--                                                                                                                     , App { _aId = appN+23
--                                                                                                                           , _aAppFun = FNative {_fInfo = ""
--                                                                                                                                                , _fName = "+"
--                                                                                                                                                , _fTypes = "(x:<a => (integer|decimal)> y:<a => (integer|decimal)>) -> <a => (integer|decimal)> :| [(x:<a => (integer|decimal)> y:<b => (integer|decimal)>) -> decimal,(x:<a => (string|list|object)> y:<a => (string|list|object)>) -> <a => (string|list|object)>]"
--                                                                                                                                                , _fSpecial = Nothing}
--                                                                                                                           , _aAppArgs =
--                                                                                                                             [ Var { _aId = bind9_to-bal10, _aVar = (bind9_to-bal10,integer)}
--                                                                                                                             , Var { _aId = analyze-tests.pay_amount2
--                                                                                                                                   , _aVar = (analyze-tests.pay_amount2,integer)}
--                                                                                                                             ]
--                                                                                                                           }
--                                                                                                                     )
--                                                                                                                   ]
--                                                                                                                 , _aUserType = Nothing}
--                                                                                                        ]
--                                                                                                      }
--                                                                                                , App { _aId = appNformat24
--                                                                                                      , _aAppFun = FNative { _fInfo = ""
--                                                                                                                           , _fName = "format"
--                                                                                                                           , _fTypes = "(template:string vars:@rest) -> string :| []"
--                                                                                                                           , _fSpecial = Nothing}
--                                                                                                      , _aAppArgs =
--                                                                                                        [ Lit { _aId = string25, _aLitType = string, _aLitValue = LVLit "{} paid {} {}"}
--                                                                                                        , Var { _aId = analyze-tests.pay_from0
--                                                                                                              , _aVar = (analyze-tests.pay_from0,string)}
--                                                                                                        , Var { _aId = analyze-tests.pay_to1
--                                                                                                              , _aVar = (analyze-tests.pay_to1,string)}
--                                                                                                        , Var { _aId = analyze-tests.pay_amount2
--                                                                                                              , _aVar = (analyze-tests.pay_amount2,integer)}
--                                                                                                        ]}
--                                                                                                ]
--                                                                                            , _aBindCtx = BindKV}
--                                                                                   ]
--                                                                                 )
--                                                                               }
--                                                                     , _aAppArgs =
--                                                                       [ Lit { _aId = string8, _aLitType = string, _aLitValue = LVLit "payments-table"}
--                                                                       , Var { _aId = analyze-tests.pay_to1
--                                                                             , _aVar = (analyze-tests.pay_to1,string)}
--                                                                       ]}
--                                                               ]
--                                                             , _aBindCtx = BindKV}
--                                                   ]
--                                                  )
--                               }
--          , _aAppArgs =
--            [ Lit {_aId = string4, _aLitType = string, _aLitValue = LVLit "payments-table"}
--            , Var {_aId = analyze-tests.pay_from0, _aVar = (analyze-tests.pay_from0,string)}
--            ]
--          }
--    ]
--  }

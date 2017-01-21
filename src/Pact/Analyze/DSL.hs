{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Analyze.DSL
  ( analyzeAndRenderTests
  , _docTestPay
  , prettyPrintProveProperty
  ) where

import Pact.Analyze.Types

--import Control.Monad.IO.Class (liftIO)
import Pact.Typecheck
import Data.Either
import Data.Decimal
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import SmtLib.Syntax.Syntax
import qualified SmtLib.Syntax.Syntax as Smt
import qualified SmtLib.Syntax.ShowSL as SmtShow

import Text.Parsec hiding (try)
import qualified Text.Parsec as Parsec

data ProverTest =
  ColumnRange
  { _ptcFunc :: String
  , _ptcValue :: Decimal} |
  ConservesMass
  deriving (Show, Eq)

data ProveProperty = ProveProperty
  { _dtTable :: String
  , _dtColumn :: String
  , _dtTests :: [ProverTest]
  } deriving (Show, Eq)

type Parser a = Parsec String () a

parseAsDecimal :: Parser Decimal
parseAsDecimal = do
  a <- many1 digit
  _ <- char '.'
  b <- many1 digit
  return $ read $ a ++ "." ++ b

parserAsInt :: Parser Decimal
parserAsInt = many1 digit >>= return . read

supportedOpSyms :: Parser String
supportedOpSyms = Parsec.try (string ">=" <|> string "<=" <|> string "==") <|> string ">" <|> string "=" <|> string "<"

parseColRange :: Parser ProverTest
parseColRange = do
  _ <- spaces
  _ <- string "Column"
  _ <- spaces
  fn <- supportedOpSyms
  _ <- spaces
  n <- parserAsInt <|> parseAsDecimal
  return $ ColumnRange fn n

parseConservesMass :: Parser ProverTest
parseConservesMass = spaces >> string "ConservesMass" >> spaces >> return ConservesMass

parseColName :: Parser (String,String)
parseColName = do
  _ <- spaces >> char '\''
  module' <- many1 (noneOf ".")
  _ <- char '.'
  table' <- many1 (noneOf ".")
  _ <- char '.'
  column' <- many1 (noneOf "'")
  _ <- char '\''
  return (module' ++ "." ++ table', column')

parseProveTest :: Parser [ProverTest]
parseProveTest = do
  _ <- spaces >> char '['
  tests <- sepBy1 (Parsec.try parseConservesMass <|> parseColRange) (char ',')
  _ <- spaces >> char ']'
  return tests

parseProperty :: Parser ProveProperty
parseProperty = do
  _ <- string "{-#" >> spaces >> string "PROVE"
  (table', column') <- parseColName
  pts <- parseProveTest
  _ <- spaces >> string "#-}" >> spaces
  return $ ProveProperty { _dtTable = table', _dtColumn = column', _dtTests = pts}

parseDocString :: Parser [ProveProperty]
parseDocString = do
  _ <- manyTill anyChar (lookAhead $ Parsec.try $ string "{-#")
  pp <- many1 parseProperty
  return pp

getTestsFromDoc :: TopLevel Node -> Either String [ProveProperty]
getTestsFromDoc (TopFun (FDefun{..})) = case _fDocs of
  Nothing -> Left "No doc string string found!"
  Just docs -> either (Left . show) Right $ parse parseDocString "" docs
getTestsFromDoc _ = Left $ "Not given a top-level defun"

-- | Go through declared variables and gather a list of those read from a given table's column and those written to it
getColumnsSymVars :: ProveProperty -> Map Node SymVar -> ([SymName], [SymName])
getColumnsSymVars (ProveProperty table' column' _) m = (reads', writes')
  where
    reads' = _svName <$> (Map.elems $ Map.filter (\(SymVar _ _ _ otc) -> otc == Just (OfTableColumn table' column' TableRead)) m)
    writes' = _svName <$> (Map.elems $ Map.filter (\(SymVar _ _ _ otc) -> otc == Just (OfTableColumn table' column' TableWrite)) m)

renderProverTest :: String -> String -> ([SymName], [SymName]) -> ProverTest -> [Smt.Command]
renderProverTest table' column' (reads', writes') ColumnRange{..} =
    preamble ++ (constructDomainAssertion <$> reads') ++ [constructRangeAssertion] ++ exitCmds
  where
    preamble = [Push 1, Echo $ "\"Verifying Domain and Range Stability for: " ++ table' ++ "." ++ column' ++ "\""]
    constructDomainAssertion (SymName sn) = Assert (TermQualIdentifierT (QIdentifier (ISymbol _ptcFunc))
                                         [TermQualIdentifier (QIdentifier (ISymbol sn))
                                         ,TermSpecConstant (SpecConstantDecimal $ show _ptcValue)])
    constructIndividualRangeAsserts (SymName sn) = TermQualIdentifierT (QIdentifier (ISymbol "not"))
                                                   [TermQualIdentifierT (QIdentifier (ISymbol _ptcFunc))
                                                    [TermQualIdentifier (QIdentifier (ISymbol sn)),TermSpecConstant (SpecConstantDecimal $ show _ptcValue)]]
    constructRangeAssertion = Assert (TermQualIdentifierT (QIdentifier (ISymbol "or")) (constructIndividualRangeAsserts <$> writes'))
    exitCmds = [Echo "\"Domain/Range relation holds IFF unsat\"", CheckSat, Pop 1]
renderProverTest table' column' (reads', writes') ConservesMass =
    preamble ++ [mkRelation] ++ exitCmds
  where
    preamble = [Push 1, Echo $ "\"Verifying mass conservation for: " ++ table' ++ "." ++ column' ++ "\""]
    symNameToTerm (SymName sn) = TermQualIdentifier (QIdentifier (ISymbol sn))
    mkRelation = Assert (TermQualIdentifierT
                         (QIdentifier (ISymbol "not"))
                         [TermQualIdentifierT (QIdentifier (ISymbol "="))
                          [TermQualIdentifierT (QIdentifier (ISymbol "+")) (symNameToTerm <$> reads')
                          ,TermQualIdentifierT (QIdentifier (ISymbol "+")) (symNameToTerm <$> writes')
                          ]])
    exitCmds = [Echo "\"Mass is conserved IFF unsat\"", CheckSat, Pop 1]

-- | This is gross and needs to be re-thought
getPreTerminationProverState :: SymAst -> Either String ProverState
getPreTerminationProverState IfBranch{..} = Left "Apologies but automatic Testing for programs containing ifs is under development (manually testing of the compiled SMT-LIB2 is required)"
getPreTerminationProverState sa
  | _saRest sa == Terminate = Right $ _saProverState sa
  | otherwise = getPreTerminationProverState $ _saRest sa

dumpModelsOnSat :: Smt.Command
dumpModelsOnSat = SetOption (OptionAttr (AttributeVal ":dump-models" (AttrValueSymbol "true")))

renderAllFromProverState :: ProverState -> [ProveProperty] -> [String]
renderAllFromProverState ps@ProverState{..} pps =
    let declrs = SmtShow.showSL <$> rights (symVarToDeclareConst <$> Map.elems _psVars)
        funcBody = declrs ++ (SmtShow.showSL <$> _psNodeSMT)
        tests = concat $ renderTestsFromState ps <$> pps
    in [SmtShow.showSL $ dumpModelsOnSat] ++ funcBody ++ tests

renderTestsFromState :: ProverState -> ProveProperty -> [String]
renderTestsFromState ProverState{..} dt@ProveProperty{..} =
    let involvedVars = getColumnsSymVars dt _psVars
        tests = SmtShow.showSL <$> (concat $ renderProverTest _dtTable _dtColumn involvedVars <$> _dtTests)
    in if not (null $ fst involvedVars) && not (null $ snd involvedVars)
       then tests
       else [SmtShow.showSL $ (Echo "\"Unable To Construct Tests: No Variables found. This is usually an issue with the column specification -- make sure it is '<module-name>.<table-name>.<column-name>'\"")]

analyzeAndRenderTests :: TopLevel Node -> IO (Either String [String])
analyzeAndRenderTests tf = do
  symAst <- analyzeFunction tf
  return $ case symAst of
    Left err -> Left $ show err
    Right sa -> case getTestsFromDoc tf of
      Left err -> Left err
      Right pps -> case getPreTerminationProverState sa of
        Left err -> Left err
        Right ps -> Right $ renderAllFromProverState ps pps

prettyPrintProveProperty :: SymAst -> [ProveProperty] -> IO ()
prettyPrintProveProperty sa pps = do
  case getPreTerminationProverState sa of
    Left err -> putStrLn err
    Right ps -> putStrLn $ unlines $ renderAllFromProverState ps pps

_docTestPay :: IO ()
_docTestPay = do
  f <- _getSampFunc "pay"
  case getTestsFromDoc f of
    Left err -> putStrLn err
    Right pps -> do
      a <- analyzeFunction f
      case a of
        Left err -> putStrLn $ show err
        Right a' -> prettyPrintProveProperty a' pps

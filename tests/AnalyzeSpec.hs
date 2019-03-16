{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module AnalyzeSpec (spec) where

import           Control.Lens                 (at, findOf, ix, matching, (&),
                                               (.~), (^.), (^..), _Left)
import           Control.Monad                (unless)
import           Control.Monad.Except         (runExceptT)
import           Control.Monad.State.Strict   (runStateT)
import           Data.Either                  (isLeft, isRight)
import           Data.Foldable                (find, for_)
import qualified Data.HashMap.Strict          as HM
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust, isJust)
import           Data.SBV                     (isConcretely)
import           Data.SBV.Internals           (SBV (SBV))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           NeatInterpolation            (text)
import           Prelude                      hiding (read)
import           Test.Hspec                   (Spec, describe,
                                               expectationFailure, it,
                                               pendingWith, runIO, shouldBe,
                                               shouldSatisfy)
import qualified Test.HUnit                   as HUnit

import           Pact.Parse                   (parseExprs)
import           Pact.Repl                    (evalRepl', initReplState)
import           Pact.Repl.Types              (ReplMode (StringEval), rEnv)
import           Pact.Types.Runtime           (Exp, Info, ModuleData,
                                               eeRefStore, rsModules)
import           Pact.Types.Pretty            (Pretty, renderCompactString)
import           Pact.Types.Util              (tShow)

import           Pact.Analyze.Check
import           Pact.Analyze.Eval.Numerical  (banker'sMethodS)
import qualified Pact.Analyze.Model           as Model
import           Pact.Analyze.Parse           (PreProp (..), TableEnv,
                                               expToProp, inferProp)
import           Pact.Analyze.PrenexNormalize (prenexConvert)
import           Pact.Analyze.Types
import           Pact.Analyze.Util

wrap :: Text -> Text -> Text
wrap code model =
  [text|
    (env-keys ["admin"])
    (env-data { "keyset": { "keys": ["admin"], "pred": "=" } })
    (begin-tx)
    (define-keyset 'ks (read-keyset "keyset"))
    (module test 'ks
      @model
        [; (defproperty dec-conserves-mass (t:table c:column) (= (column-delta t c) 0.0))
         ; (defproperty int-conserves-mass (t:table c:column) (= (column-delta t c) 0))
         (defproperty my-column-delta (d:integer) (= (column-delta accounts 'balance) d))
         (defproperty conserves-balance (= (column-delta accounts 'balance) 0))
         (defproperty conserves-balance2 (my-column-delta 0))
         ; this hash the same name as the column, but the column name takes
         ; precedence
         (defproperty balance (> 0 1))
         (defproperty bad-recursive-prop bad-recursive-prop)
         (defproperty bad-recursive-prop2 (d:integer) (bad-recursive-prop2 d))
         $model
        ]
      (defschema account
        "Row type for accounts table."
         balance:integer
       ; data
         )
      (deftable accounts:{account}
        "Main table for test module.")
      $code
      )
    (commit-tx)
  |]

wrapNoTable :: Text -> Text
wrapNoTable code =
  [text|
    (env-keys ["admin"])
    (env-data { "keyset": { "keys": ["admin"], "pred": "=" } })
    (begin-tx)
    (define-keyset 'ks (read-keyset "keyset"))
    (module test 'ks $code)
    (commit-tx)
  |]

data TestFailure
  = TestCheckFailure CheckFailure
  | NoTestModule
  | ReplError String
  | VerificationFailure VerificationFailure
  deriving Show

renderTestFailure :: TestFailure -> IO String
renderTestFailure = \case
  TestCheckFailure cf -> do
    svgInfo <- case falsifyingModel cf of
      Nothing -> pure ""
      Just m -> do
        let fp = "/tmp/execution-graph.dot"
        Model.renderDot fp m
        pure $ "\n\nrendered execution graph to DOT: " ++ fp

    pure $ T.unpack (describeCheckFailure cf) ++ svgInfo
  NoTestModule -> pure "example is missing a module named 'test'"
  ReplError err -> pure $ "ReplError: " ++ err
  VerificationFailure vf -> pure $ "VerificationFailure: " ++ show vf

--
-- TODO: use ExceptT
--

compile :: Text -> IO (Either TestFailure ModuleData)
compile code = do
  replState0 <- initReplState StringEval Nothing
  (eTerm, replState) <- runStateT (evalRepl' $ T.unpack code) replState0
  pure $ case eTerm of
    Left err -> Left $ ReplError err
    Right _t ->
      case replState ^. rEnv . eeRefStore . rsModules . at "test" of
        Nothing         -> Left NoTestModule
        Just moduleData -> Right moduleData

runVerification :: Text -> IO (Maybe TestFailure)
runVerification code = do
  eModuleData <- compile code
  case eModuleData of
    Left tf -> pure $ Just tf
    Right moduleData -> do
      results <- verifyModule (HM.fromList [("test", moduleData)]) moduleData
      case results of
        Left failure -> pure $ Just $ VerificationFailure failure
        Right (ModuleChecks propResults invariantResults _) -> pure $
          case findOf (traverse . traverse) isLeft propResults of
            Just (Left failure) -> Just $ TestCheckFailure failure
            _ -> case findOf (traverse . traverse . traverse) isLeft invariantResults of

              Just (Left failure) -> Just $ TestCheckFailure failure
              Just (Right _)      -> error "impossible: result of isLeft"
              Nothing             -> Nothing

runCheck :: Text -> Check -> IO (Maybe TestFailure)
runCheck code check = do
  eModuleData <- compile code
  case eModuleData of
    Left tf -> pure $ Just tf
    Right moduleData -> do
      result <- runExceptT $ verifyCheck moduleData "test" check
      pure $ case result of
        Left failure    -> Just $ VerificationFailure failure
        Right (Left cf) -> Just $ TestCheckFailure cf
        Right (Right _) -> Nothing

handlePositiveTestResult :: Maybe TestFailure -> IO ()
handlePositiveTestResult = \case
  Nothing -> pure ()
  Just (TestCheckFailure (CheckFailure _ (SmtFailure (SortMismatch msg))))
    -> pendingWith msg
  Just tf -> HUnit.assertFailure =<< renderTestFailure tf

expectVerified :: Text -> Spec
expectVerified = expectVerified' ""

expectVerified' :: Text -> Text -> Spec
expectVerified' model code = do
  res <- runIO $ runVerification $ wrap code model
  it "passes in-code checks" $ handlePositiveTestResult res

expectFalsified :: Text -> Spec
expectFalsified = expectFalsified' ""

expectFalsified' :: Text -> Text -> Spec
expectFalsified' model code = do
  res <- runIO $ runVerification $ wrap code model
  it "passes in-code checks" $ res `shouldSatisfy` isJust

expectPass :: Text -> Check -> Spec
expectPass code check = do
  res <- runIO $ runCheck (wrap code "") check
  it (show check) $ handlePositiveTestResult res

expectFail :: Text -> Check -> Spec
expectFail code check = do
  res <- runIO $ runCheck (wrap code "") check
  it (show check) $ res `shouldSatisfy` isJust

intConserves :: TableName -> ColumnName -> Prop 'TyBool
intConserves (TableName tn) (ColumnName cn)
  = CoreProp $ IntegerComparison Eq 0 $ Inj $
    IntColumnDelta (StrLit tn) (StrLit cn)

decConserves :: TableName -> ColumnName -> Prop 'TyBool
decConserves (TableName tn) (ColumnName cn)
  = CoreProp $ DecimalComparison Eq 0 $ Inj $
    DecColumnDelta (StrLit tn) (StrLit cn)

pattern Success' :: Prop 'TyBool
pattern Success' = PropSpecific Success

pattern Abort' :: Prop 'TyBool
pattern Abort' = PropSpecific Abort

pattern Result' :: Prop t
pattern Result' = PropSpecific Result

spec :: Spec
spec = describe "analyze" $ do
  describe "decimal arithmetic" $ do
    let unlit :: S Decimal -> Decimal
        unlit = fromJust . unliteralS

    it "+"      $ unlit (1.1  +   2.2)  == 3.3
    it "* + +"  $ unlit (1.5  *   1.5)  == 2.25
    it "* + -"  $ unlit (1.5  * (-1.5)) == -2.25
    it "* - +"  $ unlit (-1.5 *   1.5)  == -2.25
    it "* - -"  $ unlit (-1.5 * (-1.5)) == 2.25

    it "negate" $ unlit (negate 1.5)    == -1.5
    it "negate" $ unlit (negate (-1.5)) == 1.5

    it "shifts" $ unlit ( 1.5 * fromInteger 10) == 15
    it "shifts" $ unlit (-1.5 * fromInteger 10) == -15
    it "shifts" $ lShift255D (rShift255D 1.5)   == (1 :: Decimal)

    it "floor" $ floorD @Decimal 0      == 0
    it "floor" $ floorD @Decimal 1.5    == 1
    it "floor" $ floorD @Decimal (-1.5) == -2

  describe "decimal division" $ do
    let unlit = fromJust . unliteralS @Decimal

    it "can be one half" $ unlit (1 / 2) == 0.5

    it "handles the last decimal correctly" $
      unlit (1581138830084.1918464 / 1581138830084)
      ==
      1.000000000000121334316980759948431357013938975877803928214364623522650045615600621337146939720454311443026061056754776474139591383112306668111215913835129748371209820415844429729847990579481732664375546615468582277686924612859136684739968417878803629721864

    let i256 :: Int
        i256 = 256

        i255 :: Int
        i255 = 255

    it "handles the last decimal digit correctly (positive, round up 1)" $
      unlit (15 / (10 ^ i256))
      ==
      2 / 10 ^ i255

    it "handles the last decimal digit correctly (positive, round up 2)" $
      unlit (17 / (10 ^ i256))
      ==
      2 / 10 ^ i255

    it "handles the last decimal digit correctly (positive, round up 3)" $
      unlit ((-17) / (-(10 ^ i256)))
      ==
      2 / 10 ^ i255

    it "handles the last decimal digit correctly (positive, round down 1)" $
      unlit (25 / (10 ^ i256))
      ==
      2 / 10 ^ i255

    it "handles the last decimal digit correctly (positive, round down 2)" $
      unlit (13 / (10 ^ i256))
      ==
      1 / 10 ^ i255

    it "handles the last decimal digit correctly (positive, round down 3)" $
      unlit ((-13) / (-(10 ^ i256)))
      ==
      1 / 10 ^ i255

    it "handles the last decimal digit correctly (negative, round down 1)" $
      unlit ((-15) / (10 ^ i256))
      ==
      -2 / 10 ^ i255

    it "handles the last decimal digit correctly (negative, round down 2)" $
      unlit ((-17) / (10 ^ i256))
      ==
      -2 / 10 ^ i255

    it "handles the last decimal digit correctly (negative, round down 3)" $
      unlit (17 / (-(10 ^ i256)))
      ==
      -2 / 10 ^ i255

    it "handles the last decimal digit correctly (negative, round up 1)" $
      unlit ((-25) / (10 ^ i256))
      ==
      -2 / 10 ^ i255

    it "handles the last decimal digit correctly (negative, round up 2)" $
      unlit ((-13) / (10 ^ i256))
      ==
      -1 / 10 ^ i255

    it "handles the last decimal digit correctly (negative, round up 3)" $
      unlit (13 / (-(10 ^ i256)))
      ==
      -1 / 10 ^ i255

    it "handles division by a negative number correctly" $
      unlit (0 / (-1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
      ==
      0

  describe "big multiplications" $ do
    let unlit = fromJust . unliteralS @Decimal

    let code =
          [text|
            (defun test:bool ()
              (let ((x:decimal (*
                  1.58113883008419202353012810347474735209747288392336210205455502815728238592
                  1.5811388300841925223288550988445549742724468531346309495428665472399639648496680411494875076793255338200521528740886266294574044913261377201695510306266771994578476399139180068539229254218727314228818363809792
                ))
                    (y:decimal 2.500000000000008243836642384325766770967352358818865769190857605784112966024663251834475644384046904403556387019539234091871481832161009920035987673979588455403008477585022252399854826133637675727060845285897044645837341219331160090749369830048441622744791))
                (enforce (= x y))))
          |]
    expectPass code $ Valid Success'

    it "rounds up the last digit when appropriate" $
        unlit (1581138830084.192052937980207914179905763920823196984267407804357031202155885633105058076747359551154153957331638299995511193600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
       * 1581138830084.192220194412674817793768840142081644758814711734437239116786835916965025889939456340088832614965073903903044055232345709136743725518878029119488000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
      ==

      2500000000000007812618040.433562734137684699135011741551181715981891159652849247733486729055338549963875582523604695139936764674737736206382376787827325735603006181405156090936148131370459148823374617217523084670407741689762536216481173675491045653179580074788284514172469055900877

    let code' =
          [text|
            (defun test:bool ()
              (let ((x:decimal (*
                  1581138830084.192052937980207914179905763920823196984267407804357031202155885633105058076747359551154153957331638299995511193600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
                  1581138830084.192220194412674817793768840142081644758814711734437239116786835916965025889939456340088832614965073903903044055232345709136743725518878029119488000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
                ))
                    (y:decimal 2500000000000007812618040.433562734137684699135011741551181715981891159652849247733486729055338549963875582523604695139936764674737736206382376787827325735603006181405156090936148131370459148823374617217523084670407741689762536216481173675491045653179580074788284514172469055900877))
                (enforce (= x y))))
          |]
    expectPass code' $ Valid Success'

  describe "banker's method" $ do
    let unlit = fromJust . unliteralS

    it "rounds (_.5) to the nearest even" $ unlit (banker'sMethodS 1.5)    == 2
    it "rounds (_.5) to the nearest even" $ unlit (banker'sMethodS 2.5)    == 2
    it "rounds (_.5) to the nearest even" $ unlit (banker'sMethodS (-1.5)) == -2
    it "rounds (_.5) to the nearest even" $ unlit (banker'sMethodS (-2.5)) == -2

  describe "result" $ do
    let code =
          [text|
            (defun test:integer (x:integer)
              (* x -1))
          |]
    expectPass code $ Valid $ CoreProp $ IntegerComparison Eq
      (Inj (IntArithOp Mul (-1) (PVar 1 "x")))
      (Inj Result :: Prop 'TyInteger)

  describe "inlining" $ do
    let code =
          [text|
            (defun helper:integer (b:integer)
              (if (< b 10)
                10
                b))

            (defun test:integer (a:integer)
              (helper a))
          |]
    expectPass code $ Valid $ CoreProp $
      IntegerComparison Gte (Inj Result :: Prop 'TyInteger) 10

  describe "success" $ do
    let code =
          [text|
            (defun test:bool (x:integer)
              (if (< x 10) true false))
          |]
    expectPass code $ Valid (Inj Success)
    expectPass code $ Valid $ sNot Abort'

  describe "enforce.trivial" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce false "cannot pass"))
          |]
    expectPass code $ Satisfiable Abort'
    expectPass code $ Valid Abort'

    expectFail code $ Satisfiable (Inj Success)

  describe "enforce.conditional" $ do
    let code =
          [text|
            (defun test:bool (x:integer)
              (if (< x 10)
                (enforce (< x 5) "abort sometimes")
                true))
          |]
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable $ sNot Abort'
    expectPass code $ Satisfiable (Inj Success)

    expectFail code $ Valid Abort'

  describe "enforce.sequence" $ do
    let code =
          [text|
            (defun test:bool (x:integer)
              (enforce (> x 0) "positive")
              (enforce false "impossible")
              (if (< x 10)
                true
                false))
          |]
    expectPass code $ Valid Abort'

  describe "enforce.sequence" $ do
    let code =
          [text|
            (defun test:bool (x:integer)
              (enforce (> x 0) "positive")
              (if (< x 10)
                true
                false))
          |]
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable (Inj Success)

  describe "enforce.sequence" $ do
    let code =
          [text|
            (defun test:bool (x:integer)
              (enforce (> x 0) "positive")
              (if (< x 10)
                true
                false))
          |]
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable (Inj Success)
    expectPass code $ Valid $ (CoreProp $ IntegerComparison Gt (PVar 1 "x") 0) .=>
      Inj Success
    expectPass code $ Valid $ (CoreProp $ IntegerComparison Eq (PVar 1 "x") 5) .=>
      Inj Success .&&
        (CoreProp $ BoolComparison Eq (Inj Result :: Prop 'TyBool) sTrue)

  describe "read-keyset.equality" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce
                (= (read-keyset "ks")
                   (read-keyset (+ "k" "s")))
                "keysets equality failed"))
          |]
    expectPass code $ Valid (Inj Success)

  --
  -- TODO: test use of read-keyset from property once possible
  --

  describe "enforce-keyset.name.static" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-keyset 'ks))
          |]
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable (Inj Success)
    expectPass code $ Valid $ Inj Success .=> Inj (GuardPassed "ks")

    expectFail code $ Valid $ Inj Success .=> Inj (GuardPassed "different-ks")

  describe "enforce-keyset.name.dynamic" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-keyset (+ "k" "s")))
          |]
    expectPass code $ Valid $ sNot (Inj (GuardPassed "ks")) .=> Abort'

  describe "enforce-keyset.value" $ do
    let code =
          [text|
            (defun test:bool (ks:keyset)
              (enforce-keyset ks))
          |]
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable Success'

  describe "enforce-guard" $ do
    let code =
          [text|
            (defun test:bool (ks:keyset)
              (enforce-guard ks))
          |]
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable Success'

  describe "read-decimal" $ do
    let code =
          [text|
            (defun test:decimal ()
              (read-decimal "foo"))
          |]
    expectPass code $ Satisfiable $ CoreProp $ DecimalComparison Eq (Inj Result) 0
    expectPass code $ Satisfiable $ CoreProp $ DecimalComparison Eq (Inj Result) 1

  --
  -- TODO: test use of read-decimal from property once possible
  --

  describe "read-integer" $ do
    let code =
          [text|
            (defun test:integer ()
              (+ (read-integer "key")
                 (read-integer (+ "ke" "y"))))
          |]
    expectPass code $ Satisfiable $ CoreProp $ IntegerComparison Eq (Inj Result) 0
    expectFail code $ Satisfiable $ CoreProp $ IntegerComparison Eq (Inj Result) 1 -- <- FAIL
    expectPass code $ Satisfiable $ CoreProp $ IntegerComparison Eq (Inj Result) 2

  --
  -- TODO: test use of read-integer from property once possible
  --

  describe "enforce-keyset.row-level.at" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer
              ks:keyset)
            (deftable tokens:{token-row})

            (defun test:integer (acct:string)
              (let* ((obj (read tokens acct))
                     (ks  (at 'ks      obj))
                     (bal (at 'balance obj))
                    )
                (enforce-keyset ks)
                bal
                ))
          |]
    expectPass code $ Valid $ Inj Success .=>
      Inj (RowEnforced "tokens" "ks" (PVar 1 "acct"))

  describe "enforce-keyset.row-level.read" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer
              ks:keyset)
            (deftable tokens:{token-row})

            (defun test:integer (acct:string)
              (with-read tokens acct { "ks" := ks, "balance" := bal }
                (enforce-keyset ks)
                bal))
          |]
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable (Inj Success)
    expectPass code $ Valid $ sNot $ Inj $ Exists 1 "row" (EType SStr) $
      Inj $ RowWrite "tokens" (PVar 1 "row")
    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType SStr) $
      CoreProp $ IntegerComparison Eq
        (Inj (RowWriteCount "tokens" (PVar 1 "row"))) 0
    expectPass code $ Valid $ Inj Success .=>
      Inj (Exists 1 "row" (EType SStr) (Inj $ RowRead "tokens" (PVar 1 "row")))
    expectPass code $ Valid $ Inj Success .=>
      Inj (Exists 1 "row" (EType SStr)
        (CoreProp $ IntegerComparison Eq
          (Inj (RowReadCount "tokens" (PVar 1 "row"))) 1))
    expectPass code $ Satisfiable $ Inj $ Exists 1 "row" (EType SStr) $
      Inj $ RowEnforced "tokens" "ks" (PVar 1 "row")
    expectPass code $ Satisfiable $ Inj $ Exists 1 "row" (EType SStr) $
      sNot $ Inj $ RowEnforced "tokens" "ks" (PVar 1 "row")
    expectPass code $ Valid $ Inj Success .=> (Inj $ Forall 1 "row" (EType SStr) $
      Inj (RowRead "tokens" (PVar 1 "row")) .=>
        Inj (RowEnforced "tokens" "ks" (PVar 1 "row")))
    expectPass code $ Valid $ Inj Success .=>
      Inj (RowEnforced "tokens" "ks" (PVar 1 "acct"))

  describe "enforce-keyset.row-level.read.syntax" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer
              ks:keyset)
            (deftable tokens:{token-row})

            (defun test:integer (acct:string)
              @doc   "test"
              @model
                [(property (forall (row:string) (row-enforced "tokens" "ks" row)))]
              (with-read tokens acct { "ks" := ks, "balance" := bal }
                (enforce-keyset ks)
                bal))
          |]

    -- TODO: come up with better tests. Right now this just tests that this
    -- parses correctly.
    expectPass code $ Satisfiable Abort'

  describe "enforce-keyset.row-level.multiple-keysets" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer
              ks1:keyset
              ks2:keyset)
            (deftable tokens:{token-row})

            (defun test:integer (acct:string)
              (with-read tokens acct { "ks1" := ks, "balance" := bal }
                (enforce-keyset ks)
                bal))
          |]
    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType SStr) $
      Inj (RowRead "tokens" (PVar 1 "row")) .=>
        Inj (RowEnforced "tokens" "ks1" (PVar 1 "row"))
    -- Using the other keyset:
    expectFail code $ Valid $ Inj $ Forall 1 "row" (EType SStr) $
      Inj (RowRead "tokens" (PVar 1 "row")) .=>
        Inj (RowEnforced "tokens" "ks2" (PVar 1 "row"))

  describe "enforce-keyset.row-level.write" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer
              ks:keyset)
            (deftable tokens:{token-row})

            (defun test:integer (acct:string)
              (with-read tokens acct { "ks" := ks, "balance" := bal }
                (let ((new-bal (+ bal 1)))
                  (update tokens acct {"balance": new-bal})
                  (enforce-keyset ks)
                  new-bal)))
          |]
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable $ Inj Success
    expectPass code $ Valid $ Inj Success .=>
      Inj (Exists 1 "row" (EType SStr)
        (Inj (RowWrite "tokens" (PVar 1 "row"))))
    expectPass code $ Valid $ Inj Success .=>
      Inj (Exists 1 "row" (EType SStr)
        (CoreProp $ IntegerComparison Eq
          (Inj (RowWriteCount "tokens" (PVar 1 "row"))) 1))
    expectPass code $ Valid $ Inj Success .=>
      Inj (Exists 1 "row" (EType SStr)
        (Inj (RowRead "tokens" (PVar 1 "row"))))
    expectPass code $ Valid $ Inj Success .=>
      Inj (Exists 1 "row" (EType SStr)
        (CoreProp $ IntegerComparison Eq
          (Inj (RowReadCount "tokens" (PVar 1 "row"))) 1))
    expectPass code $ Valid $ Inj Success .=>
      Inj (Exists 1 "row" (EType SStr)
        (Inj (RowEnforced "tokens" "ks" (PVar 1 "row"))))
    expectPass code $ Satisfiable $ Inj $ Exists 1 "row" (EType SStr) $
      sNot $ Inj $ RowEnforced "tokens" "ks" (PVar 1 "row")
    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType SStr) $
      Inj (RowRead "tokens" (PVar 1 "row")) .=>
        Inj (RowEnforced "tokens" "ks" (PVar 1 "row"))
    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType SStr) $
      Inj (RowWrite "tokens" (PVar 1 "row")) .=>
        Inj (RowEnforced "tokens" "ks" (PVar 1 "row"))
    expectPass code $ Valid $ Inj (RowWrite "tokens" (PVar 1 "acct"))
                          .=> Inj (RowEnforced "tokens" "ks" (PVar 1 "acct"))

  describe "enforce-keyset.row-level.write-count" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (write tokens 'joel { 'balance: 10 })
              (write tokens 'joel { 'balance: 100 }))
          |]
    expectPass code $ Valid $
      CoreProp $ IntegerComparison Eq
        (Inj (RowWriteCount "tokens" (Lit' "joel"))) 2
    expectPass code $ Valid $ PNot $
      CoreProp $ IntegerComparison Eq
        (Inj (RowWriteCount "tokens" (Lit' "joel"))) 1
    expectPass code $ Valid $ PNot $
      CoreProp $ IntegerComparison Eq
        (Inj (RowWriteCount "tokens" (Lit' "joel"))) 3
    expectPass code $ Valid $
      CoreProp $ IntegerComparison Eq
        (Inj (RowReadCount "tokens" (Lit' "joel"))) 0

  describe "enforce-keyset.row-level.write.invalidation" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer
              ks:keyset)
            (deftable tokens:{token-row})

            (defun test:bool (acct:string user-controlled:keyset)
              ;; Overwrite existing keyset:
              (update tokens acct {"ks": user-controlled})
              ;; Then standard row-level keyset enforcement occurs:
              (with-read tokens acct { "ks" := ks, "balance" := bal }
                (let ((new-bal (+ bal 1)))
                  (update tokens acct {"balance": new-bal})
                  (enforce-keyset ks)
                  new-bal)))
          |]
    -- When a user overwrites an existing keyset and then enforces *that* new
    -- keyset, we don't consider the row to have been enforced due to
    -- invalidation:
    --
    expectFail code $ Valid $ Inj $ Forall 1 "row" (EType SStr) $
      Inj (RowRead "tokens" (PVar 1 "row")) .=>
        Inj (RowEnforced "tokens" "ks" (PVar 1 "row"))
    expectFail code $ Valid $ Inj $ Forall 1 "row" (EType SStr) $
      Inj (RowWrite "tokens" (PVar 1 "row")) .=>
        Inj (RowEnforced "tokens" "ks" (PVar 1 "row"))

  describe "enforce-guard.row-level" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer
              g:guard)
            (deftable tokens:{token-row})

            (defun test:integer (acct:string)
              @model
                [(property (forall (row:string)
                   (when (row-read tokens row)
                     (row-enforced tokens "g" row))))]
              (with-read tokens acct { "g" := g, "balance" := bal }
                (enforce-guard g)
                bal))
          |]

    expectVerified code

  describe "keyset-ref-guard" $ do
    let code =
          [text|
            (defun test:bool ()
              @model [(property (authorized-by "foo"))]
              (enforce-guard (keyset-ref-guard "foo")))
          |]
    expectVerified code

  describe "create-pact-guard" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-guard (create-pact-guard "foo")))
          |]
    -- Depending on whether we're executing in a pact:
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable Success'

  describe "create-user-guard" $ do
    let code =
          [text|
            (defun fail:bool (o:object{account})
              (enforce false ""))

            (defun test:bool ()
              (enforce-guard (create-user-guard {} "fail")))
          |]
    -- We leave user guards completely free for now, until we inline them into
    -- a new construct during typechecking:
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable Success'

  describe "call-by-value semantics for inlining" $ do
    let code =
          [text|
            (defun id:string (s:string)
              s
              s)

            (defun test:string ()
              @model [(property (my-column-delta 1))]
              (id
                (write accounts "bob"
                  {"balance": (+ 1 (at 'balance (read accounts "bob")))})
                ))
          |]
    expectVerified code

  describe "trivial capability which always fails" $ do
    let code =
          [text|
            (defcap CAP (i:integer)
              (enforce false "tx always fails"))

            (defun test:bool ()
              (with-capability (CAP 1)
                true))
          |]
    expectPass code $ Valid Abort'

  describe "trivial capability which always succeeds" $ do
    let code =
          [text|
            (defcap CAP (i:integer)
              true)

            (defun test:bool ()
              (with-capability (CAP 1)
                true))
          |]
    expectPass code $ Valid Success'

  describe "requesting token that was never granted" $ do
    let code =
          [text|
            (defcap CAP (i:integer)
              true)

            (defun test:bool ()
              (require-capability (CAP 100)))
          |]
    expectPass code $ Valid Abort'

  describe "requesting token that was granted" $ do
    let code =
          [text|
            (defcap CAP (i:integer b:bool)
              (enforce-keyset "foo"))

            (defun do-require ()
              (require-capability (CAP 100 false)))

            (defun test:bool ()
              (with-capability (CAP 100 false)
                (do-require)))
          |]
    expectPass code $ Valid $ Inj (GuardPassed "foo") .=> Success'

  describe "requesting different capability fails" $ do
    let code =
          [text|
            (defcap FOO (i:integer)
              true)

            (defcap BAR (b:bool)
              true)

            (defun test:bool ()
              (with-capability (FOO 2)
                (require-capability (BAR false))))
          |]
    expectPass code $ Valid Abort'

  describe "requesting token that was not granted for the same args" $ do
    let code =
          [text|
            (defcap CAP (i:integer)
              true)

            (defun test:bool ()
              (with-capability (CAP 2)
                (require-capability (CAP 1))))
          |]
    expectPass code $ Valid Abort'

  describe "require-capability does not execute the capability" $ do
    let code =
          [text|
            (defcap CAP (k:string)
              ;; Insert can only succeed the *first and only* time it's run
              (insert accounts k {"balance": 0})
              true)

            (defun test:bool ()
              (with-capability (CAP "bob")
                (require-capability (CAP "bob"))))
          |]
    expectPass code $ Valid $
      (PNot (Inj (RowExists "accounts" "bob" Before)) .&& Inj (TableWrite "accounts"))
      .<=>
      Success'

  describe "token caching works for expressions which perform computation" $ do
    let code =
          [text|
            (defcap FOO (i:integer)
              true)

            (defun test:bool ()
              (with-capability (FOO (+ 3 0))
                (require-capability (FOO (+ 2 1)))))
          |]
    expectPass code $ Valid Success'

  describe "compose-capability grants an additional capability" $ do
    let code =
          [text|
            (defcap FOO (i:integer)
              (if (> i 0)
                (compose-capability (BAR false))
                false))

            (defcap BAR (b:bool)
              true)

            (defun test:bool ()
              (with-capability (FOO 2)
                (require-capability (FOO 2))
                (require-capability (BAR false))))
          |]
    expectPass code $ Valid Success'

  describe "compose-capability can fail" $ do
    let code =
          [text|
            (defcap FOO (i:integer)
              (compose-capability (BAR false)))

            (defcap BAR (b:bool)
              (enforce b "enforce failed"))

            (defun test:bool ()
              (with-capability (FOO 2)
                (require-capability (BAR false))))
          |]
    expectPass code $ Valid Abort'

  describe "compose-capability only grants for specific arguments" $ do
    let code =
          [text|
            (defcap FOO (i:integer)
              (compose-capability (BAR i)))

            (defcap BAR (i:integer)
              true)

            (defun test:bool ()
              (with-capability (FOO 2)
                (require-capability (BAR 3))))
          |]
    expectPass code $ Valid Abort'

  describe "capabilities are not granted until the body of with-capability" $ do
    let code =
          [text|
            (defcap BAR ()
              true)

            (defcap FOO ()
              (compose-capability (BAR))
              ; BAR is not yet granted here:
              (require-capability (BAR)))

            (defun test:bool ()
              (with-capability (FOO)
                ; FOO and BAR are granted here.
                true))
          |]
    expectPass code $ Valid Abort'

  describe "compose-capability calls produce return values" $ do
    let code =
          [text|
            (defcap BAR ()
              false)
            (defcap FOO ()
              (enforce (compose-capability (BAR)) "BAR returned false"))

            (defun test:bool ()
              (with-capability (FOO)
                true))
          |]

    expectPass code $ Valid Abort'

  describe "enforce-one.1" $ do
    let code =
          [text|
        (defun test:bool (systime:time timeout:time)
          (enforce-one
            "Cancel can only be effected by creditor, or debitor after timeout"
            [(enforce-keyset 'ck)
             (and (enforce (>= systime timeout) "Timeout expired")
                  (enforce-keyset 'dk))]))
          |]

    let systime = PVar 1 "systime"
        timeout = PVar 2 "timeout"
    expectPass code $ Valid $ Inj Success .=>
      POr
        (Inj (GuardPassed "ck"))
        (PAnd
          (Inj (TimeComparison Gte systime timeout))
          (Inj (GuardPassed "dk")))

  describe "enforce-one.2" $ do
    let code =
          [text|
        (defun test:bool ()
          (enforce-one ""
            [(enforce false)
             (enforce true)]))
          |]

    expectPass code $ Valid Success'
    expectPass code $ Valid Result'

  describe "enforce-one.3" $ do
    let code =
          [text|
        (defun test:bool ()
          (enforce-one ""
            [(enforce false)
             (enforce false)]))
          |]

    expectPass code $ Valid $ PNot Success'

  describe "enforce-one.4" $ do
    let code =
          [text|
        (defun test:bool ()
          (enforce-one ""
            [(enforce true)
             (and (enforce true)
                  (enforce false))]))
          |]

    expectPass code $ Valid Success'
    expectPass code $ Valid Result'

  -- This one is a little subtle. Even though the `or` would evaluate to
  -- `true`, one of its `enforce`s threw, so it fails.
  describe "enforce-one.5" $ do
    let code =
          [text|
        (defun test:bool ()
          (enforce-one ""
            [(or (enforce false)
                 (enforce true))]))
          |]

    expectPass code $ Valid $ PNot Success'

  -- This one is also subtle. `or` short-circuits so the second `enforce` never
  -- throws.
  describe "enforce-one.6" $ do
    let code =
          [text|
        (defun test:bool ()
          (enforce-one ""
            [(or (enforce true)
                 (enforce false))]))
          |]

    expectPass code $ Valid Success'
    expectPass code $ Valid Result'

  describe "enforce-one.7" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-one ""
                [(enforce false)
                 (enforce-one "" ; nested enforce-one
                   [(enforce false)
                    (enforce true)
                    (enforce false)])
                 (enforce false)]))
          |]

    expectPass code $ Valid Success'
    expectPass code $ Valid Result'

  describe "enforce-one.8" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-one ""
                [(enforce false) false (enforce false)]))
          |]

    expectPass code $ Valid Success'
    expectPass code $ Valid $ PNot Result'

  describe "enforce-one.9" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-one "" []))
          |]

    expectPass code $ Valid $ sNot Success'

  describe "enforce-one.10" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-one "" [true])
              (enforce-one "" [false]))
          |]

    expectPass code $ Valid Success'

  describe "enforce-one.single-case-regression" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-one "regression" [true]))
          |]
    expectPass code $ Satisfiable sTrue
    expectFail code $ Satisfiable Abort'
    expectFail code $ Valid Abort'
    expectPass code $ Satisfiable Success'
    expectPass code $ Valid Success'

  describe "pact-id" $ do
    let code =
          [text|
            (defun test:integer ()
              (pact-id))
          |]

    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable Success'
    expectPass code $ Satisfiable $ CoreProp $ IntegerComparison Eq
      (Inj Result :: Prop 'TyInteger)
      10

  describe "logical short-circuiting" $ do
    describe "and" $ do
      let code =
            [text|
          (defun test:bool (x: bool)
            (and x (enforce false)))
            |]

      expectPass code $ Valid $ PVar 1 "x" .=> PNot (Inj Success)
      expectPass code $ Valid $ PNot (PVar 1 "x") .=> Inj Success

    describe "or" $ do
      let code =
            [text|
          (defun test:bool (x: bool)
            (or x (enforce false)))
            |]

      expectPass code $ Valid $ PVar 1 "x" .=> Inj Success
      expectPass code $ Valid $ PNot (PVar 1 "x") .=> PNot (Inj Success)

  describe "table-read.multiple-read" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer)
            (deftable tokens:{token-row})

            (defun test:bool ()
              (insert tokens "stu" {"balance": 5, "name": "stu"})
              (let ((stu-name    (at 'name    (read tokens "stu")))
                    (stu-balance (at 'balance (read tokens "stu"))))
                (enforce (= stu-name "stu") "name is stu")
                (enforce (= stu-balance 5) "balance is 5")))
          |]
    expectPass code $ Valid $
      PNot (PropSpecific (RowExists "tokens" "stu" Before))
      .=> Success'
    expectPass code $ Valid $
      Success'
      .=>
      PropSpecific (RowExists "tokens" "stu" After)

  describe "table-read.one-read" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer)
            (deftable tokens:{token-row})

            (defun test:bool ()
              (insert tokens "stu" {"balance": 5, "name": "stu"})
              (let ((stu (read tokens "stu")))
                (enforce (= (at 'name stu) "stu") "name is stu")
                (enforce (= (at 'balance stu) 5) "balance is 5")
                )
              )
          |]
    expectPass code $ Valid $
       PNot (PropSpecific (RowExists "tokens" "stu" Before))
       .=> sNot Abort'
    expectPass code $ Valid $
       Success'
       .=>
       PropSpecific (RowExists "tokens" "stu" After)

  describe "at.dynamic-key" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer)

            (defun test:object{token-row} ()
              (let* ((stu:object{token-row} {"balance": 5, "name": "stu"})
                     (k-start "bal")
                     (k-end "ance")
                     (val:integer (at (+ k-start k-end) stu)))
                (enforce (= val 5) "balance is 5")
                stu
                )
              )
          |]
    expectPass code $ Valid $ Inj Success

    let schema = mkSObject $
          SCons' (SSymbol @"name") SStr $
            SCons' (SSymbol @"balance") SInteger $
              SNil'
    expectPass code $ Valid $ CoreProp $ StrComparison Eq
      (PObjAt schema (Lit' "name") (Inj Result))
      (Lit' "stu" :: Prop 'TyStr)

  describe "at.object-in-object" $
    let code =
          [text|
            (defschema inner   name:string)
            (defschema wrapper wrapped:object{inner})

            (defun test:object{inner} ()
              (let ((obj:object{wrapper} {"wrapped": {"name": "pact"}}))
                (at "wrapped" obj)))
          |]
    in expectPass code $ Valid $ sNot Abort'

  describe "object-equality" $
    let code =
          [text|
            (defun test:bool (a:object{account})
              @doc ""
              @model [(property (and result (= a a)))]
              (= a a))
          |]

    in expectVerified code

  describe "object-inequality" $
    let code =
          [text|
            (defun test:bool ()
              (let ((acct1:object{account} {"balance": 10})
                    (acct2:object{account} {"balance": 5}))
                (and (!= acct1 acct2)
                     (!= acct2 acct1))))
          |]

    in expectPass code $ Valid (Inj Result :: Prop 'TyBool)

  describe "merging-objects" $
    let code =
          [text|
            (defschema person name:string age:integer)

            (defun test:object{person} ()
              @doc   ""
              @model [(property (= {"name": "brian", "age": 100} result))]

              ; merge is left-biased
              (+ {"age": 100} {"age": 1, "name": "brian"}))
          |]
    in expectVerified code

  describe "table-read" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:integer ()
              (with-read tokens "stu" {"balance" := bal}
                bal))
          |]
    expectPass code $ Valid $ Inj $ TableRead "tokens"
    expectPass code $ Valid $ sNot $ Inj $ TableRead "other"

  describe "table-written.insert" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (insert tokens "stu" {"balance": 5}))
          |]
    expectPass code $ Valid $ Inj (TableWrite "tokens") .=> Success'
    expectPass code $ Valid $ sNot $ Inj $ TableWrite "other"
    expectPass code $ Valid $
      Success' .=> PNot (Inj (RowExists "tokens" "stu" Before))
    expectPass code $ Valid $
      Success' .=> Inj (RowExists "tokens" "stu" After)

  describe "table-written.insert.partial" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (insert tokens "stu" {}))
          |]
    expectFail code $ Satisfiable (Inj Success)

  describe "table-written.update" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (update tokens "stu" {"balance": 5}))
          |]
    expectPass code $ Valid $ Success' .=> Inj (TableWrite "tokens")

  describe "table-written.update.partial" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (update tokens "stu" {}))
          |]
    expectPass code $ Valid $
      Success' .=> PropSpecific (RowExists "tokens" "stu" Before)

  describe "table-written.write" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (write tokens "stu" {"balance": 5}))
          |]
    expectPass code $ Valid $ Inj $ TableWrite "tokens"

  describe "table-written.write.partial" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (write tokens "stu" {}))
          |]
    expectFail code $ Satisfiable (Inj Success)

  describe "table-written.conditional" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string (x:bool)
              (if x
                (insert tokens "stu" {"balance": 5})
                "didn't write"))
          |]
    expectPass code $ Satisfiable $ Inj $ TableWrite "tokens"
    expectPass code $ Satisfiable $ sNot $ Inj $ TableWrite "tokens"
    expectPass code $ Valid $ sNot $ Inj $ TableWrite "other"

  describe "table-written.conditional" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string (x:bool)
              ;; returns bool:
              (enforce x "x must be true")
              ;; returns string:
              (if x
                "didn't write"
                (insert tokens "stu" {"balance": 5})))
          |]
    expectPass code $ Valid $
      Inj Success .=> sNot (Inj (TableWrite "tokens"))

  describe "conserves-mass.integer" $ do
    let code =
          [text|
            (defun test:string (from:string to:string amount:integer)
              "Transfer money between accounts"
              (let ((from-bal (at 'balance (read accounts from)))
                    (to-bal   (at 'balance (read accounts to))))
                (enforce (> amount 0)         "Non-positive amount")
                (enforce (>= from-bal amount) "Insufficient Funds")
                (enforce (!= from to)         "Sender is the recipient")
                (update accounts from { "balance": (- from-bal amount) })
                (update accounts to   { "balance": (+ to-bal amount) })))
          |]

    expectPass code $ Valid $
      Inj Success .=> intConserves "accounts" "balance"

  describe "conserves-mass.integer.insert" $ do
    let code =
          [text|
            (defun test:string ()
              "create a new account with 0 balance"
              (insert accounts "stu" { "balance": 0 }))
          |]

    expectPass code $ Valid $
      Inj Success .=> intConserves "accounts" "balance"

  describe "conserves-mass.integer.without-uniqueness" $ do
    let code =
          [text|
            (defun test:string (from:string to:string amount:integer)
              "Transfer money between accounts"
              (let ((from-bal (at 'balance (read accounts from)))
                    (to-bal   (at 'balance (read accounts to))))
                (enforce (> amount 0)         "Non-positive amount")
                (enforce (>= from-bal amount) "Insufficient Funds")
                ;; << NOTE: no (!= from to) here. >>
                (update accounts from { "balance": (- from-bal amount) })
                (update accounts to   { "balance": (+ to-bal amount) })))
          |]

    expectPass code $ Satisfiable $ Inj Success .&& intConserves "accounts" "balance"
    expectPass code $ Satisfiable $ Inj Success .&& sNot (intConserves "accounts" "balance")

  describe "conserves-mass.decimal" $ do
    let code =
          [text|
            (defschema account2
              @doc   "accounts schema"
              @model [(invariant (>= balance 0.0))]
              balance:decimal)
            (deftable accounts2:{account2})

            (defun test:string (from:string to:string amount:decimal)
              (let ((from-bal (at 'balance (read accounts2 from)))
                    (to-bal   (at 'balance (read accounts2 to))))
                (enforce (> amount 0.0)       "Non-positive amount")
                (enforce (>= from-bal amount) "Insufficient Funds")
                (enforce (!= from to)         "Sender is the recipient")
                (update accounts2 from { "balance": (- from-bal amount) })
                (update accounts2 to   { "balance": (+ to-bal amount) })))
          |]

    expectVerified code
    expectPass code $ Valid $ Inj Success .=> decConserves "accounts2" "balance"

  describe "conserves-mass.decimal.insert" $ do
    let code =
          [text|
            (defschema account2
              @doc   "accounts schema"
              @model [(invariant (>= balance 0.0))]
              balance:decimal)
            (deftable accounts2:{account2})

            (defun test:string ()
              "create a new account with 0 balance"
              (insert accounts2 "stu" { "balance": 0.0 }))
          |]

    expectPass code $ Valid $
      Inj Success .=> decConserves "accounts2" "balance"

  describe "conserves-mass.decimal.failing-invariant" $ do
    let code =
          [text|
            (defschema account
              @doc   "accounts schema"
              @model [(invariant (>= balance 0.0))]
              balance:decimal)
            (deftable accounts:{account})

            (defun test:string (from:string to:string amount:decimal)
              (let ((from-bal (at 'balance (read accounts from)))
                    (to-bal   (at 'balance (read accounts to))))
                ; (enforce (> amount 0.0)       "Non-positive amount")
                (enforce (>= from-bal amount) "Insufficient Funds")
                (enforce (!= from to)         "Sender is the recipient")
                (update accounts from { "balance": (- from-bal amount) })
                (update accounts to   { "balance": (+ to-bal amount) })))
          |]

    eModuleData <- runIO $ compile $ wrapNoTable code
    case eModuleData of
      Left err -> it "failed to compile" $ expectationFailure (show err)
      Right moduleData -> do
        results <- runIO $
          verifyModule (HM.fromList [("test", moduleData)]) moduleData
        case results of
          Left failure -> it "unexpectedly failed verification" $
            expectationFailure $ show failure
          Right (ModuleChecks propResults invariantResults _) -> do
            it "should have no prop results" $
              propResults `shouldBe` HM.singleton "test" []

            case invariantResults ^.. ix "test" . ix "accounts" . ix 0 . _Left of
              -- see https://github.com/Z3Prover/z3/issues/1819
              [CheckFailure _ (SmtFailure (SortMismatch msg))] ->
                it "...nevermind..." $ pendingWith msg
              [CheckFailure _ (SmtFailure (Invalid model))] -> do
                let (Model args ModelTags{_mtWrites} ksProvs _) = model

                it "should have a negative amount" $
                  case find (\(Located _ (Unmunged nm, _)) -> nm == "amount") $ args ^.. traverse of
                    Just (Located _ (_, (_, AVal _prov amount))) ->
                      (SBV amount :: SBV Decimal) `shouldSatisfy` (`isConcretely` (< 0))
                    _ -> fail "Failed pattern match"

                let negativeWrite (UObject m) = case m Map.! "balance" of
                      (_bal, AVal _ sval) -> (SBV sval :: SBV Decimal) `isConcretely` (< 0)
                      _                   -> False

                balanceWrite <- pure $ find negativeWrite
                  $ _mtWrites ^.. traverse . located . accObject

                it "should have a negative write" $
                  balanceWrite `shouldSatisfy` isJust

                it "should have no keyset provenance" $ do
                  ksProvs `shouldBe` Map.empty

              other -> runIO $ HUnit.assertFailure $ show other

  describe "cell-delta.integer" $ do
    let code =
          [text|
            (defun test:string ()
              @model
                [ (property
                    (not (exists (row:string) (= (cell-delta accounts 'balance row) 2)))
                  )
                ]
              (with-read accounts "bob" { "balance" := old-bob }
                (update accounts "bob" { "balance": (+ old-bob 2) })

                ; This overwrites the previous value:
                (update accounts "bob" { "balance": (+ old-bob 3) })
                ))
          |]

    expectVerified code

    expectPass code $ Valid $ sNot $ Inj $ Exists 1 "row" (EType SStr) $
      CoreProp $ IntegerComparison Eq
        (Inj (IntCellDelta "accounts" "balance" (PVar 1 "row"))) 2

    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType SStr) $
      CoreProp (StrComparison Neq (PVar 1 "row" :: Prop 'TyStr) (Lit' "bob"))
        .=>
        CoreProp (IntegerComparison Eq
          (Inj (IntCellDelta "accounts" "balance" (PVar 1 "row"))) 0)

    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType SStr) $
      CoreProp (StrComparison Eq (PVar 1 "row" :: Prop 'TyStr) (Lit' "bob"))
        .=>
        CoreProp (IntegerComparison Eq
          (Inj (IntCellDelta "accounts" "balance" (PVar 1 "row"))) 3)

    expectPass code $ PropertyHolds $ Inj $ Exists 1 "row" (EType SStr) $
      CoreProp (IntegerComparison Eq
        (Inj (IntCellDelta "accounts" "balance" (PVar 1 "row"))) 3)

    expectPass code $ Valid $
      CoreProp (IntegerComparison Eq
        (Inj (IntCellDelta "accounts" "balance" (Lit' "bob"))) 3)

  describe "with-read" $ do
    let code =
          [text|
            (defun test:bool (acct:string)
              (update accounts acct { "balance": 10 })
              (with-read accounts acct { "balance" := bal }
                (enforce (= bal 10) "Read after write failed")))
          |]

    expectPass code $ Valid $
      Inj (RowExists "accounts" (PVar 1 "acct") Before) .=> Success'

  describe "with-read.nested" $ do
    let code =
          [text|
            (defun test:bool (acct:string)
              (update accounts acct { "balance": 0 })
              (with-read accounts acct { "balance" := bal }
                (update accounts acct { "balance": 10 })
                (with-read accounts acct { "balance" := bal }
                  (enforce (= bal 10) "Shadowing failed"))))
          |]

    expectPass code $ Valid $
      Inj (RowExists "accounts" (PVar 1 "acct") Before) .=> Success'

  describe "with-read.overlapping-names" $ do
    let code =
          [text|
            (defschema owner "Pet owner" cats:integer dogs:integer)
            (deftable owners:{owner} "Table of pet owners")

            (defun test:integer ()
              (let ((o "bob")
                    (cats 2)
                    (dogs 3))
                (insert owners o {"dogs": dogs, "cats": cats})
                (with-read owners o { "cats" := num, "dogs" := num }
                  (enforce (= num cats) "First binding wasn't used")
                  num)))
          |]

    expectPass code $ Valid $
      PNot (Inj (RowExists "owners" "bob" Before)) .=> Success'

  describe "bind.from-read" $ do
    let code =
          [text|
            (defun test:integer ()
              (update accounts "bob" { "balance": 10 })
              (let ((obj:object{account} (read accounts "bob")))
                (bind obj { "balance" := bal }
                  (enforce (= bal 10) "Bind failed")
                  bal)))
          |]

    expectPass code $ Valid $
      Inj (RowExists "accounts" "bob" Before) .=> Success'

  describe "bind.from-literal" $ do
    let code =
          [text|
            (defun test:integer ()
              (let ((acct:object{account} { "balance": 10 }))
                (bind acct { "balance" := bal }
                  (enforce (= bal 10) "Bind failed")
                  bal)))
          |]

    expectPass code $ Valid Success'

  describe "let" $ do
    describe "sanity" $ do
      describe "1" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let ((y x)
                        (z (+ 10 x)))
                    (enforce (> z y) "z <= y")
                    true))
              |]
        in expectPass code $ Valid $ sNot Abort'

      describe "2" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let ((y x)
                        (z (+ 10 x)))
                    (enforce (< z y) "z >= y")
                    true))
              |]
        in expectPass code $ Valid Abort'

    describe "let*.sanity" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let* ((x 2)
                         (y (* x 10)))
                   (enforce (= 22 (+ x y)) "x + y != 22")))
              |]
        in expectPass code $ Valid $ sNot Abort'

    describe "nested" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let ((x (let ((y 2)) y))
                        (y (let ((x 3)) x)))
                    (let ((z (let ((w 1)) (+ (+ x y) w))))
                      (enforce (= 6 z) "2 + 3 + 1 != 6"))))
              |]
        in expectPass code $ Valid $ sNot Abort'

  describe "time" $
    let code =
          [text|
            (defun test:bool ()
              (let ((startTime:time (time "2016-07-22T12:00:00Z")))
                (enforce
                  (= (add-time startTime (days 1))
                     (time "2016-07-23T12:00:00Z"))
                  "one day later")
                (enforce
                  (= (add-time startTime (hours 1))
                     (time "2016-07-22T13:00:00Z"))
                  "one hour later")
                (enforce
                  (= (add-time startTime (minutes 1))
                     (time "2016-07-22T12:01:00Z"))
                  "one minute later")

                (enforce
                  (= (add-time (time "2016-07-22T12:00:00Z") (days 1.5))
                     (time "2016-07-24T00:00:00Z"))
                  "1.5 days later")
                (enforce
                  (= (add-time (time "2016-07-22T12:00:00Z") (hours 1.5))
                     (time "2016-07-22T13:30:00Z"))
                  "1.5 hours later")
                (enforce
                  (= (add-time (time "2016-07-22T12:00:00Z") (minutes 1.5))
                     (time "2016-07-22T12:01:30Z"))
                  "1.5 minutes later")
                (enforce
                  (= (add-time (time "2016-07-23T13:30:45Z") 0.001002)
                     (parse-time "%Y-%m-%d %H:%M:%S.%v" "2016-07-23 13:30:45.001002"))
                  "0.001002 seconds later")

                (enforce
                  (= (add-time (time "2016-07-23T13:30:45Z") 0.0010023)
                     (parse-time "%Y-%m-%d %H:%M:%S.%v" "2016-07-23 13:30:45.001002"))
                  "0.0010023 s = 0.001002 s")

                ; Pact rounds tenths of milliseconds using the banker's method
                ; (same as its treatment of decimals). ie it rounds to the
                ; nearest even.
                (enforce
                  (= (add-time (time "2016-07-23T13:30:45Z") 0.0010025)
                     (parse-time "%Y-%m-%d %H:%M:%S.%v" "2016-07-23 13:30:45.001002"))
                  "0.0010025 s = 0.001002 s")
                (enforce
                  (= (add-time (time "2016-07-23T13:30:45Z") 0.0010035)
                     (parse-time "%Y-%m-%d %H:%M:%S.%v" "2016-07-23 13:30:45.001004"))
                  "0.0010035 s = 0.001004 s")
              ))
          |]
    in expectPass code $ Valid $ sNot Abort'

  describe "str-to-int" $ do
    describe "without specified base" $ do
      describe "concrete string" $ do
        describe "valid inputs" $
          let code =
                [text|
                  (defun test:bool ()
                    (enforce (= (str-to-int "5") 5) "")
                    (enforce (= (str-to-int "11111111111111111111111") 11111111111111111111111) "")
                    )
                |]
          in expectPass code $ Valid Success'

        describe "invalid inputs" $ do
          for_ ["", "-123", "abc", "123a", "a123", T.replicate 129 "1"] $ \str ->
            expectFail [text|(defun test:integer () (str-to-int "$str"))|] $
              Valid Success'

      describe "symbolic string" $ do
        let code =
              [text|
                (defun test:integer (s:string)
                  (str-to-int s))
              |]

        expectPass code $ Valid $ Success' .=> CoreProp
          (IntegerComparison Gte (Result' :: Prop 'TyInteger) 0)

        expectPass code $ Valid $
          Success' .&& CoreProp (StrComparison Eq (PVar 1 "s") "123") .=>
            (CoreProp $ IntegerComparison Eq (Result' :: Prop 'TyInteger) 123)

    describe "with specified base" $ do
      describe "concrete string and base" $ do
        describe "valid inputs" $
          let code =
                [text|
                  (defun test:bool ()
                    (enforce (= (str-to-int 10 "5") 5) "")
                    (enforce (= (str-to-int 8 "10") 8) "")
                    )
                |]
          in expectPass code $ Valid Success'

        describe "invalid inputs" $ do
          for_ [(0, "23"), (6, ""), (6, "77")] $ \(base, str) ->
            let baseText = tShow (base :: Int)
            in expectFail [text|(defun test:integer () (str-to-int $baseText "$str"))|] $
                 Valid Success'

      describe "symbolic string and concrete base" $ do
        describe "only base 10 is supported" $ do
          expectPass [text| (defun test:integer (s:string) (str-to-int 10 s)) |] $
            Satisfiable Success'
          expectFail [text| (defun test:integer (s:string) (str-to-int 8 s)) |] $
            Satisfiable Success'

      describe "concrete string and symbolic base" $ do
        expectVerified
          [text|
            (defun test:integer (base:integer)
              @model [(property (when (= result 8) (= base 8)))]
              (str-to-int base "10"))
          |]

      describe "symbolic string and symbolic base" $ do
        describe "unsupported" $ do
          expectFail [text| (defun test:integer (b:integer s:string) (str-to-int b s)) |] $
            Satisfiable Success'

  describe "big round" $
    let code =
          [text|
            (defun test:bool ()
              (let ((x:decimal 5230711750420325051373061834485335325985428731552400523071175042032505137306183448533532598542873155240052307117504203250513730.618344853353259854287315524005230711750420325051373061834485335325985428731552400523071175042032505137306183448533532598542873155240052307117504203250513730618344853353259854287315524005230711750420325051373061834485335325985428731552400523071175042032505)
                    (y:integer 300))
              (enforce (= (round x y) x))))
          |]
    in expectPass code $ Valid $ sNot Abort'

  describe "arith" $
    let code =
          [text|
            (defun test:bool ()
              (let ((xI:integer 1)
                    (yI:integer 2)
                    (xD:decimal 1.5)
                    (yD:decimal 2.5))

                ; int -> int -> int
                (enforce (= (+ xI yI) 3) "")
                (enforce (= (- xI yI) -1) "")
                (enforce (= (* xI yI) 2) "")
                (enforce (= (/ xI yI) 0) "")

                ; dec -> dec -> dec
                ; disabled due to pact typechecking limitations
                (enforce (= (+ xD yD) 4.0) "")
                (enforce (= (- xD yD) -1.0) "")
                (enforce (= (* xD yD) 3.75) "")
                (enforce (= (/ xD yD) 0.6) "")

                ; int -> dec -> dec
                (enforce (= (+ xI yD) 3.5) "")
                (enforce (= (- xI yD) -1.5) "")
                (enforce (= (* xI yD) 2.5) "")
                (enforce (= (/ xI yD) 0.4) "")

                ; dec -> int -> dec
                (enforce (= (+ xD yI) 3.5) "")
                (enforce (= (- xD yI) -0.5) "")
                (enforce (= (* xD yI) 3.0) "")
                (enforce (= (/ xD yI) 0.75) "")

                (enforce (= (mod 3 2) 1) "")
                (enforce (= (mod 4 2) 0) "")
                (enforce (= (mod 2983479238473 2) 1) "")
                (enforce (= (mod 2983479238472 2) 0) "")

                (enforce (= (- 3) -3) "")
                (enforce (= (abs -3) 3) "")
                (enforce (= (abs 0) 0) "")
                (enforce (= (abs 3) 3) "")

                (enforce (= (floor 1.5) 1) "")
                (enforce (= (round 1.5) 2) "")
                (enforce (= (ceiling 1.5) 2) "")

                ; banker's rounding
                (enforce (= (round 1.5) 2) "")
                (enforce (= (round 2.5) 2) "")
                (enforce (= (round 3.5) 4) "")
                (enforce (= (round 4.5) 4) "")

                (enforce (= (round 1.50000000001) 2) "")
                (enforce (= (round 1.49999999999) 1) "")

                (enforce (= (floor 1.6) 1) "")
                (enforce (= (round 1.6) 2) "")
                (enforce (= (ceiling 1.6) 2) "")

                (enforce (= (floor 1.4) 1) "")
                (enforce (= (round 1.4) 1) "")
                (enforce (= (ceiling 1.4) 2) "")

                (enforce (= (floor -1.5) -2) "")
                (enforce (= (round -1.5) -2) "")
                (enforce (= (ceiling -1.5) -1) "")

                ; banker's rounding
                (enforce (= (round -1.5) -2) "")
                (enforce (= (round -2.5) -2) "")
                (enforce (= (round -3.5) -4) "")
                (enforce (= (round -4.5) -4) "")

                (enforce (= (round -1.50000000001) -2) "")
                (enforce (= (round -1.49999999999) -1) "")

                (enforce (= (floor -1.6) -2) "")
                (enforce (= (round -1.6) -2) "")
                (enforce (= (ceiling -1.6) -1) "")

                (enforce (= (floor -1.4) -2) "")
                (enforce (= (round -1.4) -1) "")
                (enforce (= (ceiling -1.4) -1) "")

                (enforce (= (floor 0.0) 0) "")
                (enforce (= (round 0.0) 0) "")
                (enforce (= (ceiling 0.0) 0) "")

                (enforce (= (floor 1.0) 1) "")
                (enforce (= (round 1.0) 1) "")
                (enforce (= (ceiling 1.0) 1) "")

                (enforce (= (floor 1.99999) 1) "")
                (enforce (= (round 1.99999) 2) "")
                (enforce (= (ceiling 1.99999) 2) "")

                (enforce (= (floor 100.15234 2) 100.15) "")
                (enforce (= (round 100.15234 2) 100.15) "")
                (enforce (= (ceiling 100.15234 2) 100.16) "")

                (enforce (= (floor -100.15234 2) -100.16) "")
                (enforce (= (round -100.15234 2) -100.15) "")
                (enforce (= (ceiling -100.15234 2) -100.15) "")

                (enforce (=
                  (* 0.0000000000000000000000000000000000000000000000000000000000000000000000000000001
                     0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001)
                  0.0) "this is one digit from significant")

                (enforce (=
                  (/
                    1581138830084.1918464
                    1581138830084)
                  1.000000000000121334316980759948431357013938975877803928214364623522650045615600621337146939720454311443026061056754776474139591383112306668111215913835129748371209820415844429729847990579481732664375546615468582277686924612859136684739968417878803629721864))
              ))
          |]
    in expectPass code $ Valid $ sNot Abort'

  describe "schema-invariants" $ do
    let code =
          [text|
            (defschema ints-row
              @doc "doc"
              @model
                [ (invariant (> pos 0))
                  (invariant (< neg 0))
                ]
              pos:integer
              neg:integer)
            (deftable ints:{ints-row} "Table of positive and negative integers")

            (defun test:bool ()
              (with-read ints "any index" { "pos" := p, "neg" := n }
                (enforce (> p 0) "is not positive")
                (enforce (< n 0) "is not negative")
                ))
          |]

    expectVerified code
    expectPass code $ Valid $
      PropSpecific (RowExists "ints" "any index" Before) .=> Success'

  describe "schema-invariants.not-equals" $ do
    let code =
          [text|
            (defschema ints-row
              @doc   "doc"
              @model [(invariant (!= nonzero 0))]
              nonzero:integer)
            (deftable ints:{ints-row})

            (defun test:bool ()
              (with-read ints "any index" { "nonzero" := nz }
                (enforce (or (> nz 0) (< nz 0)) "is zero")))
          |]

    expectVerified code
    expectPass code $ Valid $
      PropSpecific (RowExists "ints" "any index" Before)
      .=>
      Success'

  describe "schema-invariants.equals" $ do
    let code =
          [text|
            (defschema ints-row
              @doc   "doc"
              @model [(invariant (= zero 0))]
              zero:integer)
            (deftable ints:{ints-row})

            (defun test:bool ()
              (with-read ints "any index" { "zero" := z }
                (enforce (= z 0) "is not zero")))
          |]

    expectVerified code
    expectPass code $ Valid $
      PropSpecific (RowExists "ints" "any index" Before)
      .=>
      Success'

  describe "format-time / parse-time" $ do
    let code =
          [text|
            (defun test:bool ()
              (let* ((time1in  "2016-09-12")
                     (time1    (parse-time "%F" time1in))
                     (time1out (format-time "%F" time1))
                     (time2in  "2016-07-22T11:26:35Z")
                     (time2    (time time2in))
                     (time2out (format-time "%Y-%m-%dT%H:%M:%SZ" time2)))
                (enforce (= time1in time1out))
                (enforce (= time2in time2out)))

                (enforce
                  (= (format-time "%Y-%m-%dT%H:%M:%S%N" (time "2016-07-23T13:30:45Z"))
                     "2016-07-23T13:30:45+00:00"))
                (enforce
                  (= (format-time "%a, %_d %b %Y %H:%M:%S %Z" (time "2016-07-23T13:30:45Z"))
                     "Sat, 23 Jul 2016 13:30:45 UTC"))
                (enforce
                  (= (format-time "%Y-%m-%d %H:%M:%S.%v" (add-time (time "2016-07-23T13:30:45Z") 0.001002))
                     "2016-07-23 13:30:45.001002"))
                     )
          |]
    expectPass code $ Valid Success'

  describe "format" $ do
    let code =
          [text|
            (defun test:bool (str:string)
              (enforce (= (format "{}-{}" ["a" "z"]) "a-z"))
              (enforce (= (format "{}/{}" [11 26]) "11/26"))
              (enforce (= (format "{} or {}" [true false]) "true or false"))

              (enforce (= (format "{}" [str]) str))
            )
          |]
    expectPass code $ Valid Success'

  describe "hash" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce (=
                (hash "hello")
                "e4cfa39a3d37be31c59609e807970799caa68a19bfaa15135f165085e01d41a65ba1e1b146aeb6bd0092b49eac214c103ccfa3a365954bbbe52f74a2b3620c94"))

              (enforce (=
                (hash (- 2 1))
                "1ced8f5be2db23a6513eba4d819c73806424748a7bc6fa0d792cc1c7d1775a9778e894aa91413f6eb79ad5ae2f871eafcc78797e4c82af6d1cbfb1a294a10d10"))

              (enforce (=
                (hash (or true false))
                "5c07e85b3afb949077f2fa42181bb0498f5945f2086d37df5676ebf424ec137d0c21292c943098e22914cdca350e9140d185ca1b2b2bf0522acfcdde09b395dd"))

              (enforce (=
                (hash (and true false))
                "625ad9c6965af1a145e3c7514065eab913702c615a8fc9f4699767684f9e97e65dd50f715eae7fdbceee39a03cecf29d5f6a7e79e6a802244b65f6f915283491"))


              ; TODO:
              ; (enforce (=
              ;   (hash 3.14)
              ;   "dee8179a1755a745174f334ddc81ade0cf3e2d0bdfd1170cc42c1a1d1d0b16f9bfab86592e9ad31123ce9d470f6aa9388cc2a4f9cda1eb7328ae0a7e26cd450e"))

              ; TODO:
              ; (enforce (=
              ;   (hash { 'foo: 1 })
              ;   "61d3c8775e151b4582ca7f9a885a9b2195d5aa6acc58ddca61a504e9986bb8c06eeb37af722ad848f9009053b6379677bf111e25a680ab41a209c4d56ff1e183"))
            )
          |]
    expectPass code $ Valid Success'

  describe "enforce-keyset.row-level.read" $ do
    let code =
          [text|
            (defschema central-bank-schema
              @doc   "central bank"
              @model
                [ (invariant (= 1000000 (+ reserve circulation)))
                  (invariant (>= reserve 0))
                  (invariant (>= circulation 0))
                ]
              reserve:integer
              circulation:integer)
            (deftable central-bank-table:{central-bank-schema})

            (defun test (amt:integer)
              "Issue some amount of currency"

              (let*
                ((before (read central-bank-table "singleton"))
                 (new-reserve     (- (at 'reserve before)     amt))
                 (new-circulation (+ (at 'circulation before) amt))
                )

                (enforce (> amt 0) "")
                (enforce (>= new-reserve 0) "")

                (update central-bank-table "singleton" {
                  'reserve: new-reserve,
                  'circulation: new-circulation
                })))
          |]

    expectVerified code
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable Success'

  describe "prenex conversion" $ do
    -- These test a somewhat irrelevant implementation detail -- the specific
    -- unique id, which is not ideal, but will do for now.
    let a1       = PVar 1 "a"
        a2       = PVar 2 "a"
        -- b        = PVar 1 "b"
        ty       = EType SStr
        intTy    = EType SInteger
        allA1    = Inj ... Forall 1 "a"
        allA2    = Inj ... Forall 2 "a"
        existsA1 = Inj ... Exists 1 "a"
        existsA2 = Inj ... Exists 2 "a"

    it "lifts all over not (becomes exists)" $
      prenexConvert (PNot (allA1 ty a1))
      `shouldBe`
      existsA1 ty (PNot a1)

    it "lifts exists over not (becomes all)" $
      prenexConvert (PNot (existsA1 ty a1))
      `shouldBe`
      allA1 ty (PNot a1)

    it "lifts all over or" $
      prenexConvert (POr (allA1 ty a1) (allA2 ty a2))
      `shouldBe`
      allA1 ty (allA2 ty (POr a1 a2))

    it "lifts all over and" $
      prenexConvert (PAnd (allA1 ty a1) (allA2 ty a2))
      `shouldBe`
      allA1 ty (allA2 ty (PAnd a1 a2))

    it "lifts exists over or" $
      prenexConvert (POr (existsA1 ty a1) (existsA2 ty a2))
      `shouldBe`
      existsA1 ty (existsA2 ty (POr a1 a2))

    it "lifts exists over and" $
      prenexConvert (PAnd (existsA1 ty a1) (existsA2 ty a2))
      `shouldBe`
      existsA1 ty (existsA2 ty (PAnd a1 a2))

    it "lifts forall string" $
      prenexConvert (PAnd (Lit' True)
        (allA1 intTy (CoreProp $ StrComparison Gte a1 a1)))
      `shouldBe`
      allA1 intTy (PAnd (Lit' True) (CoreProp $ StrComparison Gte a1 a1))

    it "lifts over a list" $
      prenexConvert (CoreProp (ListAt SBool 0
        (CoreProp (LiteralList SBool [ allA1 ty a1 ]))))
      `shouldBe`
      allA1 ty (CoreProp (ListAt SBool 0 (CoreProp (LiteralList SBool [ a1 ]))))

    describe "evaluation by sbv" $ do
      -- SBV assumes formulas are always written in prenex-normal form.
      -- `(exists i. i > 0) /\ not (exists i. i > 0)` is clearly not true.
      -- however, in the absence of prenex normalization, SBV interprets this
      -- as `exists i j. i > 0 /\ not (j > 0)`, which _is_ true.
      --
      -- https://github.com/LeventErkok/sbv/issues/256
      let code' = [text|
            (defun test:bool ()
              @model
                [ (property
                    (and
                      (exists (i:integer) (> i 0))
                      (not (exists (i:integer) (> i 0)))))
                ]
              true)
            |]
      expectFalsified code'

  describe "prop parse / typecheck" $ do
    let parseExprs' :: Text -> Either String [Exp Info]
        parseExprs' t = parseExprs t & traverse . traverse . traverse .~ dummyInfo

    let textToProp'
          :: Map Text VarId
          -> Map VarId EType
          -> TableEnv
          -> SingTy a
          -> Text
          -> Either String (Prop a)
        textToProp' env1 env2 tableEnv ty t = case parseExprs' t of
          Right [exp'] ->
            expToProp tableEnv (VarId (Map.size env1)) env1 env2 HM.empty
              HM.empty ty exp'
          Left err -> Left err
          _        -> Left "Error: unexpected result from parseExprs"

        textToProp :: SingTy a -> Text -> Either String (Prop a)
        textToProp = textToProp'
          (Map.singleton "result" 0)
          (Map.singleton 0 (EType SAny))
          (TableMap mempty)

        inferProp'
          :: Map Text VarId
          -> Map VarId EType
          -> TableEnv
          -> Text
          -> Either String EProp
        inferProp' env1 env2 tableEnv t = case parseExprs' t of
          Right [exp'] ->
            inferProp tableEnv (VarId (Map.size env1)) env1 env2 HM.empty
              HM.empty exp'
          Left err -> Left err
          _        -> Left "Error: unexpected result from parseExprs"

        inferProp'' :: Text -> Either String EProp
        inferProp'' = inferProp'
          (Map.singleton "result" 0)
          (Map.singleton 0 (EType SAny))
          (TableMap mempty)

        textToPropTableEnv :: TableEnv -> SingTy a -> Text -> Either String (Prop a)
        textToPropTableEnv tableEnv = textToProp'
          (Map.singleton "result" 0)
          (Map.singleton 0 (EType SAny))
          tableEnv

        singletonTableEnv :: TableName -> ColumnName -> EType -> TableEnv
        singletonTableEnv a b ty = TableMap $ Map.singleton a $
            ColumnMap $ Map.singleton b ty

    it "infers column-delta" $ do
      let tableEnv = singletonTableEnv "a" "b" (EType SInteger)
      textToPropTableEnv tableEnv SBool "(> (column-delta a 'b) 0)"
        `shouldBe`
        Right (CoreProp $ IntegerComparison Gt (Inj (IntColumnDelta "a" "b")) 0)

      let tableEnv' = singletonTableEnv "a" "b" (EType SDecimal)
      textToPropTableEnv tableEnv' SBool "(> (column-delta a 'b) 0.0)"
        `shouldBe`
        Right (CoreProp $ DecimalComparison Gt (Inj (DecColumnDelta "a" "b")) 0)

      textToPropTableEnv tableEnv' SBool "(> (column-delta a \"b\") 0.0)"
        `shouldBe`
        Right (CoreProp $ DecimalComparison Gt (Inj (DecColumnDelta "a" "b")) 0)

    it "checks +" $ do
      textToProp SStr "(+ \"a\" \"b\")"
        `shouldBe`
        Right (PStrConcat (Lit' "a") (Lit' "b"))

      textToProp SInteger "(+ 0 1)"
        `shouldBe`
        Right (Inj (IntArithOp Add 0 1 :: Numerical Prop 'TyInteger))

      textToProp SDecimal "(+ 0.0 1.0)"
        `shouldBe`
        Right (Inj (DecArithOp Add 0 1 :: Numerical Prop 'TyDecimal))

      textToProp SDecimal "(+ 0 1)"
        `shouldBe`
        Left "in (+ 0 1), unexpected argument types for (+): integer and integer"

    it "infers prop objects" $ do
      -- let pairSchema = Schema $
      --       Map.fromList [("x", EType SInteger), ("y", EType SInteger)]
      let pairSchema = mkSObject $
            SCons' (SSymbol @"x") SInteger $
              SCons' (SSymbol @"y") SInteger $
                SNil'
          litPair = Object $
            SCons (SSymbol @"x") (Column sing 0) $
            SCons (SSymbol @"y") (Column sing 1) $
            SNil
          litPairProp = CoreProp $ LiteralObject pairSchema litPair

          nestedObj = CoreProp $ LiteralObject
            nestedSchema
            (Object $ SCons (SSymbol @"foo")
              (Column sing (CoreProp (LiteralObject sing litPair)))
              SNil)
            -- Map.singleton "foo" (EObject pairSchema litPair)

          nestedSchema =
            mkSObject $ SCons' (SSymbol @"foo") pairSchema SNil'
           -- Schema $
           --  Map.singleton "foo" (EObjectTy pairSchema)

      inferProp'' "{ 'x: 0, 'y: 1 }"
        `shouldBe`
        Right (Some pairSchema litPairProp)

      inferProp'' "(at 'x { 'x: 0, 'y: 1 })"
        `shouldBe`
        Right (Some SInteger (PObjAt pairSchema (Lit' "x") litPairProp))

      inferProp'' "{ 'foo: { 'x: 0, 'y: 1 } }"
        `shouldBe`
        Right (Some nestedSchema nestedObj)

    it "infers forall / exists" $ do
      inferProp'' "(forall (x:string y:string) (= x y))"
        `shouldBe`
        Right
          (Some SBool
            (Inj $ Forall (VarId 1) "x" (EType SStr)
              (Inj $ Forall (VarId 2) "y" (EType SStr)
                (CoreProp $ StrComparison Eq
                  (PVar (VarId 1) "x" :: Prop 'TyStr)
                  (PVar (VarId 2) "y")))))

      let tableEnv = singletonTableEnv "accounts" "balance" $ EType SInteger
      textToPropTableEnv
        tableEnv
        SBool
        "(not (exists (row:string) (= (cell-delta accounts 'balance row) 2)))"
        `shouldBe`
        Right (PNot
          (Inj $ Exists (VarId 1) "row" (EType SStr)
            (CoreProp $ IntegerComparison Eq
              (Inj $ IntCellDelta "accounts" "balance" (PVar (VarId 1) "row"))
              2)))

    it "parses row-enforced / vars" $ do
      let env1 = Map.singleton "from" (VarId 1)
          env2 = Map.singleton (VarId 1) (EType SStr)
          tableEnv = singletonTableEnv "accounts" "ks" $ EType SGuard
      textToProp' env1 env2 tableEnv SBool "(row-enforced accounts 'ks from)"
      `shouldBe`
      Right (Inj $ RowEnforced
        (TableNameLit "accounts")
        (ColumnNameLit "ks")
        (PVar (VarId 1) "from"))

    it "parses column properties" $
      let tableEnv = singletonTableEnv "accounts" "balance" $ EType SInteger
      in textToPropTableEnv tableEnv SBool "(= (column-delta accounts 'balance) 0)"
           `shouldBe`
           Right (CoreProp $ IntegerComparison Eq (Inj $ IntColumnDelta "accounts" "balance") 0)

    it "parses (when (not (authorized-by 'accounts-admin-keyset)) abort)" $
      let tableEnv = singletonTableEnv "accounts" "accounts-admin-keyset" $ EType SGuard
      in textToPropTableEnv tableEnv SBool "(when (not (authorized-by 'accounts-admin-keyset)) abort)"
         `shouldBe`
         Right (PLogical OrOp
           [ PLogical NotOp [
               PLogical NotOp [Inj $ GuardPassed "accounts-admin-keyset"]
             ]
           , Abort'
           ])

    it "handles special identifiers" $ do
      -- convert to prop in the environment holding only bindings for `result`
      let textToProp'' ty = textToProp'
            (Map.singleton "result" 0)
            (Map.singleton 0 (EType ty))
            (TableMap mempty)
            ty

      textToProp   SBool "abort"   `shouldBe` Right Abort'
      textToProp   SBool "success" `shouldBe` Right Success'
      inferProp''  "abort"         `shouldBe` Right (Some SBool Abort')
      inferProp''  "success"       `shouldBe` Right (Some SBool Success')

      textToProp'' SBool "result"  `shouldBe` Right Result'
      textToProp'' SInteger  "result"  `shouldBe` Right Result'
      textToProp'' SStr  "result"  `shouldBe` Right Result'

    it "parses quantified tables" $
      inferProp'' "(forall (table:table) (not (table-written table)))"
        `shouldBe`
        Right
          (Some SBool
            $ Inj $ Forall (VarId 1) "table" QTable
              $ PNot $ Inj $ TableWrite $
                CoreProp $ Var (VarId 1) "table")

    it "parses quantified columns" $ do
      pendingWith "parsing quantified table names"
      inferProp'' [text|
        (forall (table:table)
          (forall (column:(column-of table))
            (column-written table column)))
        |]
        `shouldBe`
        Right
          (Some SBool
            (Inj $ Forall (VarId 1) "table" QTable
              (Inj $ Forall (VarId 2) "column" (QColumnOf "table")
                (Inj (ColumnWritten
                  (CoreProp (Var (VarId 1) "table"))
                  (CoreProp (Var (VarId 2) "column")))))))

  let pretty' :: Pretty a => a -> String
      pretty' = renderCompactString
  describe "pretty (PreProp)" $ do
    it "renders literals how you would expect" $ do
      pretty' (PreIntegerLit 1)            `shouldBe` "1"
      pretty' (PreStringLit "foo")         `shouldBe` "\"foo\""
      pretty' (PreDecimalLit 1) `shouldBe` "1.0"
      -- TODO: test rendering time literals
      -- pretty' (PreTimeLit _) `shouldBe` _
      pretty' (PreBoolLit True)            `shouldBe` "true"
      pretty' (PreBoolLit False)           `shouldBe` "false"

    it "renders quantifiers how you would expect" $ do
      pretty' (PreForall 0 "foo" (EType SBool) (PreVar 0 "foo"))
        `shouldBe`
        "(forall (foo:bool) foo)"
      pretty' (PreExists 0 "bar" (EType SBool) (PreApp "not" [PreVar 0 "bar"]))
        `shouldBe`
        "(exists (bar:bool) (not bar))"

  describe "table quantification" $ do
    let code =
          [text|
            (defschema simple-schema balance:integer)
            (deftable simple-table:{simple-schema})

            (defschema additional-schema name:string)
            (deftable additional-table:{additional-schema})

            (defun test1:integer ()
              @doc   "don't touch a table"
              @model
                [ (property (forall (table:table) (not (table-written table))))
                  (property (forall (table:table) (not (table-read table))))
                ]
              1)

            (defun test2:string ()
              @doc "write a table"
              @model
                [
                  (property (exists (table:table) (table-written table)))
                  (property (forall (table:table) (not (table-read table))))
                ]
              (insert simple-table "joel" { 'balance : 5 }))

            (defun test3:object{simple-schema} ()
              @doc   "read a table"
              @model
                [
                  (property (forall (table:table) (not (table-written table))))
                  (property (exists (table:table) (table-read table)))
                ]
              (read simple-table "joel"))

          |]

    expectVerified code

  describe "column quantification" $ do
    let code =
          [text|
            (defschema simple-schema balance:integer)
            (deftable simple-table:{simple-schema})

            (defun test1:integer ()
              @doc   "don't touch a column"
              @model
                [
                  (property (forall (column:(column-of simple-table))
                    (not (column-written simple-table column))))
                  ; ^- equisatisfiable -v
                  (property (not (exists (column:(column-of simple-table))
                    (column-written simple-table column))))

                  (property (forall (column:(column-of simple-table))
                    (not (column-read simple-table column))))
                  ; ^- equisatisfiable -v
                  (property (not (exists (column:(column-of simple-table))
                    (column-read simple-table column))))
                ]
              1)

            (defun test2:string ()
              @doc "write a column"
              @model
                [
                  (property (exists (column:(column-of simple-table))
                    (column-written simple-table column)))
                  ; ^- equisatisfiable -v
                  (property (not (forall (column:(column-of simple-table))
                    (not (column-written simple-table column)))))

                  (property (forall (column:(column-of simple-table))
                    (not (column-read simple-table column))))
                  ; ^- equisatisfiable -v
                  (property (not (exists (column:(column-of simple-table))
                    (column-read simple-table column))))
                ]
              (insert simple-table "joel" { 'balance : 5 }))

            (defun test3:object{simple-schema} ()
              @doc   "read a column"
              @model
                [
                  (property (forall (column:(column-of simple-table))
                    (not (column-written simple-table column))))
                  ; ^- equisatisfiable -v
                  (property (not (exists (column:(column-of simple-table))
                    (column-written simple-table column))))

                  (property (exists (column:(column-of simple-table))
                    (column-read simple-table column)))
                  ; ^- equisatisfiable -v
                  (property (not (forall (column:(column-of simple-table))
                    (not (column-read simple-table column)))))
                ]
              (read simple-table "joel"))

          |]

    expectVerified code

  describe "Pretty" $
    it "schema looks okay" $ do
      let schema = mkSObject $
            SCons' (SSymbol @"name") SStr $
              SCons' (SSymbol @"balance") SInteger
                SNil'
      pretty' schema `shouldBe` "{balance: integer,name: string}"

  describe "at-properties verify" $ do
    let code = [text|
          (defschema user
            @doc "user info"
            @model
              [
                (invariant (>= (length first) 2))
                (invariant (>= (length last) 2))
                (invariant (=  (length ssn) 9))
                (invariant (>= balance 0))
              ]
            first:string
            last:string
            ssn:string
            balance:integer
            )

          (defun test:object{user} (first:string last:string ssn:string balance:integer)
            @doc "make a user"
            @model
              [ (property (= (at 'first result) first))
                (property (= (at 'last result) last))
                (property (= (at 'ssn result) ssn))
                (property (= (at 'balance result) balance))
              ]
             { 'first:first, 'last:last, 'ssn:ssn, 'balance:balance })
          |]

    expectVerified code

  describe "user-defined properties verify" $ do
    let code = [text|
          (defun test:string (from:string to:string)
            @model
              [ (property (my-column-delta 0))
                (property conserves-balance)
                (property conserves-balance2)
              ]
            (enforce (!= from to) "sender and receive must not be the same")
            (with-read accounts from { "balance" := from-bal }
              (with-read accounts to { "balance" := to-bal }
                (update accounts from { "balance": (- from-bal 1) })
                (update accounts to   { "balance": (+ to-bal 1) }))))
          |]
    expectVerified code

    let code' model = [text|
          (defun test:string (from:string to:string)
            @model $model
            (enforce (!= from to) "sender and receive must not be the same")
            (with-read accounts from { "balance" := from-bal }
              (with-read accounts to { "balance" := to-bal }
                ; (update accounts from { "balance": (- from-bal 1) })
                (update accounts to   { "balance": (+ to-bal 1) }))))
          |]
    expectFalsified $ code' "[(property conserves-balance)]"
    expectFalsified $ code' "[(property conserves-balance2)]"
    expectFalsified $ code' "[(property (my-column-delta 0))]"
    expectVerified  $ code' "[(property (my-column-delta 1))]"

  -- user-defined properties can't be recursive
  describe "user-defined properties can't be recursive" $ do
    let code = [text|
          (defun test:string (from:string to:string)
            @model [(property bad-recursive-prop)]
            "foo")
          |]
    expectFalsified code

    let code' = [text|
          (defun test:string (from:string to:string)
            @model [(property (bad-recursive-prop2 0))]
            "foo")
          |]
    expectFalsified code'

  describe "execution trace" $ do
    let read, write, assert {-auth,-} :: TraceEvent -> Bool
        read   = isRight . matching _TraceRead
        write  = isRight . matching _TraceWrite
        assert = isRight . matching _TraceAssert
        -- auth= isRight . matching _TraceAuth
        path   = isRight . matching _TraceSubpathStart
        push   = isRight . matching _TracePushScope
        pop    = isRight . matching _TracePopScope

        match :: [a -> Bool] -> [a] -> Bool
        tests `match` items
          | length items == length tests
          = and $ uncurry ($) <$> zip tests items
          | otherwise
          = False

        expectTrace :: Text -> Prop 'TyBool -> [TraceEvent -> Bool] -> Spec
        expectTrace code prop tests = do
          res <- runIO $ runCheck (wrap code "") $ Valid prop
          it "produces the correct trace" $
            case res of
              Just (TestCheckFailure (falsifyingModel -> Just model)) -> do
                let trace = _etEvents (Model.linearize model)
                unless (tests `match` trace) $ HUnit.assertFailure $
                  "trace doesn't match:\n\n" ++ show trace
              _ ->
                HUnit.assertFailure "unexpected lack of falsifying model"

    describe "is a linearized trace of events" $ do
      let code =
            [text|
              (defun test:string (from:string to:string amount:integer)
                "Transfer money between accounts"
                (let ((from-bal (at 'balance (read accounts from)))
                      (to-bal   (at 'balance (read accounts to))))
                  (enforce (> amount 0)         "Non-positive amount")
                  (enforce (>= from-bal amount) "Insufficient Funds")
                  ;; NOTE: this is disabled:
                  ; (enforce (!= from to)         "Sender is the recipient")
                  (update accounts from { "balance": (- from-bal amount) })
                  (update accounts to   { "balance": (+ to-bal amount) })))
            |]

      expectTrace code
        (sNot Success')
        [push, read, read, push, assert, assert, write, write, pop, pop]

    describe "doesn't include events excluded by a conditional" $ do
      let code =
            [text|
              (defun test:string ()
                (if false
                  (insert accounts "stu" {"balance": 5}) ; impossible
                  "didn't write"))
            |]
      expectTrace code (Lit' False) [push, {- else -} path, pop]

    describe "doesn't include events after a failed enforce" $ do
      let code =
            [text|
              (defun test:integer ()
                (write accounts "test" {"balance": 5})
                (enforce false)
                (at 'balance (read accounts "test")))
            |]
      expectTrace code Success' [push, write, assert]

    describe "doesn't include cases after a successful enforce-one case" $ do
      let code =
            [text|
              (defun test:bool ()
                (enforce-one ""
                  [(enforce false)
                   true
                   (enforce false)
                  ]))
            |]
      expectTrace code (sNot Success')
        [push, assert, {- failure -} path, {- success -} path, pop]

    it "doesn't include events after the first failure in an enforce-one case" $
      pendingWith "use of resumptionPath"

  describe "references to module constants" $ do
    expectVerified [text|
      (defconst FOO "FOO")

      (defun test:string ()
        @model [(property (= result FOO))]
        FOO)
      |]

  describe "module-scoped properties verify" $ do
    let okay = [text|
          (defun okay:string (from:string to:string)
            (enforce (!= from to) "sender and receive must not be the same")
            (with-read accounts from { "balance" := from-bal }
              (with-read accounts to { "balance" := to-bal }
                (update accounts from { "balance": (- from-bal 1) })
                (update accounts to   { "balance": (+ to-bal 1) }))))
          |]
        bad = [text|
          (defun bad:string ()
            (with-read accounts "joel" { "balance" := bal }
              (update accounts "joel" { "balance": (+ bal 1000000000) })))
          |]
        conservesBalance = "(property conserves-balance)"

    expectVerified'  conservesBalance okay
    expectFalsified' conservesBalance bad
    expectFalsified' conservesBalance $ T.unlines [ bad, okay ]
    expectFalsified' conservesBalance $ T.unlines [ okay, bad ]

    expectVerified' "(property conserves-balance {'except: [bad]})" $
      T.unlines [ okay, bad ]
    expectVerified' "(property conserves-balance {'only:   [good]})" $
      T.unlines [ okay, bad ]

    expectVerified'  "(property conserves-balance {'except: []})" okay
    expectFalsified' "(property conserves-balance {'except: []})" bad

    expectVerified'  "(property conserves-balance {'only:   []    })" bad
    expectFalsified' "(property conserves-balance {'only:   [bad]})" bad

  describe "read (property)" $ do
    let code1 = [text|
          (defun test:object{account} (acct:string)
            @model [(property (= result (read accounts acct 'before)))]
            (read accounts acct))
          |]
    expectVerified code1

    -- reading from a different account
    let code2 = [text|
          (defun test:object{account} (acct:string)
            @model [(property (= result (read accounts acct 'before)))]
            (read accounts 'brian))
          |]
    expectFalsified code2

    let code3 = [text|
          (defun test:string (acct:string)
            @model
              [ (property
                  (= 100
                    (at 'balance (read accounts acct 'after))))
              ]
            (write accounts acct { 'balance: 100 }))
          |]
    expectVerified code3

    -- writing to a different account
    let code4 = [text|
          (defun test:string (acct:string)
            @model
              [ (property
                  (= 100
                    (at 'balance (read accounts acct 'after))))
              ]
            (write accounts acct { 'balance: 0 }))
          |]
    expectFalsified code4

    let code5 = [text|
          (defun test:string (acct:string)
            @model
              [ (property
                  (=
                    (+ (at 'balance (read accounts acct 'before)) 100)
                       (at 'balance (read accounts acct 'after))))
              ]
            (with-read accounts acct { 'balance := bal }
              (write accounts acct { 'balance: (+ 100 bal) })))
          |]
    expectVerified code5

    -- writing to a different account
    let code6 = [text|
          (defun test:string (acct:string)
            @model
              [ (property
                  (=
                    (+ (at 'balance (read accounts acct 'before)) 100)
                       (at 'balance (read accounts acct 'after))))
              ]
            (with-read accounts acct { 'balance := bal }
              (write accounts 'brian { 'balance: (+ 100 bal) })))
          |]
    expectFalsified code6

    let code7 = [text|
          (defun test:string (acct:string)
            @model
              [ (property
                  (=
                    (+ (at 'balance (read accounts acct 'before)) 100)
                       (at 'balance (read accounts acct 'after))))
              ]
            (write accounts acct { 'balance: 0 })
            (with-read accounts acct { 'balance := bal }
              (enforce (> bal 0))
              (write accounts acct { 'balance: 100 })))
          |]
    let acct           = PVar 1 "acct"
        objTy         = -- Schema $ Map.singleton "balance" $ EType SInteger
          mkSObject $
            SCons' (SSymbol @"balance") SInteger SNil'
        readBalance ba = PObjAt objTy "balance"
          (PropSpecific $ PropRead objTy ba "accounts" acct)
        exists ba      = PropSpecific (RowExists "accounts" acct ba)

    expectPass code7 $ Valid $
      Success'
      .=>
      PAnd (exists Before) (exists After)

    expectPass code7 $ Valid $
      Success'
      -- TODO: this arrow should point both ways
      .=>
      Inj (IntegerComparison Eq (readBalance After) 100)

    -- this should hold in general (for any contract)
    expectPass code7 $ Valid $ exists Before .=> exists After

    -- TODO:
    -- this could be generalized to a property that should hold in general
    -- expectPass code7 $ Valid $
    --   Abort'
    --   .=>
    --   Inj (IntegerComparison Eq (readBalance Before) (readBalance After))

  describe "list literals" $ do
    let code0 model = [text|
          (defun test:[integer] (a:integer b:integer c:integer)
            @model $model
            [a b c])
          |]
    expectVerified  $ code0 "[(property (= result [a b c]))]"
    expectFalsified $ code0 "[(property (= result [a b]))]"

  describe "deprecated list literals" $ do
    let code0 = [text|
          (defun test:[integer] (a:integer b:integer c:integer)
            (list a b c))
          |]
    -- we expect this to give an error
    expectFail code0 $ Satisfiable $ Inj Success

  describe "make-list" $ do
    let code = [text|
          (defun test:[integer] (a:integer)
            @model
              [ (property (= (length result) 5))
              , (property
                  (forall (i:integer)
                    (when
                      (and
                        (>= i 0)
                        (<  i 5))
                      (= (at i result) a))))
              , (property (not (!=
                  (take 0 (make-list 2 0))
                  (take 0 (make-list 2 0)))))
              ]
            (make-list 5 a))
          |]
    -- we expect this to give an error
    expectVerified code

  describe "list drop" $ do
    let code1 model = [text|
          (defun test:[integer] (a:integer b:integer c:integer)
            @model $model
            (drop 2 [a b c]))
          |]
    expectVerified  $ code1 "[(property (= result [c]))]"
    expectFalsified $ code1 "[(property (= result [b c]))]"

    let code1' model = [text|
          (defun test:[integer] (a:integer b:integer c:integer)
            @model $model
            (drop -1 [a b c]))
          |]
    expectVerified  $ code1' "[(property (= result [a b]))]"
    expectFalsified $ code1' "[(property (= result [a]))]"

    let code2 = [text|
          (defun test:[integer] (a:integer b:integer c:integer)
            @model [(property (= result []))
                    (property (= (length result) 0))
                   ]
            (drop 4 [a b c]))
          |]
    expectVerified code2

  describe "list take" $ do
    let code3 model = [text|
          (defun test:[integer] (a:integer b:integer c:integer)
            @model $model
            (take 2 [a b c]))
          |]
    expectVerified  $ code3 "[(property (= result [a b]))]"
    expectFalsified $ code3 "[(property (= result [b c]))]"

    let code3' model = [text|
          (defun test:[integer] (a:integer b:integer c:integer)
            @model $model
            (take -2 [a b c]))
          |]
    expectVerified  $ code3' "[(property (= result [b c]))]"
    expectFalsified $ code3' "[(property (= result [a b]))]"

    let code4 = [text|
          (defun test:[integer] (a:integer b:integer c:integer)
            @model [(property (= result [a b c]))
                    (property (= (length result) 3))
                   ]
            (take 4 [a b c]))
          |]
    expectVerified code4

    let code5 = [text|
          (defun test:bool (a:integer b:integer c:integer)
            @model [(property (= result true))]
            (= [] (take 0 [a b c])))
          |]
    expectVerified code5

  describe "list at" $ do
    let code5 = [text|
          (defun test:integer (a:integer b:integer c:integer)
            @model [(property (= result a))]
            (at 0 [a b c]))
          |]
    expectVerified code5

    let code6' = [text|
          (defun test:integer (ix:integer)
            @model [(property (= result ix))]
            (at ix [0 1 2]))
          |]
    expectVerified code6'
    expectPass code6' $ Satisfiable Abort'

    let code6'' = [text|
          (defun test:integer (list:[integer])
            @model [(property (when (> (length list) 2) (= result (at 2 list))))]
            (at 2 list))
          |]
    expectVerified code6''

    let code6''' = [text|
          (defun test:integer (list:[integer])
            @model [(property (= result (at 2 list)))]
            (at 2 list))
          |]
    result <- runIO $ runVerification code6'''
    it "query fails" $ result `shouldSatisfy` isJust

  describe "string contains" $ do
    let code7 = [text|
          (defun test:bool ()
            @model [(property (= result true))]
            (contains "foo" "foobar"))
          |]
    expectVerified code7

  describe "list contains" $ do
    let code8 model = [text|
          (defun test:bool (a:integer b:integer c:integer)
            @model $model
            (contains a [a b c]))
          |]
    expectVerified  $ code8 "[(property (= result true))]"
    expectFalsified $ code8 "[(property (= result false))]"

  describe "list concat" $ do
    let code9 model = [text|
          (defun test:[integer] (a:integer b:integer c:integer)
            @model $model
            (+ [a b] [c]))
          |]
    expectFalsified $ code9 "[(property (= result [a b]))]"
    expectVerified  $ code9 "[(property (= result (+ [a] [b c])))]"

  describe "list reverse" $ do
    let code10 = [text|
          (defun test:[integer] (a:integer b:integer c:integer)
            @model [(property (= result (reverse [a b c])))]
            [c b a])
          |]
    expectVerified code10

  describe "list sort" $ do
    let code11 = [text|
          (defun min:integer (x:integer y:integer)
            (if (< x y) x y))

          (defun test:integer (a:integer b:integer c:integer)
            @model [(property (= result (at 0 (sort [a b c]))))]
            (min a (min b c)))
          |]
    expectVerified code11

  describe "identity" $ do
    let code = [text|
          (defun test:integer ()
            @model []
            (identity 1))
          |]
    expectVerified code

  describe "list map" $ do
    let code1 = [text|
          (defun test:[integer] ()
            @model [(property (= result [1 2 3]))]
            (map (identity) [1 2 3]))
          |]
    expectVerified code1

    let code2 = [text|
          (defun test:[integer] ()
            @model [(property (= result [2 3 4]))]
            (map (+ 1) [1 2 3]))
          |]
    expectVerified code2

    let code3 = [text|
          (defun test:[integer] ()
            @model [(property (= result [1 1 1]))]
            (map (constantly 1) [1 2 3]))
          |]
    expectVerified code3

    describe "constantly" $ do
      let code4 = [text|
            (defun test:[integer] ()
              @model [(property (= result [2 2 2]))]
              (map (compose (constantly 1) (+ 1)) [1 2 3]))
            |]
      expectVerified code4

      it "ignores multiple variables" $ pendingWith "implementation"

    let code5 = [text|
          (defun test:[integer] ()
            @model [(property (= result [1 1 1]))]
            (map (compose (+ 1) (constantly 1)) [1 2 3]))
          |]
    expectVerified code5

  describe "list filter" $ do
    let code = [text|
          (defun test:[integer] ()
            @model [(property (= result [2 3 4]))]
            (filter (> 5) [2 6 3 7 4 8]))
          |]
    expectVerified code

    let code' = [text|
          (defun test:[string] ()
            @model [(property (= result ["dog" "has" "fleas"]))]
            (filter (compose (length) (< 2)) ["my" "dog" "has" "fleas"]))
          |]
    expectVerified code'

  describe "list fold" $ do
    let code = [text|
          (defun test:integer ()
            @model [(property (= result 115))]
            (fold (+) 0 [100 10 5]))
          |]
    expectVerified code

  describe "and?" $ do
    let code = [text|
          (defun test:bool ()
            @model [(property (= result false))]
            (and? (> 20) (> 10) 15))
          |]
    expectVerified code

  describe "or?" $ do
    let code = [text|
          (defun test:bool ()
            @model [(property (= result true))]
            (or? (> 20) (> 10) 15))
          |]
    expectVerified code

--  describe "where" $
--    it "works" $
--      pendingWith "implementation"

  describe "typeof" $ do
    let code = [text|
          (defun test:string ()
            @model [(property (= result "string"))]
            (typeof "foo"))
          |]
    expectVerified code

  -- TODO: pending sbv unicode fix
  -- describe "unicode strings" $
  --   let code = [text|
  --         (defun test:string ()
  --           @model [(property (= result "foo"))]
  --           (format-time "%j%V%l\246806\941262\743680\111256\291942\925764%u%M\413482\881382\334334\465271\899033\560560\47211\75894\501994\417096%w%r\507953\1007703\1111496\1088522\37264\325569\357465\769605\960665\527873\218746\570536\389358\202229\870795%l\476745\322505\570838\545020\965973\1052968\868090\746731\25721\233682\1055404\91996\51906\278110\1104222\629147\1033543\1083315\318365\449369\186461\884415\293431\257589\601947\971770\880368\881194\497135\354885%V%V" (parse-time "" "")))
  --         |]
  --   expectVerified code

  describe "read columns" $ do
    it "read after write" $ do
      pendingWith "partial row typechecking (#360)"
      -- let code =
      --       [text|
      --         (defschema token-row
      --           name:string
      --           balance:integer)
      --         (deftable tokens:{token-row})

      --         (defun test:bool ()
      --           (write tokens 'joel { 'name: 'joel, 'balance: 5 })
      --           (let ((joel (read tokens 'joel ['balance])))
      --             (enforce (= (at 'balance joel) 5) "balance is 5")))
      --       |]

      -- expectPass code $ Valid Success'

    it "only includes the specified columns" $ do
      pendingWith "partial row typechecking (#360)"
      -- We read only the name column from the database then try to read
      -- balance
      -- let code =
      --       [text|
      --         (defschema token-row
      --           name:string
      --           balance:integer)
      --         (deftable tokens:{token-row})

      --         (defun test:integer ()
      --           (let ((joel (read tokens 'joel ['name])))
      --             (at 'balance joel)))
      --       |]

      -- expectPass code $ Valid Abort'

  describe "using a separate function to state properties of multiple function calls" $ do
    describe "associativity of addition" $ do
      let code = [text|
            (defun test:bool (a:integer b:integer c:integer)
              @model [ (property result) ]
              (= (+ a (+ b c)) (+ (+ a b) c)))
            |]
      expectVerified code

    describe "associativity of list concatenation" $ do
      let code = [text|
            (defun test:bool (a:[integer] b:[integer] c:[integer])
              @model [ (property result) ]
              (= (+ a (+ b c)) (+ (+ a b) c)))
            |]
      expectVerified code

    describe "testing monotonicity of a function" $ do
      let code = [text|
            (defun f:integer (x:integer)
              (if (< x 0) (- x 5) (- x 3)))

            (defun test:bool (a:integer b:integer)
              @model [ (property result) ]
              ; a < b => f(a) < f(b)
              (or (< a b)
                  (not (< (f a) (f b)))
                  )
              )
            |]
      expectVerified code

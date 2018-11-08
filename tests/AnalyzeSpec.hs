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
import           Data.Foldable                (find)
import qualified Data.HashMap.Strict          as HM
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust, isJust)
import           Data.SBV                     (Boolean (bnot, true, (&&&), (<=>), (==>)),
                                               isConcretely)
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

import           Pact.Analyze.Check
import           Pact.Analyze.Eval.Numerical  (banker'sMethodS)
import qualified Pact.Analyze.Model           as Model
import           Pact.Analyze.Parse           (PreProp (..), TableEnv,
                                               expToProp, inferProp)
import           Pact.Analyze.PrenexNormalize (prenexConvert)
import           Pact.Analyze.Types
import           Pact.Analyze.Util            (dummyInfo, (...))


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
  replState0 <- initReplState StringEval
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

intConserves :: TableName -> ColumnName -> Prop Bool
intConserves tn cn = CoreProp $ IntegerComparison Eq 0 $ Inj $
  IntColumnDelta (PLit tn) (PLit cn)

decConserves :: TableName -> ColumnName -> Prop Bool
decConserves tn cn = CoreProp $ DecimalComparison Eq 0 $ Inj $
  DecColumnDelta (PLit tn) (PLit cn)

pattern Success' :: Prop Bool
pattern Success' = PropSpecific Success

pattern Abort' :: Prop Bool
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
      (Inj Result :: Prop Integer)

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
      IntegerComparison Gte (Inj Result :: Prop Integer) 10

  describe "success" $ do
    let code =
          [text|
            (defun test:bool (x:integer)
              (if (< x 10) true false))
          |]
    expectPass code $ Valid (Inj Success)
    expectPass code $ Valid $ bnot Abort'

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
    expectPass code $ Satisfiable $ bnot Abort'
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
    expectPass code $ Valid $ (CoreProp $ IntegerComparison Gt (PVar 1 "x") 0) ==>
      Inj Success
    expectPass code $ Valid $ (CoreProp $ IntegerComparison Eq (PVar 1 "x") 5) ==>
      Inj Success &&&
        (CoreProp $ BoolComparison Eq (Inj Result :: Prop Bool) true)

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
    expectPass code $ Valid $ Inj Success ==> Inj (KsNameAuthorized "ks")

    expectFail code $ Valid $ Inj Success ==> Inj (KsNameAuthorized "different-ks")

  describe "enforce-keyset.name.dynamic" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-keyset (+ "k" "s")))
          |]
    expectPass code $ Valid $ bnot (Inj (KsNameAuthorized "ks")) ==> Abort'

  describe "enforce-keyset.value" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-keyset (read-keyset (+ "k" "s"))))
          |]
    expectPass code $ Satisfiable Abort'
    expectPass code $ Satisfiable (Inj Success)
    expectPass code $ Valid $ bnot (Inj (KsNameAuthorized "ks")) ==> Abort'

    expectFail code $ Valid $ bnot (Inj (KsNameAuthorized "different-ks")) ==> Abort'

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
    expectPass code $ Valid $ bnot $ Inj $ Exists 1 "row" (EType TStr) $
      Inj $ RowWrite "tokens" (PVar 1 "row")
    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType TStr) $
      CoreProp $ IntegerComparison Eq
        (Inj (RowWriteCount "tokens" (PVar 1 "row"))) 0
    expectPass code $ Valid $ Inj Success ==>
      Inj (Exists 1 "row" (EType TStr) (Inj $ RowRead "tokens" (PVar 1 "row")))
    expectPass code $ Valid $ Inj Success ==>
      Inj (Exists 1 "row" (EType TStr)
        (CoreProp $ IntegerComparison Eq
          (Inj (RowReadCount "tokens" (PVar 1 "row"))) 1))
    expectPass code $ Satisfiable $ Inj $ Exists 1 "row" (EType TStr) $
      Inj $ RowEnforced "tokens" "ks" (PVar 1 "row")
    expectPass code $ Satisfiable $ Inj $ Exists 1 "row" (EType TStr) $
      bnot $ Inj $ RowEnforced "tokens" "ks" (PVar 1 "row")
    expectPass code $ Valid $ Inj Success ==> (Inj $ Forall 1 "row" (EType TStr) $
      Inj (RowRead "tokens" (PVar 1 "row")) ==>
        Inj (RowEnforced "tokens" "ks" (PVar 1 "row")))
    expectPass code $ Valid $ Inj Success ==>
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
    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType TStr) $
      Inj (RowRead "tokens" (PVar 1 "row")) ==>
        Inj (RowEnforced "tokens" "ks1" (PVar 1 "row"))
    -- Using the other keyset:
    expectFail code $ Valid $ Inj $ Forall 1 "row" (EType TStr) $
      Inj (RowRead "tokens" (PVar 1 "row")) ==>
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
    expectPass code $ Valid $ Inj Success ==>
      Inj (Exists 1 "row" (EType TStr)
        (Inj (RowWrite "tokens" (PVar 1 "row"))))
    expectPass code $ Valid $ Inj Success ==>
      Inj (Exists 1 "row" (EType TStr)
        (CoreProp $ IntegerComparison Eq
          (Inj (RowWriteCount "tokens" (PVar 1 "row"))) 1))
    expectPass code $ Valid $ Inj Success ==>
      Inj (Exists 1 "row" (EType TStr)
        (Inj (RowRead "tokens" (PVar 1 "row"))))
    expectPass code $ Valid $ Inj Success ==>
      Inj (Exists 1 "row" (EType TStr)
        (CoreProp $ IntegerComparison Eq
          (Inj (RowReadCount "tokens" (PVar 1 "row"))) 1))
    expectPass code $ Valid $ Inj Success ==>
      Inj (Exists 1 "row" (EType TStr)
        (Inj (RowEnforced "tokens" "ks" (PVar 1 "row"))))
    expectPass code $ Satisfiable $ Inj $ Exists 1 "row" (EType TStr) $
      bnot $ Inj $ RowEnforced "tokens" "ks" (PVar 1 "row")
    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType TStr) $
      Inj (RowRead "tokens" (PVar 1 "row")) ==>
        Inj (RowEnforced "tokens" "ks" (PVar 1 "row"))
    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType TStr) $
      Inj (RowWrite "tokens" (PVar 1 "row")) ==>
        Inj (RowEnforced "tokens" "ks" (PVar 1 "row"))
    expectPass code $ Valid $ Inj (RowWrite "tokens" (PVar 1 "acct"))
                          ==> Inj (RowEnforced "tokens" "ks" (PVar 1 "acct"))

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
        (Inj (RowWriteCount "tokens" (PLit "joel"))) 2
    expectPass code $ Valid $ PNot $
      CoreProp $ IntegerComparison Eq
        (Inj (RowWriteCount "tokens" (PLit "joel"))) 1
    expectPass code $ Valid $ PNot $
      CoreProp $ IntegerComparison Eq
        (Inj (RowWriteCount "tokens" (PLit "joel"))) 3
    expectPass code $ Valid $
      CoreProp $ IntegerComparison Eq
        (Inj (RowReadCount "tokens" (PLit "joel"))) 0

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
    expectFail code $ Valid $ Inj $ Forall 1 "row" (EType TStr) $
      Inj (RowRead "tokens" (PVar 1 "row")) ==>
        Inj (RowEnforced "tokens" "ks" (PVar 1 "row"))
    expectFail code $ Valid $ Inj $ Forall 1 "row" (EType TStr) $
      Inj (RowWrite "tokens" (PVar 1 "row")) ==>
        Inj (RowEnforced "tokens" "ks" (PVar 1 "row"))

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
    expectPass code $ Valid $ Inj Success ==>
      POr
        (Inj (KsNameAuthorized "ck"))
        (PAnd
          (Inj (TimeComparison Gte systime timeout))
          (Inj (KsNameAuthorized "dk")))

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

    expectPass code $ Valid $ bnot Success'

  describe "logical short-circuiting" $ do
    describe "and" $ do
      let code =
            [text|
          (defun test:bool (x: bool)
            (and x (enforce false)))
            |]

      expectPass code $ Valid $ PVar 1 "x" ==> PNot (Inj Success)
      expectPass code $ Valid $ PNot (PVar 1 "x") ==> Inj Success

    describe "or" $ do
      let code =
            [text|
          (defun test:bool (x: bool)
            (or x (enforce false)))
            |]

      expectPass code $ Valid $ PVar 1 "x" ==> Inj Success
      expectPass code $ Valid $ PNot (PVar 1 "x") ==> PNot (Inj Success)

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
      <=> Success'
    expectPass code $ Valid $
      Success'
      ==>
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
       <=> bnot Abort'
    expectPass code $ Valid $
       Success'
       ==>
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

    let schema = Schema $
          Map.fromList [("name", EType TStr), ("balance", EType TInt)]
        ety    = EType TStr
    expectPass code $ Valid $ CoreProp $ StringComparison Eq
      (PAt schema (PLit "name") (Inj Result) ety)
      (PLit "stu" :: Prop String)

  describe "at.object-in-object" $
    let code =
          [text|
            (defschema inner   name:string)
            (defschema wrapper wrapped:object{inner})

            (defun test:object{inner} ()
              (let ((obj:object{wrapper} {"wrapped": {"name": "pact"}}))
                (at "wrapped" obj)))
          |]
    in expectPass code $ Valid $ bnot Abort'

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

    in expectPass code $ Valid (Inj Result :: Prop Bool)

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
    expectPass code $ Valid $ bnot $ Inj $ TableRead "other"

  describe "table-written.insert" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (insert tokens "stu" {"balance": 5}))
          |]
    expectPass code $ Valid $ Inj (TableWrite "tokens") <=> Success'
    expectPass code $ Valid $ bnot $ Inj $ TableWrite "other"
    expectPass code $ Valid $
      Success' <=> PNot (Inj (RowExists "tokens" "stu" Before))
    expectPass code $ Valid $
      Success' ==> Inj (RowExists "tokens" "stu" After)

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
    expectPass code $ Valid $ Success' <=> Inj (TableWrite "tokens")

  describe "table-written.update.partial" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (update tokens "stu" {}))
          |]
    expectPass code $ Valid $
      Success' <=> PropSpecific (RowExists "tokens" "stu" Before)

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
    expectPass code $ Satisfiable $ bnot $ Inj $ TableWrite "tokens"
    expectPass code $ Valid $ bnot $ Inj $ TableWrite "other"

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
      Inj Success ==> bnot (Inj (TableWrite "tokens"))

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
      Inj Success ==> intConserves "accounts" "balance"

  describe "conserves-mass.integer.insert" $ do
    let code =
          [text|
            (defun test:string ()
              "create a new account with 0 balance"
              (insert accounts "stu" { "balance": 0 }))
          |]

    expectPass code $ Valid $
      Inj Success ==> intConserves "accounts" "balance"

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

    expectPass code $ Satisfiable $ Inj Success &&& intConserves "accounts" "balance"
    expectPass code $ Satisfiable $ Inj Success &&& bnot (intConserves "accounts" "balance")

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
    expectPass code $ Valid $ Inj Success ==> decConserves "accounts2" "balance"

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
      Inj Success ==> decConserves "accounts2" "balance"

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

                let negativeWrite (Object m) = case m Map.! "balance" of
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

    expectPass code $ Valid $ bnot $ Inj $ Exists 1 "row" (EType TStr) $
      CoreProp $ IntegerComparison Eq
        (Inj (IntCellDelta "accounts" "balance" (PVar 1 "row"))) 2

    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType TStr) $
      CoreProp (StringComparison Neq (PVar 1 "row" :: Prop String) (PLit "bob"))
        ==>
        CoreProp (IntegerComparison Eq
          (Inj (IntCellDelta "accounts" "balance" (PVar 1 "row"))) 0)

    expectPass code $ Valid $ Inj $ Forall 1 "row" (EType TStr) $
      CoreProp (StringComparison Eq (PVar 1 "row" :: Prop String) (PLit "bob"))
        ==>
        CoreProp (IntegerComparison Eq
          (Inj (IntCellDelta "accounts" "balance" (PVar 1 "row"))) 3)

    expectPass code $ Valid $ Inj $ Exists 1 "row" (EType TStr) $
      CoreProp (IntegerComparison Eq
        (Inj (IntCellDelta "accounts" "balance" (PVar 1 "row"))) 3)

    expectPass code $ Valid $
      CoreProp (IntegerComparison Eq
        (Inj (IntCellDelta "accounts" "balance" (PLit "bob"))) 3)

  describe "with-read" $ do
    let code =
          [text|
            (defun test:bool (acct:string)
              (update accounts acct { "balance": 10 })
              (with-read accounts acct { "balance" := bal }
                (enforce (= bal 10) "Read after write failed")))
          |]

    expectPass code $ Valid $
      Inj (RowExists "accounts" (PVar 1 "acct") Before) ==> Success'

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
      Inj (RowExists "accounts" (PVar 1 "acct") Before) <=> Success'

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
      PNot (Inj (RowExists "owners" "bob" Before)) <=> Success'

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
      Inj (RowExists "accounts" "bob" Before) <=> Success'

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
        in expectPass code $ Valid $ bnot Abort'

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
        in expectPass code $ Valid $ bnot Abort'

    describe "nested" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let ((x (let ((y 2)) y))
                        (y (let ((x 3)) x)))
                   (let ((z (let ((w 1)) (+ (+ x y) w))))
                     (enforce (= 6 z) "2 + 3 + 1 != 6"))))
              |]
        in expectPass code $ Valid $ bnot Abort'

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
    in expectPass code $ Valid $ bnot Abort'

  describe "big round" $
    let code =
          [text|
            (defun test:bool ()
              (let ((x:decimal 5230711750420325051373061834485335325985428731552400523071175042032505137306183448533532598542873155240052307117504203250513730.618344853353259854287315524005230711750420325051373061834485335325985428731552400523071175042032505137306183448533532598542873155240052307117504203250513730618344853353259854287315524005230711750420325051373061834485335325985428731552400523071175042032505)
                    (y:integer 300))
              (enforce (= (round x y) x))))
          |]
    in expectPass code $ Valid $ bnot Abort'

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
    in expectPass code $ Valid $ bnot Abort'

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
              (with-read ints "any index" { "pos" := pos, "neg" := neg }
                (enforce (> pos 0) "is not positive")
                (enforce (< neg 0) "is not negative")
                ))
          |]

    expectVerified code
    expectPass code $ Valid $
      PropSpecific (RowExists "ints" "any index" Before) <=> Success'

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
      <=>
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
      <=>
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
    let a0       = PVar 1 "a"
        a1       = PVar 2 "a"
        -- b        = PVar 1 "b"
        ty       = EType TStr
        intTy    = EType TInt
        allA0    = Inj ... Forall 1 "a"
        allA1    = Inj ... Forall 2 "a"
        existsA0 = Inj ... Exists 1 "a"
        existsA1 = Inj ... Exists 2 "a"

    it "lifts all over not (becomes exists)" $
      prenexConvert (PNot (allA0 ty a0))
      `shouldBe`
      existsA0 ty (PNot a0)

    it "lifts exists over not (becomes all)" $
      prenexConvert (PNot (existsA0 ty a0))
      `shouldBe`
      allA0 ty (PNot a0)

    it "lifts all over or" $
      prenexConvert (POr (allA0 ty a0) (allA1 ty a1))
      `shouldBe`
      allA0 ty (allA1 ty (POr a0 a1))

    it "lifts all over and" $
      prenexConvert (PAnd (allA0 ty a0) (allA1 ty a1))
      `shouldBe`
      allA0 ty (allA1 ty (PAnd a0 a1))

    it "lifts exists over or" $
      prenexConvert (POr (existsA0 ty a0) (existsA1 ty a1))
      `shouldBe`
      existsA0 ty (existsA1 ty (POr a0 a1))

    it "lifts exists over and" $
      prenexConvert (PAnd (existsA0 ty a0) (existsA1 ty a1))
      `shouldBe`
      existsA0 ty (existsA1 ty (PAnd a0 a1))

    it "lifts forall string" $
      prenexConvert (PAnd (PLit True)
        (allA0 intTy (CoreProp $ StringComparison Gte a0 a0)))
      `shouldBe`
      allA0 intTy (PAnd (PLit True) (CoreProp $ StringComparison Gte a0 a0))

  describe "prop parse / typecheck" $ do
    let parseExprs' :: Text -> Either String [Exp Info]
        parseExprs' t = parseExprs t & traverse . traverse . traverse .~ dummyInfo

    let textToProp'
          :: Map Text VarId
          -> Map VarId EType
          -> TableEnv
          -> Type a
          -> Text
          -> Either String (Prop a)
        textToProp' env1 env2 tableEnv ty t = case parseExprs' t of
          Right [exp'] ->
            expToProp tableEnv (VarId (Map.size env1)) env1 env2 HM.empty
              HM.empty ty exp'
          Left err -> Left err
          _        -> Left "Error: unexpected result from parseExprs"

        textToProp :: Type a -> Text -> Either String (Prop a)
        textToProp = textToProp'
          (Map.singleton "result" 0)
          (Map.singleton 0 (EType TAny))
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
          (Map.singleton 0 (EType TAny))
          (TableMap mempty)

        textToPropTableEnv :: TableEnv -> Type a -> Text -> Either String (Prop a)
        textToPropTableEnv tableEnv = textToProp'
          (Map.singleton "result" 0)
          (Map.singleton 0 (EType TAny))
          tableEnv

        singletonTableEnv :: TableName -> ColumnName -> EType -> TableEnv
        singletonTableEnv a b ty = TableMap $ Map.singleton a $
            ColumnMap $ Map.singleton b ty

    it "infers column-delta" $ do
      let tableEnv = singletonTableEnv "a" "b" (EType TInt)
      textToPropTableEnv tableEnv TBool "(> (column-delta a 'b) 0)"
        `shouldBe`
        Right (CoreProp $ IntegerComparison Gt (Inj (IntColumnDelta "a" "b")) 0)

      let tableEnv' = singletonTableEnv "a" "b" (EType TDecimal)
      textToPropTableEnv tableEnv' TBool "(> (column-delta a 'b) 0.0)"
        `shouldBe`
        Right (CoreProp $ DecimalComparison Gt (Inj (DecColumnDelta "a" "b")) 0)

      textToPropTableEnv tableEnv' TBool "(> (column-delta a \"b\") 0.0)"
        `shouldBe`
        Right (CoreProp $ DecimalComparison Gt (Inj (DecColumnDelta "a" "b")) 0)

    it "checks +" $ do
      textToProp TStr "(+ \"a\" \"b\")"
        `shouldBe`
        Right (PStrConcat (PLit "a") (PLit "b"))

      textToProp TInt "(+ 0 1)"
        `shouldBe`
        Right (Inj (IntArithOp Add 0 1 :: Numerical Prop Integer))

      textToProp TDecimal "(+ 0.0 1.0)"
        `shouldBe`
        Right (Inj (DecArithOp Add (PLit 0) (PLit 1)))

      textToProp TDecimal "(+ 0 1)"
        `shouldBe`
        Left "in (+ 0 1), unexpected argument types for (+): integer and integer"

    it "infers prop objects" $ do
      let pairSchema = Schema $
            Map.fromList [("x", EType TInt), ("y", EType TInt)]
          ety = EType TInt
          litPair = CoreProp $ LiteralObject $ Map.fromList
            [ ("x", ESimple TInt (PLit 0))
            , ("y", ESimple TInt (PLit 1))
            ]

          nestedObj = CoreProp $ LiteralObject $
            Map.singleton "foo" (EObject pairSchema litPair)

          nestedSchema = Schema $
            Map.singleton "foo" (EObjectTy pairSchema)

      inferProp'' "{ 'x: 0, 'y: 1 }"
        `shouldBe`
        Right (EObject pairSchema litPair)

      inferProp'' "(at 'x { 'x: 0, 'y: 1 })"
        `shouldBe`
        Right (ESimple TInt (PAt pairSchema (PLit "x") litPair ety))

      inferProp'' "{ 'foo: { 'x: 0, 'y: 1 } }"
        `shouldBe`
        Right (EObject nestedSchema nestedObj)

    it "infers forall / exists" $ do
      inferProp'' "(forall (x:string y:string) (= x y))"
        `shouldBe`
        Right
          (ESimple TBool
            (Inj $ Forall (VarId 1) "x" (EType TStr)
              (Inj $ Forall (VarId 2) "y" (EType TStr)
                (CoreProp $ StringComparison Eq
                  (PVar (VarId 1) "x" :: Prop String)
                  (PVar (VarId 2) "y")))))

      let tableEnv = singletonTableEnv "accounts" "balance" $ EType TInt
      textToPropTableEnv
        tableEnv
        TBool
        "(not (exists (row:string) (= (cell-delta accounts 'balance row) 2)))"
        `shouldBe`
        Right (PNot
          (Inj $ Exists (VarId 1) "row" (EType TStr)
            (CoreProp $ IntegerComparison Eq
              (Inj $ IntCellDelta "accounts" "balance" (PVar (VarId 1) "row"))
              2)))

    it "parses row-enforced / vars" $ do
      let env1 = Map.singleton "from" (VarId 1)
          env2 = Map.singleton (VarId 1) (EType TStr)
          tableEnv = singletonTableEnv "accounts" "ks" $ EType TKeySet
      textToProp' env1 env2 tableEnv TBool "(row-enforced accounts 'ks from)"
      `shouldBe`
      Right (Inj $ RowEnforced
        (TableNameLit "accounts")
        (ColumnNameLit "ks")
        (PVar (VarId 1) "from"))

    it "parses column properties" $
      let tableEnv = singletonTableEnv "accounts" "balance" $ EType TInt
      in textToPropTableEnv tableEnv TBool "(= (column-delta accounts 'balance) 0)"
           `shouldBe`
           Right (CoreProp $ IntegerComparison Eq (Inj $ IntColumnDelta "accounts" "balance") 0)

    it "parses (when (not (authorized-by 'accounts-admin-keyset)) abort)" $
      let tableEnv = singletonTableEnv "accounts" "accounts-admin-keyset" $ EType TKeySet
      in textToPropTableEnv tableEnv TBool "(when (not (authorized-by 'accounts-admin-keyset)) abort)"
         `shouldBe`
         Right (PLogical OrOp
           [ PLogical NotOp [
               PLogical NotOp [Inj $ KsNameAuthorized "accounts-admin-keyset"]
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

      textToProp   TBool "abort"   `shouldBe` Right Abort'
      textToProp   TBool "success" `shouldBe` Right Success'
      inferProp''  "abort"         `shouldBe` Right (ESimple TBool Abort')
      inferProp''  "success"       `shouldBe` Right (ESimple TBool Success')

      textToProp'' TBool "result"  `shouldBe` Right Result'
      textToProp'' TInt  "result"  `shouldBe` Right Result'
      textToProp'' TStr  "result"  `shouldBe` Right Result'

    it "parses quantified tables" $
      inferProp'' "(forall (table:table) (not (table-written table)))"
        `shouldBe`
        Right
          (ESimple TBool
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
          (ESimple TBool
            (Inj $ Forall (VarId 1) "table" QTable
              (Inj $ Forall (VarId 2) "column" (QColumnOf "table")
                (Inj (ColumnWritten
                  (CoreProp (Var (VarId 1) "table"))
                  (CoreProp (Var (VarId 2) "column")))))))

  describe "UserShow (PreProp)" $ do
    it "renders literals how you would expect" $ do
      userShow (PreIntegerLit 1)            `shouldBe` "1"
      userShow (PreStringLit "foo")         `shouldBe` "\"foo\""
      userShow (PreDecimalLit 1) `shouldBe` "1.0"
      -- TODO: test rendering time literals
      -- userShow (PreTimeLit _) `shouldBe` _
      userShow (PreBoolLit True)            `shouldBe` "true"
      userShow (PreBoolLit False)           `shouldBe` "false"

    it "renders quantifiers how you would expect" $ do
      userShow (PreForall 0 "foo" (EType TBool) (PreVar 0 "foo"))
        `shouldBe`
        "(forall (foo:bool) foo)"
      userShow (PreExists 0 "bar" (EType TBool) (PreApp "not" [PreVar 0 "bar"]))
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

  describe "UserShow" $
    it "schema looks okay" $ do
      let schema = Schema $
            Map.fromList [("name", EType TStr), ("balance", EType TInt)]
      userShow schema `shouldBe` "{ balance: integer, name: string }"

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

        expectTrace :: Text -> Prop Bool -> [TraceEvent -> Bool] -> Spec
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
        (bnot Success')
        [push, read, read, push, assert, assert, write, write, pop, pop]

    describe "doesn't include events excluded by a conditional" $ do
      let code =
            [text|
              (defun test:string ()
                (if false
                  (insert accounts "stu" {"balance": 5}) ; impossible
                  "didn't write"))
            |]
      expectTrace code (PLit False) [push, {- else -} path, pop]

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
      expectTrace code (bnot Success')
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
        schema         = Schema $ Map.singleton "balance" $ EType TInt
        readBalance ba = PAt schema "balance"
          (PropSpecific $ PropRead ba schema "accounts" acct)
          (EType TInt)
        exists ba      = PropSpecific (RowExists "accounts" acct ba)

    expectPass code7 $ Valid $
      Success'
      ==>
      PAnd (exists Before) (exists After)

    expectPass code7 $ Valid $
      Success'
      -- TODO: this arrow should point both ways
      ==>
      Inj (IntegerComparison Eq (readBalance After) 100)

    -- this should hold in general (for any contract)
    expectPass code7 $ Valid $ exists Before ==> exists After

    -- TODO:
    -- this could be generalized to a property that should hold in general
    -- expectPass code7 $ Valid $
    --   Abort'
    --   ==>
    --   Inj (IntegerComparison Eq (readBalance Before) (readBalance After))

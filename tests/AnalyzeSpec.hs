{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeApplications  #-}

module AnalyzeSpec (spec) where

import           Control.Lens               (at, findOf, (^.))
import           Control.Monad.State.Strict (runStateT)
import           Data.Either                (isLeft)
import qualified Data.HashMap.Strict        as HM
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (isJust, isNothing)
import           Data.SBV                   (Boolean (bnot, true, (&&&), (==>)))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           NeatInterpolation          (text)
import           Test.Hspec                 (Spec, describe, it, runIO,
                                             shouldBe, shouldSatisfy)

import           Pact.Parse                 (parseExprs)
import           Pact.Repl                  (evalRepl', initReplState)
import           Pact.Repl.Types            (ReplMode (StringEval), rEnv)
import           Pact.Types.Runtime         (ModuleData, eeRefStore, rsModules)

import           Pact.Analyze.Check
import           Pact.Analyze.Parse           (expToProp)
import           Pact.Analyze.PrenexNormalize (prenexConvert)
import           Pact.Analyze.Types

wrap :: Text -> Text
wrap code =
  [text|
    (env-keys ["admin"])
    (env-data { "keyset": { "keys": ["admin"], "pred": "=" } })
    (begin-tx)
    (define-keyset 'ks (read-keyset "keyset"))
    (module test 'ks
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

compile :: Text -> IO (Either CheckFailure ModuleData)
compile code = do
  replState0 <- initReplState StringEval
  (eTerm, replState) <- runStateT (evalRepl' $ T.unpack code) replState0
  pure $ case eTerm of
    Left err -> Left $ CodeCompilationFailed err
    Right _t ->
      case replState ^. rEnv . eeRefStore . rsModules . at "test" of
        Nothing -> Left $ CodeCompilationFailed "expected module 'test'"
        Just moduleData -> Right moduleData

runVerification :: Text -> IO (Maybe CheckFailure)
runVerification code = do
  eModuleData <- compile code
  case eModuleData of
    Left cf -> pure $ Just cf
    Right moduleData -> do
      results <- verifyModule (HM.fromList [("test", moduleData)]) moduleData
      -- TODO(joel): use `fromLeft` when we're on modern GHC
      pure $ case findOf (traverse . traverse) isLeft results of
        Just (Left (_parsed, failure)) -> Just failure
        _                              -> Nothing

runCheck :: Text -> Check -> IO (Maybe CheckFailure)
runCheck code check = do
  eModuleData <- compile code
  case eModuleData of
    Left cf -> pure $ Just cf
    Right moduleData -> do
      result <- verifyCheck moduleData "test" check
      pure $ case result of
        Left (_parsed, cf) -> Just cf
        Right _            -> Nothing

expectVerified :: Text -> Spec
expectVerified code = do
  res <- runIO $ runVerification $ wrap code
  it "passes in-code checks" $ res `shouldSatisfy` isNothing

expectPass :: Text -> Check -> Spec
-- TODO(joel): use expectNothing when it's available
expectPass code check = do
  res <- runIO $ runCheck (wrap code) check
  it (show check) $ res `shouldSatisfy` isNothing

expectFail :: Text -> Check -> Spec
expectFail code check = do
  res <- runIO $ runCheck (wrap code) check
  it (show check) $ res `shouldSatisfy` isJust

intConserves :: TableName -> ColumnName -> Prop Bool
intConserves tn cn = PIntegerComparison Eq 0 $ IntColumnDelta tn cn

decConserves :: TableName -> ColumnName -> Prop Bool
decConserves tn cn = PDecimalComparison Eq 0 $ DecColumnDelta tn cn

spec :: Spec
spec = describe "analyze" $ do
  describe "result" $ do
    let code =
          [text|
            (defun test:integer (x:integer)
              (* x -1))
          |]
    expectPass code $ Valid $ PIntegerComparison Eq
      (PIntArithOp Mul (-1) (PVar 0 "x"))
      (Result :: Prop Integer)

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
    expectPass code $ Valid $ PIntegerComparison Gte (Result :: Prop Integer) 10

  describe "success" $ do
    let code =
          [text|
            (defun test:bool (x:integer)
              (if (< x 10) true false))
          |]
    expectPass code $ Valid Success
    expectPass code $ Valid $ bnot Abort

  describe "enforce.trivial" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce false "cannot pass"))
          |]
    expectPass code $ Satisfiable Abort
    expectPass code $ Valid Abort

    expectFail code $ Satisfiable Success

  describe "enforce.conditional" $ do
    let code =
          [text|
            (defun test:bool (x:integer)
              (if (< x 10)
                (enforce (< x 5) "abort sometimes")
                true))
          |]
    expectPass code $ Satisfiable Abort
    expectPass code $ Satisfiable $ bnot Abort
    expectPass code $ Satisfiable Success

    expectFail code $ Valid Abort

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
    expectPass code $ Valid Abort

  describe "enforce.sequence" $ do
    let code =
          [text|
            (defun test:bool (x:integer)
              (enforce (> x 0) "positive")
              (if (< x 10)
                true
                false))
          |]
    expectPass code $ Satisfiable Abort
    expectPass code $ Satisfiable Success

  describe "enforce.sequence" $ do
    let code =
          [text|
            (defun test:bool (x:integer)
              (enforce (> x 0) "positive")
              (if (< x 10)
                true
                false))
          |]
    expectPass code $ Satisfiable Abort
    expectPass code $ Satisfiable Success
    expectPass code $ Valid $ (PIntegerComparison Gt (PVar 0 "x") (0 :: Prop Integer)) ==>
      Success
    expectPass code $ Valid $ (PIntegerComparison Eq (PVar 0 "x") (5 :: Prop Integer)) ==>
      Success &&& (PBoolComparison Eq (Result :: Prop Bool) true)

  describe "read-keyset.equality" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce
                (= (read-keyset "ks")
                   (read-keyset (+ "k" "s")))
                "keysets equality failed"))
          |]
    expectPass code $ Valid Success

  describe "enforce-keyset.name.static" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-keyset 'ks))
          |]
    expectPass code $ Satisfiable Abort
    expectPass code $ Satisfiable Success
    expectPass code $ Valid $ Success ==> KsNameAuthorized "ks"

    expectFail code $ Valid $ Success ==> KsNameAuthorized "different-ks"

  describe "enforce-keyset.name.dynamic" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-keyset (+ "k" "s")))
          |]
    expectPass code $ Valid $ bnot (KsNameAuthorized "ks") ==> Abort

  describe "enforce-keyset.value" $ do
    let code =
          [text|
            (defun test:bool ()
              (enforce-keyset (read-keyset (+ "k" "s"))))
          |]
    expectPass code $ Satisfiable Abort
    expectPass code $ Satisfiable Success
    expectPass code $ Valid $ bnot (KsNameAuthorized "ks") ==> Abort

    expectFail code $ Valid $ bnot (KsNameAuthorized "different-ks") ==> Abort

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
    expectPass code $ Satisfiable Abort
    expectPass code $ Satisfiable Success
    expectPass code $ Valid $ bnot $ Exists 0 "row" (Ty (Rep @String)) $
      RowWrite "tokens" (PVar 0 "row")
    expectPass code $ Valid $ Success ==>
      Exists 0 "row" (Ty (Rep @String)) (RowRead "tokens" (PVar 0 "row"))
    expectPass code $ Satisfiable $ Exists 0 "row" (Ty (Rep @String)) $
      RowEnforced "tokens" "ks" (PVar 0 "row")
    expectPass code $ Satisfiable $ Exists 0 "row" (Ty (Rep @String)) $
      bnot $ RowEnforced "tokens" "ks" (PVar 0 "row")
    expectPass code $ Valid $ Success ==> (Forall 0 "row" (Ty (Rep @String)) $
      RowRead "tokens" (PVar 0 "row") ==> RowEnforced "tokens" "ks" (PVar 0 "row"))
    expectPass code $ Valid $ Success ==> RowEnforced "tokens" "ks" (PVar 0 "acct")

  describe "enforce-keyset.row-level.read.syntax" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer
              ks:keyset)
            (deftable tokens:{token-row})

            (defun test:integer (acct:string)
              (meta "test"
                (property (forall (row:string)
                  (row-enforced "tokens" "ks" row))))
              (with-read tokens acct { "ks" := ks, "balance" := bal }
                (enforce-keyset ks)
                bal))
          |]

    -- TODO: come up with better tests. Right now this just tests that this
    -- parses correctly.
    expectPass code $ Satisfiable Abort

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
    expectPass code $ Valid $ Forall 0 "row" (Ty (Rep @String)) $
      RowRead "tokens" (PVar 0 "row") ==> RowEnforced "tokens" "ks1" (PVar 0 "row")
    -- Using the other keyset:
    expectFail code $ Valid $ Forall 0 "row" (Ty (Rep @String)) $
      RowRead "tokens" (PVar 0 "row") ==> RowEnforced "tokens" "ks2" (PVar 0 "row")

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
    expectPass code $ Satisfiable Abort
    expectPass code $ Satisfiable Success
    expectPass code $ Valid $ Success ==>
      Exists 0 "row" (Ty (Rep @String)) (RowWrite "tokens" (PVar 0 "row"))
    expectPass code $ Valid $ Success ==>
      Exists 0 "row" (Ty (Rep @String)) (RowRead "tokens" (PVar 0 "row"))
    expectPass code $ Valid $ Success ==>
      Exists 0 "row" (Ty (Rep @String)) (RowEnforced "tokens" "ks" (PVar 0 "row"))
    expectPass code $ Satisfiable $ Exists 0 "row" (Ty (Rep @String)) $
      bnot $ RowEnforced "tokens" "ks" (PVar 0 "row")
    expectPass code $ Valid $ Forall 0 "row" (Ty (Rep @String)) $
      RowRead "tokens" (PVar 0 "row") ==> RowEnforced "tokens" "ks" (PVar 0 "row")
    expectPass code $ Valid $ Forall 0 "row" (Ty (Rep @String)) $
      RowWrite "tokens" (PVar 0 "row") ==> RowEnforced "tokens" "ks" (PVar 0 "row")
    expectPass code $ Valid $ RowWrite "tokens" (PVar 0 "acct")
                          ==> RowEnforced "tokens" "ks" (PVar 0 "acct")

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
    expectFail code $ Valid $ Forall 0 "row" (Ty (Rep @String)) $
      RowRead "tokens" (PVar 0 "row") ==> RowEnforced "tokens" "ks" (PVar 0 "row")
    expectFail code $ Valid $ Forall 0 "row" (Ty (Rep @String)) $
      RowWrite "tokens" (PVar 0 "row") ==> RowEnforced "tokens" "ks" (PVar 0 "row")

  describe "table-read.multiple-read" $
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer)
            (deftable tokens:{token-row})

            (defun test:bool ()
              (insert tokens "stu" {"balance": 5, "name": "stu"})
              (let ((stu-name    (at 'name (read tokens "stu")))
                    (stu-balance (at 'balance (read tokens "stu"))))
                (enforce (= stu-name "stu") "name is stu")
                (enforce (= stu-balance 5) "balance is 5")))
          |]
    in expectPass code $ Valid Success

  describe "table-read.one-read" $
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
    in expectPass code $ Valid $ bnot Abort

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
    expectPass code $ Valid Success

    let schema = Schema $
          Map.fromList [("name", EType TStr), ("balance", EType TInt)]
        ety    = EType TStr
    expectPass code $ Valid $ PStringComparison Eq
      (PAt schema (PLit "name") Result ety)
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
    in expectPass code $ Valid $ bnot Abort

  describe "table-read" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:integer ()
              (with-read tokens "stu" {"balance" := bal}
                bal))
          |]
    expectPass code $ Valid $ TableRead "tokens"
    expectPass code $ Valid $ bnot $ TableRead "other"

  describe "table-write.insert" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (insert tokens "stu" {"balance": 5}))
          |]
    expectPass code $ Valid $ TableWrite "tokens"
    expectPass code $ Valid $ bnot $ TableWrite "other"

  describe "table-write.update" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (update tokens "stu" {"balance": 5}))
          |]
    expectPass code $ Valid $ TableWrite "tokens"

  describe "table-write.write" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string ()
              (write tokens "stu" {"balance": 5}))
          |]
    expectPass code $ Valid $ TableWrite "tokens"

  describe "table-write.conditional" $ do
    let code =
          [text|
            (defschema token-row balance:integer)
            (deftable tokens:{token-row})

            (defun test:string (x:bool)
              (if x
                (insert tokens "stu" {"balance": 5})
                "didn't write"))
          |]
    expectPass code $ Satisfiable $ TableWrite "tokens"
    expectPass code $ Satisfiable $ bnot $ TableWrite "tokens"
    expectPass code $ Valid $ bnot $ TableWrite "other"

  describe "table-write.conditional" $ do
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
    expectPass code $ Valid $ Success ==> bnot (TableWrite "tokens")

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

    expectPass code $ Valid $ Success ==> intConserves "accounts" "balance"

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

    expectPass code $ Satisfiable $ Success &&& intConserves "accounts" "balance"
    expectPass code $ Satisfiable $ Success &&& bnot (intConserves "accounts" "balance")

  describe "conserves-mass.decimal" $ do
    let code =
          [text|
            (defschema account2
              (meta "accounts schema"
                (invariant (>= balance 0.0)))
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
    expectPass code $ Valid $ Success ==> decConserves "accounts2" "balance"

  describe "cell-delta.integer" $ do
    let code =
          [text|
            (defun test:string ()
              (meta ""
                (properties [
                  (not (exists (row:string) (= (cell-delta 'accounts 'balance row) 2)))
                ]))
              (with-read accounts "bob" { "balance" := old-bob }
                (update accounts "bob" { "balance": (+ old-bob 2) })

                ; This overwrites the previous value:
                (update accounts "bob" { "balance": (+ old-bob 3) })
                ))
          |]

    expectVerified code

    expectPass code $ Valid $ bnot $ Exists 0 "row" (Ty (Rep @String)) $
      PIntegerComparison Eq (IntCellDelta "accounts" "balance" (PVar 0 "row")) 2

    expectPass code $ Valid $ Forall 0 "row" (Ty (Rep @String)) $
      PStringComparison Neq (PVar 0 "row" :: Prop String) (PLit "bob") ==>
        PIntegerComparison Eq (IntCellDelta "accounts" "balance" (PVar 0 "row")) 0

    expectPass code $ Valid $ Forall 0 "row" (Ty (Rep @String)) $
      PStringComparison Eq (PVar 0 "row" :: Prop String) (PLit "bob") ==>
        PIntegerComparison Eq (IntCellDelta "accounts" "balance" (PVar 0 "row")) 3

    expectPass code $ Valid $ Exists 0 "row" (Ty (Rep @String)) $
      PIntegerComparison Eq (IntCellDelta "accounts" "balance" (PVar 0 "row")) 3

    expectPass code $ Valid $
      PIntegerComparison Eq (IntCellDelta "accounts" "balance" (PLit "bob")) 3

  describe "with-read" $ do
    let code =
          [text|
            (defun test:bool (acct:string)
              (update accounts acct { "balance": 10 })
              (with-read accounts acct { "balance" := bal }
                (enforce (= bal 10) "Read after write failed")))
          |]

    expectPass code $ Valid Success

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

    expectPass code $ Valid Success

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

    expectPass code $ Valid Success

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

    expectPass code $ Valid Success

  describe "bind.from-literal" $ do
    let code =
          [text|
            (defun test:integer ()
              (let ((acct:object{account} { "balance": 10 }))
                (bind acct { "balance" := bal }
                  (enforce (= bal 10) "Bind failed")
                  bal)))
          |]

    expectPass code $ Valid Success

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
        in expectPass code $ Valid $ bnot Abort

      describe "2" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let ((y x)
                        (z (+ 10 x)))
                    (enforce (< z y) "z >= y")
                    true))
              |]
        in expectPass code $ Valid Abort

    describe "let*.sanity" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let* ((x 2)
                         (y (* x 10)))
                   (enforce (= 22 (+ x y)) "x + y != 22")))
              |]
        in expectPass code $ Valid $ bnot Abort

    describe "nested" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let ((x (let ((y 2)) y))
                        (y (let ((x 3)) x)))
                   (let ((z (let ((w 1)) (+ (+ x y) w))))
                     (enforce (= 6 z) "2 + 3 + 1 != 6"))))
              |]
        in expectPass code $ Valid $ bnot Abort

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
    in expectPass code $ Valid $ bnot Abort

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
              ))
          |]
    in expectPass code $ Valid $ bnot Abort

  describe "schema-invariants" $ do
    let code =
          [text|
            (defschema ints-row
              (meta "doc"
                (invariants
                  [(> pos 0)
                   (< neg 0)]))
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
    expectPass code $ Valid Success

  describe "schema-invariants.not-equals" $ do
    let code =
          [text|
            (defschema ints-row
              (meta "doc"
                (invariant (!= nonzero 0)))
              nonzero:integer)
            (deftable ints:{ints-row})

            (defun test:bool ()
              (with-read ints "any index" { "nonzero" := nz }
                (enforce (or (> nz 0) (< nz 0)) "is zero")))
          |]

    expectVerified code
    expectPass code $ Valid Success

  describe "schema-invariants.equals" $ do
    let code =
          [text|
            (defschema ints-row
              (meta "doc"
                (invariant (= zero 0)))
              zero:integer)
            (deftable ints:{ints-row})

            (defun test:bool ()
              (with-read ints "any index" { "zero" := z }
                (enforce (= z 0) "is not zero")))
          |]

    expectVerified code
    expectPass code $ Valid Success

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
    expectPass code $ Valid Success

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
    expectPass code $ Valid Success

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
    expectPass code $ Valid Success

  describe "enforce-keyset.row-level.read" $ do
    let code =
          [text|
            (defschema central-bank-schema
              (meta "central bank"
                (invariants
                  [(= 1000000 (+ reserve circulation))
                   (>= reserve 0)
                   (>= circulation 0)
                  ]))
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
    expectPass code $ Satisfiable Abort
    expectPass code $ Satisfiable Success

  describe "prenex conversion" $ do
    -- These test a somewhat irrelevant implementation detail -- the specific
    -- unique id, which is not ideal, but will do for now.
    let a0       = PVar 0 "a"
        a1       = PVar 1 "a"
        -- b        = PVar 0 "b"
        ty       = Ty (Rep @String)
        intTy    = Ty (Rep @Integer)
        allA0    = Forall 0 "a"
        allA1    = Forall 1 "a"
        existsA0 = Exists 0 "a"
        existsA1 = Exists 1 "a"

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
      prenexConvert (PAnd (PLit True) (allA0 intTy (PIntegerComparison Gte a0 a0)))
      `shouldBe`
      allA0 intTy (PAnd (PLit True) (PIntegerComparison Gte a0 a0))

  describe "prop parse / typecheck" $ do
    let textToProp'
          :: Map Text UniqueId
          -> Map UniqueId EType
          -> Type a
          -> Text
          -> Maybe (Prop a)
        textToProp' env1 env2 ty t = case parseExprs t of
          Right [exp'] -> expToProp (UniqueId (Map.size env1)) env1 env2 ty exp'
          _            -> Nothing

        textToProp :: Type a -> Text -> Maybe (Prop a)
        textToProp = textToProp' Map.empty Map.empty

    it "infers column-delta" $ do
      textToProp TBool "(> (column-delta 'a 'b) 0)"
        `shouldBe`
        Just (PIntegerComparison Gt (IntColumnDelta "a" "b") 0)

      textToProp TBool "(> (column-delta 'a 'b) 0.0)"
        `shouldBe`
        Just (PDecimalComparison Gt (DecColumnDelta "a" "b") 0)

      textToProp TBool "(> (column-delta \"a\" \"b\") 0.0)"
        `shouldBe`
        Just (PDecimalComparison Gt (DecColumnDelta "a" "b") 0)

    it "infers +" $ do
      textToProp TStr "(+ \"a\" \"b\")"
        `shouldBe`
        Just (PStrConcat (PLit "a") (PLit "b"))

      textToProp TInt "(+ 0 1)"
        `shouldBe`
        Just (PIntArithOp Add 0 1)

      textToProp TDecimal "(+ 0.0 1.0)"
        `shouldBe`
        Just (PDecArithOp Add (PLit 0) (PLit 1))

      textToProp TDecimal "(+ 0 1)" `shouldBe` Nothing

    it "infers forall / exists" $ do
      textToProp TBool "(forall (x:string y:string) (= x y))"
        `shouldBe`
        Just
          (Forall (UniqueId 1) "x" (Ty (Rep @String))
            (Forall (UniqueId 2) "y" (Ty (Rep @String))
              (PIntegerComparison Eq
                (PVar (UniqueId 1) "x")
                (PVar (UniqueId 2) "y"))))

      textToProp TBool
        "(not (exists (row:string) (= (cell-delta 'accounts 'balance row) 2)))"
        `shouldBe`
        Just (PNot
          (Exists (UniqueId 1) "row" (Ty (Rep @String))
            (PIntegerComparison Eq
              (IntCellDelta "accounts" "balance" (PVar (UniqueId 1) "row"))
              2)))

    it "parses row-enforced / vars" $ do
      let env1 = Map.singleton "from" (UniqueId 1)
          env2 = Map.singleton (UniqueId 1) (EType TStr)
      textToProp' env1 env2 TBool "(row-enforced 'accounts 'ks from)"
      `shouldBe`
      Just (RowEnforced (TableName "accounts") (ColumnName "ks") (PVar (UniqueId 1) "from"))

    it "parses column properties" $
      textToProp TBool "(= (column-delta 'accounts 'balance) 0)"
      `shouldBe`
      Just (PIntegerComparison Eq (IntColumnDelta "accounts" "balance") 0)

    it "parses (when (not (authorized-by 'accounts-admin-keyset)) abort)" $
      textToProp TBool "(when (not (authorized-by 'accounts-admin-keyset)) abort)"
      `shouldBe`
      Just (PLogical OrOp
        [ PLogical NotOp [
            PLogical NotOp [KsNameAuthorized "accounts-admin-keyset"]
          ]
        , Abort
        ])

    it "handles special identifiers" $ do
      textToProp TBool "abort"   `shouldBe` Just Abort
      textToProp TBool "success" `shouldBe` Just Success
      textToProp TBool "result"  `shouldBe` Just Result
      textToProp TInt  "result"  `shouldBe` Just Result
      textToProp TStr  "result"  `shouldBe` Just Result

{-# language GADTs             #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes       #-}
{-# language Rank2Types        #-}
{-# language TypeApplications  #-}
module AnalyzeSpec (spec) where

import           Control.Lens
import           Control.Monad.State.Strict (runStateT)
import           Data.Either                (isLeft)
import qualified Data.Map                   as Map
import           Data.Maybe                 (isJust, isNothing)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.SBV                   (Boolean(..))
import           NeatInterpolation
import           Test.Hspec

import           Pact.Analyze.Check
import           Pact.Analyze.Prop
import           Pact.Repl
import           Pact.Types.Runtime         hiding (RowKey, TableName)

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

runTest :: Text -> Check -> IO (Maybe CheckFailure)
runTest code check = do
  replState0 <- initReplState StringEval
  (eTerm, replState) <- runStateT (evalRepl' $ T.unpack code) replState0
  case eTerm of
    Left err -> pure $ Just $ CodeCompilationFailed err
    Right _t ->
      case replState ^. rEnv . eeRefStore . rsModules . at "test" of
        Nothing -> pure $ Just $ CodeCompilationFailed "expected module 'test'"
        Just moduleData -> do
          results <- verifyModule (Just check) moduleData
          -- TODO(joel): use `fromLeft` when we're on modern GHC
          pure $ case findOf (traverse . traverse) isLeft results of
            Just (Left failure) -> Just failure
            _                   -> Nothing

expectPass :: Text -> Check -> Spec
-- TODO(joel): use expectNothing when it's available
expectPass code check = do
  res <- runIO $ runTest (wrap code) check
  it (show check) $ res `shouldSatisfy` isNothing

expectFail :: Text -> Check -> Spec
expectFail code check = do
  res <- runIO $ runTest (wrap code) check
  it (show check) $ res `shouldSatisfy` isJust

intConserves :: TableName -> ColumnName -> Prop Bool
intConserves tn cn = PComparison Eq 0 $ IntColumnDelta tn cn

decConserves :: TableName -> ColumnName -> Prop Bool
decConserves tn cn = PComparison Eq 0 $ DecColumnDelta tn cn

spec :: Spec
spec = describe "analyze" $ do
  describe "result" $ do
    let code =
          [text|
            (defun test:integer (x:integer)
              (* x -1))
          |]
    expectPass code $ Valid $ PComparison Eq
      (PIntArithOp Mul (-1) "x")
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
    expectPass code $ Valid $ PComparison Gte (Result :: Prop Integer) 10

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
    expectPass code $ Valid $ (PComparison Gt "x" (0 :: Prop Integer)) ==>
      Success
    expectPass code $ Valid $ (PComparison Eq "x" (5 :: Prop Integer)) ==>
      Success &&& (PComparison Eq (Result :: Prop Bool) true)

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
    expectPass code $ Valid $ bnot $ Exists "row" (Ty (Rep @RowKey)) $
      RowWrite "tokens" (PVar "row")
    expectPass code $ Valid $ Exists "row" (Ty (Rep @RowKey)) $
      RowRead "tokens" (PVar "row")
    expectPass code $ Valid $ Exists "row" (Ty (Rep @RowKey)) $
      RowEnforced "tokens" "ks" (PVar "row")
    expectPass code $ Satisfiable $ Exists "row" (Ty (Rep @RowKey)) $
      bnot $ RowEnforced "tokens" "ks" (PVar "row")
    expectPass code $ Valid $ Forall "row" (Ty (Rep @RowKey)) $
      RowRead "tokens" (PVar "row") ==> RowEnforced "tokens" "ks" (PVar "row")
    expectPass code $ Valid $ Success ==> RowEnforced "tokens" "ks" "acct"

  describe "enforce-keyset.row-level.read.syntax" $ do
    let code =
          [text|
            (defschema token-row
              name:string
              balance:integer
              ks:keyset)
            (deftable tokens:{token-row})

            (defun test:integer (acct:string)
              ("test"
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
    expectPass code $ Valid $ Forall "row" (Ty (Rep @RowKey)) $
      RowRead "tokens" (PVar "row") ==> RowEnforced "tokens" "ks1" (PVar "row")
    -- Using the other keyset:
    expectFail code $ Valid $ Forall "row" (Ty (Rep @RowKey)) $
      RowRead "tokens" (PVar "row") ==> RowEnforced "tokens" "ks2" (PVar "row")

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
    expectPass code $ Valid $ Exists "row" (Ty (Rep @RowKey)) $
      RowWrite "tokens" (PVar "row")
    expectPass code $ Valid $ Exists "row" (Ty (Rep @RowKey)) $
      RowRead "tokens" (PVar "row")
    expectPass code $ Valid $ Exists "row" (Ty (Rep @RowKey)) $
      RowEnforced "tokens" "ks" (PVar "row")
    expectPass code $ Satisfiable $ Exists "row" (Ty (Rep @RowKey)) $
      bnot $ RowEnforced "tokens" "ks" (PVar "row")
    expectPass code $ Valid $ Forall "row" (Ty (Rep @RowKey)) $
      RowRead "tokens" (PVar "row") ==> RowEnforced "tokens" "ks" (PVar "row")
    expectPass code $ Valid $ Forall "row" (Ty (Rep @RowKey)) $
      RowWrite "tokens" (PVar "row") ==> RowEnforced "tokens" "ks" (PVar "row")
    expectPass code $ Valid $ RowWrite "tokens" (PVar "acct")
                          ==> RowEnforced "tokens" "ks" (PVar "acct")

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
    expectFail code $ Valid $ Forall "row" (Ty (Rep @RowKey)) $
      RowRead "tokens" (PVar "row") ==> RowEnforced "tokens" "ks" (PVar "row")
    expectFail code $ Valid $ Forall "row" (Ty (Rep @RowKey)) $
      RowWrite "tokens" (PVar "row") ==> RowEnforced "tokens" "ks" (PVar "row")

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

            (defun test:{token-row} ()
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
    expectPass code $ Valid $ PComparison Eq
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


   -- TODO: pending removal of some `undefined` causing CallStack here

   -- describe "conserves-mass.integer.without-uniqueness" $ do
   --     let code =
   --           [text|
   --             (defun test:string (from:string to:string amount:integer)
   --               "Transfer money between accounts"
   --               (let ((from-bal (at 'balance (read accounts from)))
   --                     (to-bal   (at 'balance (read accounts to))))
   --                 (enforce (> amount 0)         "Non-positive amount")
   --                 (enforce (>= from-bal amount) "Insufficient Funds")
   --                 (update accounts from { "balance": (- from-bal amount) })
   --                 (update accounts to   { "balance": (+ to-bal amount) })))
   --           |]

   --     expectPass code $ Valid $ bnot $ Success ==> intConserves "accounts" "balance"

  describe "conserves-mass.decimal" $ do
    let code =
          [text|
            (defschema account2 balance:decimal)
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

    expectPass code $ Valid $ Success ==> decConserves "accounts2" "balance"

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
                    (enforce (> z y) "z > y")
                    true))
              |]
        in expectPass code $ Valid $ bnot Abort

      describe "2" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let ((y x)
                        (z (+ 10 x)))
                    (enforce (< z y) "z > y")
                    true))
              |]
        in expectPass code $ Valid Abort

    describe "let*.sanity" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let* ((x 2)
                        (y (* x 10)))
                   (enforce (= 22 (+ x y)) "x + y = 22")))
              |]
        in expectPass code $ Valid $ bnot Abort

    describe "nested" $
        let code =
              [text|
                (defun test:bool (x:integer)
                  (let ((x (let ((y 2)) y))
                        (y (let ((x 3)) x)))
                   (let ((z (let ((w 1)) (+ (+ x y) w))))
                     (enforce (= 6 z) "2 + 3 + 1 = 6"))))
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
              ("doc"
                (invariants
                  ((> pos 0)
                   (< neg 0))))
              pos:integer
              neg:integer)
            (deftable ints:{ints-row} "Table of positive and negative integers")

            (defun test:bool ()
              (with-read ints "any index" { "pos" := pos, "neg" := neg }
                (enforce (> pos 0) "is not positive")
                (enforce (< neg 0) "is not negative")
                ))
          |]

    expectPass code $ Valid Success

  describe "schema-invariants.not-equals" $ do
    let code =
          [text|
            (defschema ints-row
              ("doc"
                (invariants ((!= nonzero 0))))
              nonzero:integer)
            (deftable ints:{ints-row})

            (defun test:bool ()
              (with-read ints "any index" { "nonzero" := nz }
                (enforce (or (> nz 0) (< nz 0)) "is zero")))
          |]

    expectPass code $ Valid Success

  describe "schema-invariants.equals" $ do
    let code =
          [text|
            (defschema ints-row
              ("doc"
                (invariants ((= zero 0))))
              zero:integer)
            (deftable ints:{ints-row})

            (defun test:bool ()
              (with-read ints "any index" { "zero" := z }
                (enforce (= z 0) "is not zero")))
          |]

    expectPass code $ Valid Success


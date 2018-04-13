{-# language OverloadedStrings #-}
{-# language QuasiQuotes       #-}

import           Data.Text          (Text)
import           EasyTest
import           NeatInterpolation

import           Pact.Analyze.Check
import           Pact.Analyze.Types hiding (scope)

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

expectPass :: Text -> Check -> Test ()
expectPass code check = expectRight =<< io (runTest (wrap code) check)

expectFail :: Text -> Check -> Test ()
expectFail code check = expectLeft =<< io (runTest (wrap code) check)

--
-- For now, we're not testing conditionals or sequence on their own, but as
-- they affect other "features". e.g. we test enforce.conditional or
-- enforce.sequence, but Not sequence.enforce or conditional.enforce.
--

suite :: Test ()
suite = tests
  [ scope "success" $ do
      let code =
            [text|
              (defun test:bool (x:integer)
                (if (< x 10) true false))
            |]
      expectPass code $ Valid $ Occurs Success
      expectPass code $ Valid $ Not $ Occurs Abort

  , scope "enforce.trivial" $ do
      let code =
            [text|
              (defun test:bool ()
                (enforce false "cannot pass"))
            |]
      expectPass code $ Satisfiable $ Occurs Abort
      expectPass code $ Valid $ Occurs Abort

      expectFail code $ Satisfiable $ Occurs Success

  , scope "enforce.conditional" $ do
      let code =
            [text|
              (defun test:bool (x:integer)
                (if (< x 10)
                  (enforce (< x 5) "abort sometimes")
                  true))
            |]
      expectPass code $ Satisfiable $ Occurs Abort
      expectPass code $ Satisfiable $ Not $ Occurs Abort
      expectPass code $ Satisfiable $ Occurs Success

      expectFail code $ Valid $ Occurs Abort

  , scope "enforce.sequence" $ do
      let code =
            [text|
              (defun test:bool (x:integer)
                (enforce (> x 0) "positive")
                (enforce false "impossible")
                (if (< x 10)
                  true
                  false))
            |]
      expectPass code $ Valid $ Occurs Abort

   , scope "enforce.sequence" $ do
      let code =
            [text|
              (defun test:bool (x:integer)
                (enforce (> x 0) "positive")
                (if (< x 10)
                  true
                  false))
            |]
      expectPass code $ Satisfiable $ Occurs Abort
      expectPass code $ Satisfiable $ Occurs Success

  , scope "enforce-keyset.name.static" $ do
      let code =
            [text|
              (defun test:bool ()
                (enforce-keyset 'ks))
            |]
      expectPass code $ Satisfiable $ Occurs Abort
      expectPass code $ Satisfiable $ Occurs Success
      expectPass code $ Valid $ Not (Occurs $ KsNameAuthorized "ks")
                                  `Implies` Occurs Abort

      expectFail code $ Valid $ Not (Occurs $ KsNameAuthorized "different-ks")
                                  `Implies` Occurs Abort

  , scope "enforce-keyset.name.dynamic" $ do
      let code =
            [text|
              (defun test:bool ()
                (enforce-keyset (+ "k" "s")))
            |]
      expectPass code $ Valid $ Not (Occurs $ KsNameAuthorized "ks")
                      `Implies` Occurs Abort

  --
  -- TODO: enforce-keyset.object
  --

  , scope "table-read.multiple-read" $
      let code =
            [text|
              (defschema token-row
                name:string
                balance:integer)
              (deftable tokens:{token-row})

              (defun test:string ()
                (insert tokens "stu" {"balance": 5, "name": "stu"})
                (let ((stu-name    (at 'name (read tokens "stu")))
                      (stu-balance (at 'balance (read tokens "stu"))))
                  (enforce (= stu-name "stu") "name is stu")
                  (enforce (= stu-balance 5) "balance is 5")))
            |]
      in expectPass code $ Valid $ Occurs Success

  , scope "table-read.one-read" $
      let code =
            [text|
              (defschema token-row
                name:string
                balance:integer)
              (deftable tokens:{token-row})

              (defun test:string ()
                (insert tokens "stu" {"balance": 5, "name": "stu"})
                (let ((stu (read tokens "stu")))
                  (enforce (= (at 'name stu) "stu") "name is stu")
                  (enforce (= (at 'balance stu) 5) "balance is 5")
                  )
                )
            |]
      in expectPass code $ Valid $ Not $ Occurs Abort

  , scope "at.dynamic-key" $
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
      in expectPass code $ Valid $ Not $ Occurs Abort

  -- TODO: pending fix for https://github.com/kadena-io/pact/issues/53
  -- , scope "at.object-in-object" $
  --     let code =
  --           [text|
  --             (defschema inner
  --               name:string)
  --             (defschema wrapper
  --               inner:{inner})

  --             (defun test:{inner} ()
  --               (let ((obj:{wrapper} {"inner": {"name": "pact"}}))
  --                 (at "inner" obj)))
  --           |]
  --     in expectPass code $ Valid $ Not $ Occurs Abort

  --
  -- TODO: test TableRead
  --

  , scope "table-write.insert" $ do
      let code =
            [text|
              (defschema token-row balance:integer)
              (deftable tokens:{token-row})

              (defun test:string ()
                (insert tokens "stu" {"balance": 5}))
            |]
      expectPass code $ Valid $ Occurs $ TableWrite "tokens"
      expectPass code $ Valid $ Not $ Occurs $ TableWrite "other"

  , scope "table-write.update" $ do
      let code =
            [text|
              (defschema token-row balance:integer)
              (deftable tokens:{token-row})

              (defun test:string ()
                (update tokens "stu" {"balance": 5}))
            |]
      expectPass code $ Valid $ Occurs $ TableWrite "tokens"

  , scope "table-write.write" $ do
      let code =
            [text|
              (defschema token-row balance:integer)
              (deftable tokens:{token-row})

              (defun test:string ()
                (write tokens "stu" {"balance": 5}))
            |]
      expectPass code $ Valid $ Occurs $ TableWrite "tokens"

  , scope "table-write.conditional" $ do
      let code =
            [text|
              (defschema token-row balance:integer)
              (deftable tokens:{token-row})

              (defun test:string (x:bool)
                (if x
                  (insert tokens "stu" {"balance": 5})
                  "didn't write"))
            |]
      expectPass code $ Satisfiable $ Occurs $ TableWrite "tokens"
      expectPass code $ Satisfiable $ Not $ Occurs $ TableWrite "tokens"
      expectPass code $ Valid $ Not $ Occurs $ TableWrite "other"

  , scope "table-write.conditional" $ do
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
      expectPass code $ Valid $ Occurs Success
                      `Implies` Not (Occurs $ TableWrite "tokens")

  , scope "conserves-mass" $ do
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

      expectPass code $ Valid $ Occurs Success
                      `Implies` Occurs (ColumnConserve "accounts" "balance")
      -- expectPass code $ Valid $ Occurs $ CellIncrease "accounts" "balance"

  , scope "with-read" $ do
      let code =
            [text|
              (defun test:integer (acct:string)
                (update accounts acct { "balance": 10 })
                (with-read accounts acct { "balance" := bal }
                  (enforce (= bal 10) "Read after write failed")))
            |]

      expectPass code $ Valid $ Occurs Success

  , scope "with-read.nested" $ do
      let code =
            [text|
              (defun test:integer (acct:string)
                (update accounts acct { "balance": 0 })
                (with-read accounts acct { "balance" := bal }
                  (update accounts acct { "balance": 10 })
                  (with-read accounts acct { "balance" := bal }
                    (enforce (= bal 10) "Shadowing failed"))))
            |]

      expectPass code $ Valid $ Occurs Success

  , scope "with-read.overlapping-names" $ do
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

      expectPass code $ Valid $ Occurs Success

  --
  -- TODO: pending fix for https://github.com/kadena-io/pact/issues/52
  --
  -- , scope "bind.from-read" $ do
  --     let code =
  --           [text|
  --             (defun test:integer ()
  --               (let ((obj:object{account} (read accounts "bob")))
  --                 (bind obj { "balance" := bal }
  --                   (enforce (= bal 10) "Bind failed")
  --                   bal)))
  --           |]
  --
  --     expectPass code $ Valid $ Occurs Success
  --
  -- , scope "bind.from-literal" $ do
  --     let code =
  --           [text|
  --             (defun test:integer ()
  --               (let ((acct:object{account} { "balance": 10 }))
  --                 (bind acct { "balance" := bal }
  --                   (enforce (= bal 10) "Bind failed")
  --                   bal)))
  --           |]
  --
  --     expectPass code $ Valid $ Occurs Success

  , scope "let" $ do
      scope "sanity" $ do
        scope "1" $
          let code =
                [text|
                  (defun test:bool (x:integer)
                    (let ((y x)
                          (z (+ 10 x)))
                      (enforce (> z y) "z > y")
                      true))
                |]
          in expectPass code $ Valid $ Not $ Occurs Abort

        scope "2" $
          let code =
                [text|
                  (defun test:bool (x:integer)
                    (let ((y x)
                          (z (+ 10 x)))
                      (enforce (< z y) "z > y")
                      true))
                |]
          in expectPass code $ Valid $ Occurs Abort

      scope "let*.sanity" $
          let code =
                [text|
                  (defun test:bool (x:integer)
                    (let* ((x 2)
                          (y (* x 10)))
                     (enforce (= 22 (+ x y)) "x + y = 22")))
                |]
          in expectPass code $ Valid $ Not $ Occurs Abort

      scope "nested" $
          let code =
                [text|
                  (defun test:bool (x:integer)
                    (let ((x (let ((y 2)) y))
                          (y (let ((x 3)) x)))
                     (let ((z (let ((w 1)) (+ (+ x y) w))))
                       (enforce (= 6 z) "2 + 3 + 1 = 6"))))
                |]
          in expectPass code $ Valid $ Not $ Occurs Abort

  , scope "time" $
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
      in expectPass code $ Valid $ Not $ Occurs Abort

  , scope "arith" $
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
      in expectPass code $ Valid $ Not $ Occurs Abort
  ]

main :: IO ()
main = run suite

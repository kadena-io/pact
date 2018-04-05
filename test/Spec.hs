{-# language OverloadedStrings #-}
{-# language QuasiQuotes       #-}

import           Data.Text            (Text)
import qualified Data.Text            as T
import           EasyTest
import           NeatInterpolation

import           Pact.Analyze.Analyze
import           Pact.Analyze.Types   hiding (scope)

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

  --, scope "conserves-mass" $ do
  --    let code =
  --          [text|
  --            (defun test:string (from:string to:string amount:integer)
  --              "Transfer money between accounts"
  --              (let ((from-bal (at 'balance (read accounts from)))
  --                    (to-bal   (at 'balance (read accounts to))))
  --                (enforce (> amount 0)         "Non-positive amount")
  --                (enforce (>= from-bal amount) "Insufficient Funds")
  --                (update accounts from { "balance": (- from-bal amount) })
  --                (update accounts to   { "balance": (+ to-bal amount) })))

  --              ;(with-read accounts from { "balance":= from-bal }
  --              ;  (with-read accounts to { "balance":= to-bal }
  --              ;    (enforce (>= from-bal amount) "Insufficient Funds")
  --              ;    (update accounts from { "balance": (- from-bal amount) })
  --              ;    (update accounts to   { "balance": (+ to-bal amount) }))))
  --          |]

  --    expectPass code $ Valid $ Occurs $ ColumnConserve "accounts" "balance"
  --     expectPass code $ Valid $ Occurs $ CellIncrease "accounts" "balance"

  --
  -- TODO: this is pending fixes to pact's typechecker.
  --
  -- , scope "enforce-keyset.name.dynamic" $ do
  --     let code =
  --           [text|
  --             (defun test:bool ()
  --               (enforce-keyset (+ "k" "s")))
  --           |]
  --     expectPass code $ Valid $ Not (Occurs $ KsNameAuthorized "ks")
  --                                 `Implies` Occurs Abort

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

  --
  -- TODO: test table-level reads, but to implement this we need to support
  --       objects.
  --

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
  ]

main :: IO ()
main = run suite

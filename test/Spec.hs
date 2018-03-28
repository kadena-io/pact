{-# language OverloadedStrings #-}
{-# language QuasiQuotes       #-}

import           Data.Text          (Text)
import qualified Data.Text          as T
import           EasyTest
import           NeatInterpolation

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
         data
         )
      (deftable accounts:{account}
        "Main table for accounts module.")
      $code
      )
    (commit-tx)
  |]

expectPass :: Text -> Check -> Test ()
expectPass code check = expectRight =<< io (runTest (wrap code) check)

expectFail :: Text -> Check -> Test ()
expectFail code check = expectLeft =<< io (runTest (wrap code) check)

suite :: Test ()
suite = tests
  [ scope "success" $ do
      let code =
            [text|
              (defun test:bool (x:integer)
                (if (< x 10) true false))
            |]
      expectPass code $ Valid $ Occurs Success

  , scope "success" $ do
      let code =
            [text|
              (defun test:bool ()
                (enforce false "cannot pass"))
            |]
      expectPass code $ Satisfiable $ Occurs Abort
      expectPass code $ Valid $ Occurs Abort

      expectFail code $ Satisfiable $ Occurs Success

  , scope "success" $ do
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

  , scope "conserves-masss" $ do
      let code =
            [text|
              (defun test:string (from to amount)
                  "Transfer money between accounts"
                  (with-read accounts from { "balance":= from-bal }
                    (with-read accounts to { "balance":= to-bal }
                        (enforce (>= from-bal amount) "Insufficient Funds")
                        (update accounts from
                                { "balance": (- from-bal amount) })
                        (update accounts to
                                { "balance": (+ to-bal amount) })
                    )
                  )
                )
            |]

      -- PROVE 'analyze-tests.accounts.balance' [ConservesMass, Column >= 0]
      expectPass code $ Valid $ Occurs $ ColumnConserves "accounts" "balance"
      expectPass code $ Valid $ Occurs $ CellIncrease "accounts" "balance"

  --
  -- NOTE: this is pending fixes to pact's typechecker.
  --
  -- , scope "enforce-keyset.name.dynamic" $ do
  --     let code =
  --           [text|
  --             (defun test:bool ()
  --               (enforce-keyset (+ "k" "s")))
  --           |]
  --     expectPass code $ Valid $ Not (Occurs $ KsNameAuthorized "ks")
  --                                 `Implies` Occurs Abort
  ]

main :: IO ()
main = run suite

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

  , scope "write.insert" $ do
      let code =
            [text|
              (defschema tokens-table
                balance:integer)
              (deftable tokens:{tokens-table})

              (defun test:string ()
                (insert tokens "stu" {"balance": 5}))
            |]
      expectPass code $ Valid $ Occurs $ TableWrite "tokens"
      expectPass code $ Valid $ Not $ Occurs $ TableWrite "other"

  , scope "write.update" $ do
      let code =
            [text|
              (defschema tokens-table
                balance:integer)
              (deftable tokens:{tokens-table})

              (defun test:string ()
                (update tokens "stu" {"balance": 5}))
            |]
      expectPass code $ Valid $ Occurs $ TableWrite "tokens"

  , scope "write.write" $ do
      let code =
            [text|
              (defschema tokens-table
                balance:integer)
              (deftable tokens:{tokens-table})

              (defun test:string ()
                (write tokens "stu" {"balance": 5}))
            |]
      expectPass code $ Valid $ Occurs $ TableWrite "tokens"
  ]

main :: IO ()
main = run suite

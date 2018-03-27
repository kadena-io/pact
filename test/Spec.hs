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

suite :: Test ()
suite = tests
  [ do result <- io $ runCompilerTest
         "examples/analyze-tests/analyze-tests.repl" "analyze-tests" "layup" $
           Valid $ Occurs Success
       expectRight result
       ok
  , do let code =
             [text|
               (defun test:bool (x:integer)
                 (if (< x 10) true false))
             |]
       expectRight =<< io (runTest (wrap code) $ Valid $ Occurs Success)
       ok
  , do let code =
             [text|
               (defun test:bool ()
                 (enforce false "cannot pass"))
             |]
       expectRight =<< io (runTest (wrap code) $ Satisfiable $ Occurs Abort)
       expectRight =<< io (runTest (wrap code) $ Valid $ Occurs Abort)
       -- expected failures:
       expectLeft  =<< io (runTest (wrap code) $ Satisfiable $ Occurs Success)
       ok
  , do let code =
             [text|
               (defun test:bool (x:integer)
                 (if (< x 10)
                   (enforce (< x 5) "abort sometimes")
                   true))
             |]
       expectRight =<< io (runTest (wrap code) $ Satisfiable $ Occurs Abort)
       expectRight =<< io (runTest (wrap code) $ Satisfiable $ Not $ Occurs Abort)
       expectRight =<< io (runTest (wrap code) $ Satisfiable $ Occurs Success)
       -- expected failure:
       expectLeft  =<< io (runTest (wrap code) $ Valid $ Occurs Abort)
       ok
  ]

main :: IO ()
main = run suite

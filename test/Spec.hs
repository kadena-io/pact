{-# language OverloadedStrings #-}
{-# language QuasiQuotes       #-}

import           Data.Text          (Text)
import qualified Data.Text          as T
import           EasyTest
import           NeatInterpolation

import           Pact.Analyze.Types

wrap :: Text -> String
wrap code = T.unpack $
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
       pure ()
  , do let code =
             [text|
               (defun test:bool (x: integer)
                 (if (< x 10) true false))
             |]
           prop = Valid $ Occurs Success
       result <- io $ runTest (wrap code) prop
       expectRight result
       pure ()
  ]

main :: IO ()
main = run suite

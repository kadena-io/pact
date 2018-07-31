module DocgenSpec where

import qualified Pact.Docgen as Docgen
import           Test.Hspec

spec :: Spec
spec = runIO Docgen.main

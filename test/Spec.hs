import Test.Framework

import qualified Execution
import qualified Expression

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Expression" Expression.tests
  , testGroup "Execution" Execution.tests
  ]
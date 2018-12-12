import           Control.Monad

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Basics
import qualified Test.Generators
import qualified Test.Patterns
import qualified Test.Practice
import qualified Test.Advanced
import qualified Test.StateMachine

main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Test.Basics.tests
    , Test.Generators.tests
    , Test.Patterns.tests
    , Test.Practice.tests
    , Test.Advanced.tests
    , Test.StateMachine.tests
    ] >>= \rs -> when (not . all id $ rs) Exit.exitFailure

import           Control.Monad

import qualified System.Exit as Exit
import           System.IO (IO)
import qualified System.IO as IO

main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
    ] >>= \rs -> when (not . all id $ rs) Exit.exitFailure

import Data.Kicad.PcbnewExpr
import System.Environment
import System.IO

main = do
  [file] <- getArgs
  contents <- readFile file
  x <- parse contents
  print x


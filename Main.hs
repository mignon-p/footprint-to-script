-- import Data.Kicad.PcbnewExpr
import Data.Kicad.SExpr
import System.Environment
import System.IO
import Text.PrettyPrint.Compact

main = do
  [file] <- getArgs
  contents <- readFile file
  let eth = parse contents
  case eth of
    Left s -> putStrLn s
    Right x -> putStrLn $ render 0.8 80 $ pretty x

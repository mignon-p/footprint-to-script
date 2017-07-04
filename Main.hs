import Data.Kicad.PcbnewExpr
import Language.Python.Common
import System.Environment
import System.IO
import Text.PrettyPrint

type Ann = ()
type ModuleA = Module Ann

defAnn :: Ann
defAnn = ()

footprintToModule :: PcbnewExpr -> ModuleA
footprintToModule pcb =
  Module
  [ FromImport 

footprintToStr :: PcbnewExpr -> String
footprintToStr = prettyText . footprintToModule

main = do
  [file] <- getArgs
  contents <- readFile file
  let eth = parse contents
  case eth of
    Left s -> putStrLn s
    Right x -> putStrLn $ render 0.8 80 $ pretty x

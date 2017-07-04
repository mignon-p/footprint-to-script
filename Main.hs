import Data.Kicad.PcbnewExpr
import Language.Python.Common
import System.Environment
import System.IO
import Text.PrettyPrint

imports :: [Statement ()]

initialize :: PcbnewModule -> [Statement ()]

itemToStatement :: PcbnewItem -> Statement ()

output :: [Statement ()]

footprintToModule :: PcbnewExpr -> Module ()
footprintToModule pcb =
  Module $ concat [imports, initialize pcb, items, output]
  where items = map itemToStatement (pcbnewModuleItems pcb)

footprintToStr :: PcbnewExpr -> String
footprintToStr = prettyText . footprintToModule

main = do
  [file] <- getArgs
  contents <- readFile file
  let eth = parse contents
  case eth of
    Left s -> putStrLn s
    Right x -> putStrLn $ render 0.8 80 $ pretty x

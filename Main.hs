import Data.Kicad.PcbnewExpr
import Language.Python.Common
import System.Environment
import System.IO
import Text.PrettyPrint

imports :: [Statement ()]
imports = [ FromImport fromModule fromItems () ]
  where
    fromModule = ImportRelative 0 (Just dotted) ()
    dotted = [ Ident "KicadModTree" () ]
    fromItems = ImportEverything ()

initialize :: PcbnewModule -> [Statement ()]

itemToStatement :: PcbnewItem -> Statement ()

output :: String -> [Statement ()]
output name = [ Assign asTo asExp (), StmtExpr stmtExpr () ]
  where
    asTo = [ Var (Ident "file_handler" ()) () ]
    asExp = Call callFun callArgs ()
    callFun = Var (Ident "KicadFileHandler" ()) ()
    callArgs = [ Var (Ident "kicad_mod" ()) () ]
    stmtExpr = Call callFun2 callArgs2 ()
    callFun2 = Dot dotExpr dotAttr ()
    dotExpr = Var (Ident "file_handler" ()) ()
    dotAttr = Ident "writeFile" ()
    callArgs2 = [ Strings [name ++ ".kicad_mod"] () ]

footprintToModule :: PcbnewExpr -> Module ()
footprintToModule pcb =
  Module $ concat [imports, initialize pcb, items, output name]
  where items = map itemToStatement (pcbnewModuleItems pcb)
        name = pcbnewModuleName pcb

footprintToStr :: PcbnewExpr -> String
footprintToStr = prettyText . footprintToModule

main = do
  [file] <- getArgs
  contents <- readFile file
  let eth = parse contents
  case eth of
    Left s -> putStrLn s
    Right x -> putStrLn $ render 0.8 80 $ pretty x

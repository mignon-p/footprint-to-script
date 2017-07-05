import Data.Kicad.PcbnewExpr
import Data.Maybe
import Language.Python.Common
import System.Environment
import System.IO
import Text.PrettyPrint

var :: String -> Expr ()
var s = Var (Ident s ()) ()

str :: String -> Expr ()
str s = Strings [s] ()

flo :: Double -> Expr ()
flo x = Float x (show x) ()

vect :: V2Double -> Expr ()
vect (x, y) = Tuple [ flo x, flo y ] ()

boo :: Bool -> Expr ()
boo b = Bool b ()

call :: String -> [Expr ()] -> Expr ()
call name args = Call (var name) (map ordinary args) ()
  where ordinary exp = ArgExpr exp ()

callKW :: String -> [(String, Expr ())] -> Expr ()
callKW name args = Call (var name) (map kw args) ()
  where kw (keyword, expr) = ArgKeyword (Ident keyword ()) expr ()

callMethod :: String -> String -> [Expr ()] -> Expr ()
callMethod receiver method args = Call dot (map ordinary args) ()
  where dot = Dot (var receiver) (Ident method ()) ()
        ordinary exp = ArgExpr exp ()

callMethSt :: String -> String -> [Expr ()] -> Statement ()
callMethSt receiver method args =
  StmtExpr (callMethod receiver method args) ()

assign :: String -> Expr () -> Statement ()
assign name exp = Assign [var name] exp ()

imports :: [Statement ()]
imports = [ FromImport fromModule fromItems () ]
  where
    fromModule = ImportRelative 0 (Just dotted) ()
    dotted = [ Ident "KicadModTree" () ]
    fromItems = ImportEverything ()

attrToStatement :: PcbnewAttribute -> Statement ()
attrToStatement (PcbnewDescr s) =
  Just $ callMethSt "kicad_mod" "setDescription" [str s]
attrToStatement (PcbnewTags s) =
  Just $ callMethSt "kicad_mod" "setTags" [str s]
attrToStatement _ = Nothing

initialize :: PcbnewModule -> [Statement ()]
initialize pcb = assignments ++ mapMaybe attrToStatement (pcbnewModuleAttrs pcb)
  where assignments =
          [ assign "footprint_name" (str (pcbNewModuleName pcb))
          , assign "kicad_mod" (call "Footprint" (var "footprint_name"))
          ]

apnd :: String -> [(String, Expr ())] -> Statement ()
apnd constructor args =
  callMethSt "kicad_mod" "append" $ callKW constructor args

pythag :: V2Double -> V2Double -> Double
pythag = undefined -- TODO

itemToStatement :: PcbnewItem -> Statement ()
itemToStatement item@(PcbnewFpText {}) =
  apnd "Text" [ ( "type" , str (fpTextTypeToStr (fpTextType item)) )
              , ( "text" , fpTextStr item )
              , ( "at" , vect (pcbnewAtPoint (itemAt item)) )
              , ( "rotation" , flo (pcbnewAtOrientation (itemAt item)) )
              , ( "layer" , str (layerToStr (itemLayer item)) )
              , ( "size" , vect (itemSize item) )
              , ( "thickness" , flo (fpTextThickness item) )
              , ( "hide" , boo (fpTextHide item) )
              ]
itemToStatement item@(PcbnewFpLine {}) =
  apnd "Line" [ ( "start" , vect (itemStart item) )
              , ( "end" , vect (itemEnd item) )
              , ( "layer" , str (layerToStr (itemLayer item)) )
              , ( "width" , flo (itemWidth item) )
              ]
itemToStatement item@(PcbnewFpCircle {}) =
  apnd "Circle" [ ( "center" , vect (itemStart item) )
                , ( "radius" , flo (pythag (itemStart item) (itemEnd item)) )
                , ( "layer" , str (layerToStr (itemLayer item)) )
                , ( "width" , flo (itemWidth item) )
                ]

output :: [Statement ()]
output = [ Assign asTo asExp (), StmtExpr stmtExpr () ]
  where
    asTo = [ Var (Ident "file_handler" ()) () ]
    asExp = Call callFun callArgs ()
    callFun = Var (Ident "KicadFileHandler" ()) ()
    callArgs = [ Var (Ident "kicad_mod" ()) () ]
    stmtExpr = Call callFun2 callArgs2 ()
    callFun2 = Dot dotExpr dotAttr ()
    dotExpr = Var (Ident "file_handler" ()) ()
    dotAttr = Ident "writeFile" ()
    callArgs2 = [ BinaryOp (Plus ()) leftOpArg rightOpArg () ]
    leftOpArg = Var (Ident "footprint_name" ()) ()
    rightOpArg = Strings [".kicad_mod"] ()

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

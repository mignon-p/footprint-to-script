import Data.Kicad.PcbnewExpr
import Data.Kicad.PcbnewExpr.PcbnewExpr
import Data.Maybe
import Language.Python.Common
import System.Environment
import System.IO
import Text.PrettyPrint
import Text.Printf

escape :: String -> String
escape s = concatMap e s
  where e '\"' = "\\\""
        e '\\' = "\\\\"
        e x
          | x < ' ' = printf "\\%03o" x
          | otherwise = [x]

var :: String -> Expr ()
var s = Var (Ident s ()) ()

str :: String -> Expr ()
str s = Strings ["\"", escape s, "\""] ()

flo :: Double -> Expr ()
flo x = Float x (show x) ()

vect :: V2Double -> Expr ()
vect (x, y) = List [ flo x, flo y ] ()

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

attrToStatement :: PcbnewAttribute -> Maybe (Statement ())
attrToStatement (PcbnewDescr s) =
  Just $ callMethSt "kicad_mod" "setDescription" [str s]
attrToStatement (PcbnewTags s) =
  Just $ callMethSt "kicad_mod" "setTags" [str s]
attrToStatement _ = Nothing

initialize :: PcbnewModule -> [Statement ()]
initialize pcb = assignments ++ mapMaybe attrToStatement (pcbnewModuleAttrs pcb)
  where assignments =
          [ assign "footprint_name" (str (pcbnewModuleName pcb))
          , assign "kicad_mod" (call "Footprint" [(var "footprint_name")])
          ]

apnd :: String -> [(String, Expr ())] -> Statement ()
apnd constructor args =
  callMethSt "kicad_mod" "append" [callKW constructor args]

pythag :: V2Double -> V2Double -> Double
pythag (x1, y1) (x2, y2) = sqrt (dx * dx + dy * dy)
  where dx = x2 - x1
        dy = y2 - y1

attrToPair :: PcbnewAttribute -> Maybe (String, Expr ())
attrToPair (PcbnewDrill drill) =
  case pcbnewDrillSize drill of
    Nothing -> Nothing
    Just v -> Just ("drill", vect v)
attrToPair (PcbnewOffset off) = Just ("offset", vect off)
attrToPair (PcbnewSolderPasteRatio rat) =
  Just ("solder_paste_margin_ratio", flo rat)
attrToPair _ = Nothing

itemToStatement :: PcbnewItem -> Statement ()
itemToStatement item@(PcbnewFpText {}) =
  apnd "Text" [ ( "type" , str (fpTextTypeToStr (fpTextType item)) )
              , ( "text" , str (fpTextStr item) )
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
itemToStatement item@(PcbnewFpArc {}) =
  apnd "Arc" [ ( "center" , vect (itemStart item) )
             , ( "start" , vect (itemEnd item) ) -- not sure about this
             , ( "angle" , flo (fpArcAngle item) )
             , ( "layer" , str (layerToStr (itemLayer item)) )
             , ( "width" , flo (itemWidth item) )
             ]
itemToStatement item@(PcbnewFpPoly {}) =
  apnd "PolygoneLine" [ ( "polygone" , List (map vect (fpPolyPts item)) () )
                      , ( "layer" , str (layerToStr (itemLayer item)) )
                      , ( "width" , flo (itemWidth item) )
                      ]
itemToStatement item@(PcbnewPad {}) =
  apnd "Pad" $ [ ( "number" , str (padNumber item) )
               , ( "type" , str (fpPadTypeToStr (padType item)) )
               , ( "shape" , str (fpPadShapeToStr (padShape item)) )
               , ( "at" , vect (pcbnewAtPoint (itemAt item)) )
               , ( "rotation" , flo (pcbnewAtOrientation (itemAt item)) )
               , ( "size" , vect (itemSize item) )
               , ( "layers" , List (map (str . layerToStr) (padLayers item)) () )
               ] ++ mapMaybe attrToPair (padAttributes_ item)

output :: [Statement ()]
output = [ assign asTo asExp, stmtExpr ]
  where
    asTo = "file_handler"
    asExp = call "KicadFileHandler" callArgs
    callArgs = [ var "kicad_mod" ]
    stmtExpr = callMethSt "file_handler" "writeFile" callArgs2
    callArgs2 = [ BinaryOp (Plus ()) leftOpArg rightOpArg () ]
    leftOpArg = var "footprint_name"
    rightOpArg = str ".kicad_mod"

footprintToModule :: PcbnewModule -> Module ()
footprintToModule pcb =
  Module $ concat [imports, initialize pcb, items, output]
  where items = map itemToStatement (pcbnewModuleItems pcb)

footprintToStr :: PcbnewModule -> String
footprintToStr = prettyText . footprintToModule

footprintToFile :: PcbnewModule -> FilePath -> IO ()
footprintToFile pcb file = withFile file WriteMode $ \h -> do
  hPutStrLn h "#!/usr/bin/env python3"
  hPutStrLn h ""
  hPutStrLn h $ footprintToStr pcb

main = do
  [file, out] <- getArgs
  contents <- readFile file
  let eth = parse contents
  case eth of
    Left s -> putStrLn s
    Right (PcbnewExprModule x) -> footprintToFile x out
    _ -> putStrLn "not a module"

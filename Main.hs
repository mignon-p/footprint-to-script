import Control.Monad.Trans.State.Strict
import Data.Kicad.PcbnewExpr hiding (pretty)
import Data.Kicad.PcbnewExpr.PcbnewExpr
import Data.List
import Data.Maybe
import Data.Ord
import Language.Python.Common
import System.Environment
import System.IO
import Text.PrettyPrint
import Text.Printf

type VarState = State [(Expr (), String)]

variableize :: Char
            -> Expr ()
            -> [(Expr (), String)]
            -> (String, [(Expr (), String)])
variableize c exp vars =
  let relevant = filter (\(_,x:_) -> c == x) vars
      found = lookup exp relevant
      largest = maximum (0 : map toNum relevant)
      toNum (_, _:s) = read s :: Int
      next = c : show (largest + 1)
  in case found of
    Just x -> (x, vars)
    Nothing -> (next, (exp, next) : vars)

vbz :: Char -> Expr () -> VarState (Expr ())
vbz c exp = do
  st <- get
  let (name, st') = variableize c exp st
  put st'
  return (var name)

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
flo x = Float x (printf "%f" x) ()

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

-- this is an abuse of Strings but seems to work!
blankLine :: Statement ()
blankLine = StmtExpr (Strings [""] ()) ()

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

apnd :: String -> [(String, Expr ())] -> VarState (Statement ())
apnd constructor args =
  return $ callMethSt "kicad_mod" "append" [callKW constructor args]

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

vbzAt :: PcbnewItem -> VarState (Expr ())
vbzAt item = do
  let (x, y) = pcbnewAtPoint (itemAt item)
  x' <- vbz 'x' $ flo x
  y' <- vbz 'y' $ flo y
  return $ List [x', y'] ()

itemToStatement :: PcbnewItem -> VarState (Statement ())
itemToStatement item@(PcbnewFpText {}) = do
  s <- vbz 's' $ vect (itemSize item)
  w <- vbz 'w' $ flo (fpTextThickness item)
  apnd "Text" [ ( "type" , str (fpTextTypeToStr (fpTextType item)) )
              , ( "text" , str (fpTextStr item) )
              , ( "at" , vect (pcbnewAtPoint (itemAt item)) )
              , ( "rotation" , flo (pcbnewAtOrientation (itemAt item)) )
              , ( "layer" , str (layerToStr (itemLayer item)) )
              , ( "size" , s )
              , ( "thickness" , w )
              , ( "hide" , boo (fpTextHide item) )
              ]
itemToStatement item@(PcbnewFpLine {}) = do
  w <- vbz 'w' $ flo (itemWidth item)
  apnd "Line" [ ( "start" , vect (itemStart item) )
              , ( "end" , vect (itemEnd item) )
              , ( "layer" , str (layerToStr (itemLayer item)) )
              , ( "width" , w )
              ]
itemToStatement item@(PcbnewFpCircle {}) = do
  w <- vbz 'w' $ flo (itemWidth item)
  apnd "Circle" [ ( "center" , vect (itemStart item) )
                , ( "radius" , flo (pythag (itemStart item) (itemEnd item)) )
                , ( "layer" , str (layerToStr (itemLayer item)) )
                , ( "width" , w )
                ]
itemToStatement item@(PcbnewFpArc {}) = do
  w <- vbz 'w' $ flo (itemWidth item)
  apnd "Arc" [ ( "center" , vect (itemStart item) )
             , ( "start" , vect (itemEnd item) ) -- not sure about this
             , ( "angle" , flo (fpArcAngle item) )
             , ( "layer" , str (layerToStr (itemLayer item)) )
             , ( "width" , w )
             ]
itemToStatement item@(PcbnewFpPoly {}) = do
  w <- vbz 'w' $ flo (itemWidth item)
  apnd "PolygoneLine" [ ( "polygone" , List (map vect (fpPolyPts item)) () )
                      , ( "layer" , str (layerToStr (itemLayer item)) )
                      , ( "width" , w )
                      ]
itemToStatement item@(PcbnewPad {}) = do
  s <- vbz 's' $ vect (itemSize item)
  apnd "Pad" $ [ ( "number" , str (padNumber item) )
               , ( "type" , str (fpPadTypeToStr (padType item)) )
               , ( "shape" , str (fpPadShapeToStr (padShape item)) )
               , ( "at" , vect (pcbnewAtPoint (itemAt item)) )
               , ( "rotation" , flo (pcbnewAtOrientation (itemAt item)) )
               , ( "size" , s )
               , ( "layers" , List (map (str . layerToStr) (padLayers item)) () )
               ] ++ mapMaybe attrToPair (padAttributes_ item)

itemsToStatements :: [PcbnewItem] -> [Statement ()]
itemsToStatements items = assignVars vars' ++ [blankLine] ++ stmts
  where (stmts, vars) = runState go []
        vars' = sortBy (comparing snd) vars
        go = mapM itemToStatement items
        assignVars = map assignVar
        assignVar (expr, name) = assign name expr

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
  Module $ intercalate [blankLine] [imports, initialize pcb, items, output]
  where items = itemsToStatements (pcbnewModuleItems pcb)

footprintToStr :: PcbnewModule -> String
footprintToStr = renderStyle sty . pretty . footprintToModule
  -- Unfortunately, language-python produces very long lines, despite
  -- the lineLength setting:
  -- https://github.com/bjpop/language-python/issues/3
  where sty = style { lineLength = 78 }

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

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Kicad.PcbnewExpr hiding (pretty)
import Data.Kicad.PcbnewExpr.PcbnewExpr
import Data.List
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord
import Language.Python.Common hiding ((<>))
import Options.Applicative hiding (str, style)
import qualified Options.Applicative as O (str)
import System.Environment
import System.Exit
import System.IO
import Text.PrettyPrint hiding ((<>))
import Text.Printf

data MyState =
  MyState
  { sVars :: [(Expr (), String)]
  , sModule :: String
  }

type VarState = State MyState

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
  let vars = sVars st
      (name, vars') = variableize c exp vars
      st' = st { sVars = vars' }
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

pad :: String -> Expr ()
pad p = Dot (var "Pad") (Ident p ()) ()

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

attrToPair :: PcbnewAttribute -> VarState (Maybe (String, Expr ()))
attrToPair (PcbnewDrill drill) = do
  case pcbnewDrillSize drill of
    Nothing -> return Nothing
    Just v -> do
      d <- vbz 'd' (vect v)
      return $ Just ("drill", d)
attrToPair (PcbnewOffset off) = return $ Just ("offset", vect off)
attrToPair (PcbnewSolderPasteRatio rat) =
  return $ Just ("solder_paste_margin_ratio", flo rat)
attrToPair _ = return Nothing

vbzXY :: V2Double -> VarState (Expr ())
vbzXY (x, y) = do
  x' <- vbz 'x' $ flo x
  y' <- vbz 'y' $ flo y
  return $ List [x', y'] ()

vbzTxt :: String -> VarState (Expr ())
vbzTxt s = do
  st <- get
  let modName = sModule st
  if s == modName
    then return (var "footprint_name")
    else return (str s)

pType :: PcbnewPadTypeT -> Expr ()
pType ThruHole = pad "TYPE_THT"
pType SMD = pad "TYPE_SMT"
pType Connect = pad "TYPE_CONNECT"
pType NPThruHole = pad "TYPE_NPTH"
-- pType x = str (fpPadTypeToStr x)

pShape :: PcbnewPadShapeT -> Expr ()
pShape Circle = pad "SHAPE_CIRCLE"
pShape Oval = pad "SHAPE_OVAL"
pShape Rect = pad "SHAPE_RECT"
pShape Trapezoid = pad "SHAPE_TRAPEZE"
pShape x = str (fpPadShapeToStr x)

layers :: [String] -> PcbnewPadTypeT -> Expr ()
layers ["F.Cu", "F.Mask", "F.Paste"] _ = pad "LAYERS_SMT"
layers ["*.Cu", "*.Mask"] NPThruHole = pad "LAYERS_NPTH"
layers ["*.Cu", "*.Mask"] _ = pad "LAYERS_THT"
layers ls _ = List (map str ls) ()

optionalArg :: String -> Expr () -> Expr () -> [(String, Expr ())]
optionalArg name val def
  | val == def = []
  | otherwise = [(name, val)]

optionalRot :: PcbnewItem -> [(String, Expr ())]
optionalRot item =
  let val = flo (pcbnewAtOrientation (itemAt item))
  in optionalArg "rotation" val (flo 0)

itemToStatement :: PcbnewItem -> VarState (Statement ())
itemToStatement item@(PcbnewFpText {}) = do
  txt <- vbzTxt (fpTextStr item)
  at <- vbzXY (pcbnewAtPoint (itemAt item))
  s <- vbz 't' $ vect (itemSize item)
  w <- vbz 'w' $ flo (fpTextThickness item)
  apnd "Text" $ [ ( "type" , str (fpTextTypeToStr (fpTextType item)) )
                , ( "text" , txt )
                , ( "at" , at )
                ] ++ optionalRot item ++
                [ ( "layer" , str (layerToStr (itemLayer item)) )
                , ( "size" , s )
                , ( "thickness" , w )
                ] ++ optionalArg "hide" (boo (fpTextHide item)) (boo False)
itemToStatement item@(PcbnewFpLine {}) = do
  start <- vbzXY (itemStart item)
  end <- vbzXY (itemEnd item)
  w <- vbz 'w' $ flo (itemWidth item)
  apnd "Line" [ ( "start" , start )
              , ( "end" , end )
              , ( "layer" , str (layerToStr (itemLayer item)) )
              , ( "width" , w )
              ]
itemToStatement item@(PcbnewFpCircle {}) = do
  center <- vbzXY (itemStart item)
  r <- vbz 'r' $ flo (pythag (itemStart item) (itemEnd item))
  w <- vbz 'w' $ flo (itemWidth item)
  apnd "Circle" [ ( "center" , center )
                , ( "radius" , r )
                , ( "layer" , str (layerToStr (itemLayer item)) )
                , ( "width" , w )
                ]
itemToStatement item@(PcbnewFpArc {}) = do
  center <- vbzXY (itemStart item)
  start <- vbzXY (itemEnd item) -- not sure about this
  w <- vbz 'w' $ flo (itemWidth item)
  apnd "Arc" [ ( "center" , center )
             , ( "start" , start )
             , ( "angle" , flo (fpArcAngle item) )
             , ( "layer" , str (layerToStr (itemLayer item)) )
             , ( "width" , w )
             ]
itemToStatement item@(PcbnewFpPoly {}) = do
  poly <- mapM vbzXY (fpPolyPts item)
  w <- vbz 'w' $ flo (itemWidth item)
  apnd "PolygoneLine" [ ( "polygone" , List poly () )
                      , ( "layer" , str (layerToStr (itemLayer item)) )
                      , ( "width" , w )
                      ]
itemToStatement item@(PcbnewPad {}) = do
  at <- vbzXY (pcbnewAtPoint (itemAt item))
  s <- vbz 'p' $ vect (itemSize item)
  attrs <- mapM attrToPair (padAttributes_ item)
  apnd "Pad" $ [ ( "number" , str (padNumber item) )
               , ( "type" , pType (padType item) )
               , ( "shape" , pShape (padShape item) )
               , ( "at" , at )
               ] ++ optionalRot item ++
               [ ( "size" , s )
               , ( "layers" , layers (map layerToStr (padLayers item)) (padType item) )
               ] ++ catMaybes attrs

itemsToStatements :: String -> [PcbnewItem] -> [Statement ()]
itemsToStatements modName items = assignVars vars' ++ [blankLine] ++ stmts
  where (stmts, MyState vars _) = runState go $ MyState [] modName
        vars' = sortBy (comparing cmp) vars
        go = mapM itemToStatement items
        assignVars = map assignVar
        assignVar (expr, name) = assign name expr
        cmp (_, c:n) = (c, read n :: Int)

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
  where items = itemsToStatements (pcbnewModuleName pcb) (pcbnewModuleItems pcb)

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

data Opts =
  Opts
  { oX   :: Double
  , oY   :: Double
  , oRot :: Int
  , oIn  :: String
  , oOut :: String
  }

opts :: Parser Opts
opts = Opts <$> optX <*> optY <*> optRot <*> argIn <*> argOut

optX :: Parser Double
optX = option auto (short 'x' <>
                    metavar "FLOAT" <>
                    help ("Amount to translate in X (default 0)") <>
                    value 0)

optY :: Parser Double
optY = option auto (short 'y' <>
                    metavar "FLOAT" <>
                    help ("Amount to translate in Y (default 0)") <>
                    value 0)

optRot :: Parser Int
optRot = option auto (short 'r' <>
                      metavar "DEGREES" <>
                      help ("Clockwise rotation around origin, " <>
                            "after applying translation.  Must be " <>
                            "0, 90, 180, or 270.  (default 0)") <>
                      value 0)

argIn :: Parser String
argIn = argument O.str (metavar "INPUTFILE.kicad_mod")

argOut :: Parser String
argOut = argument O.str (metavar "OUTPUTFILE.py")

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption "version x.y.z" (long "version" <>
                              short 'v' <>
                              help "Print version" <>
                              hidden)

opts' = info (helper <*> versionOpt <*> opts)
  ( fullDesc <>
    header "footprint-to-script - convert a KiCad footprint to a Python script"
  )

main = do
  o <- execParser opts'

  contents <- readFile (oIn o)
  let eth = parse contents
  case eth of
    Left s -> hPutStrLn stderr s >> exitFailure
    Right (PcbnewExprModule x) -> footprintToFile x (oOut o)
    _ -> hPutStrLn stderr "not a module" >> exitFailure

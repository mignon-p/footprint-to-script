{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Kicad.PcbnewExpr hiding (pretty)
import Data.Kicad.PcbnewExpr.PcbnewExpr
import Data.List
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord
import Data.Scientific
import Data.Version
import Language.Python.Common hiding ((<>))
import Options.Applicative hiding (str, style)
import qualified Options.Applicative as O (str)
import System.Environment
import System.Exit
import System.Info
import System.IO
import Text.PrettyPrint hiding ((<>))
import Text.Printf

import Paths_footprint_to_script

footprintVar :: String
footprintVar = "f"

data MyState =
  MyState
  { sVars :: [(Expr (), String)]
  , sModule :: String
  , sTransformPoint :: V2Sci -> V2Sci
  , sTransformRot :: Scientific -> Scientific
  }

type VarState = State MyState

-- We try to do as much math as possible using Scientific, instead
-- of Double, to avoid floating-point rounding error.
type V2Sci = (Scientific, Scientific)

dbl2sci :: Double -> Scientific
dbl2sci = read . show

vdbl2sci :: V2Double -> V2Sci
vdbl2sci (x, y) = (dbl2sci x, dbl2sci y)

sci2dbl :: Scientific -> Double
sci2dbl = toRealFloat

-- Given an expression, return a variable name.  This may either be
-- an existing variable, or a new variable.
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

-- A version of variableize that works in the VarState monad,
-- to keep track of the variable definitions.
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

-- Some helpers for generating Python expressions and statements.
var :: String -> Expr ()
var s = Var (Ident s ()) ()

str :: String -> Expr ()
str s = Strings ["\"", escape s, "\""] ()

flo :: Scientific -> Expr ()
flo x = Float (sci2dbl x) (show x) ()

dflo :: Double -> Expr ()
dflo x = Float x (printf "%f" x) ()

vect :: V2Sci -> Expr ()
vect (x, y) = List [ flo x, flo y ] ()

dvect :: V2Double -> Expr ()
dvect (x, y) = List [ dflo x, dflo y ] ()

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
  Just $ callMethSt footprintVar "setDescription" [str s]
attrToStatement (PcbnewTags s) =
  Just $ callMethSt footprintVar "setTags" [str s]
attrToStatement _ = Nothing

initialize :: PcbnewModule -> [Statement ()]
initialize pcb = assignments ++ mapMaybe attrToStatement (pcbnewModuleAttrs pcb)
  where assignments =
          [ assign "footprint_name" (str (pcbnewModuleName pcb))
          , assign footprintVar (call "Footprint" [(var "footprint_name")])
          ]

apnd :: String -> [(String, Expr ())] -> VarState (Statement ())
apnd constructor args =
  return $ callMethSt footprintVar "append" [callKW constructor args]

pythag :: V2Double -> V2Double -> Double
pythag (x1, y1) (x2, y2) = sqrt $ sci2dbl $ dx * dx + dy * dy
  where dx = dbl2sci x2 - dbl2sci x1
        dy = dbl2sci y2 - dbl2sci y1

attrToPair :: PcbnewAttribute -> VarState (Maybe (String, Expr ()))
attrToPair (PcbnewDrill drill) = do
  case pcbnewDrillSize drill of
    Nothing -> return Nothing
    Just v -> do
      d <- vbz 'd' (dvect v)
      return $ Just ("drill", d)
attrToPair (PcbnewOffset off) = return $ Just ("offset", dvect off)
attrToPair (PcbnewSolderPasteRatio rat) =
  return $ Just ("solder_paste_margin_ratio", dflo rat)
attrToPair _ = return Nothing

transformXY :: V2Sci -> VarState V2Sci
transformXY xy = do
  st <- get
  return $ sTransformPoint st xy

transformRot :: Scientific -> VarState Scientific
transformRot r = do
  st <- get
  return $ sTransformRot st r

vbzXY :: V2Double -> VarState (Expr ())
vbzXY xy = do
  (x, y) <- transformXY $ vdbl2sci xy
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

optionalRot :: PcbnewItem -> VarState [(String, Expr ())]
optionalRot item = do
  r <- transformRot $ dbl2sci $ pcbnewAtOrientation $ itemAt item
  return $ optionalArg "rotation" (flo r) (flo 0)

itemToStatement :: PcbnewItem -> VarState (Statement ())
itemToStatement item@(PcbnewFpText {}) = do
  txt <- vbzTxt (fpTextStr item)
  at <- vbzXY (pcbnewAtPoint (itemAt item))
  s <- vbz 't' $ dvect (itemSize item)
  w <- vbz 'w' $ dflo (fpTextThickness item)
  r <- optionalRot item
  apnd "Text" $ [ ( "type" , str (fpTextTypeToStr (fpTextType item)) )
                , ( "text" , txt )
                , ( "at" , at )
                ] ++ r ++
                [ ( "layer" , str (layerToStr (itemLayer item)) )
                , ( "size" , s )
                , ( "thickness" , w )
                ] ++ optionalArg "hide" (boo (fpTextHide item)) (boo False)
itemToStatement item@(PcbnewFpLine {}) = do
  start <- vbzXY (itemStart item)
  end <- vbzXY (itemEnd item)
  w <- vbz 'w' $ dflo (itemWidth item)
  apnd "Line" [ ( "start" , start )
              , ( "end" , end )
              , ( "layer" , str (layerToStr (itemLayer item)) )
              , ( "width" , w )
              ]
itemToStatement item@(PcbnewFpCircle {}) = do
  center <- vbzXY (itemStart item)
  r <- vbz 'r' $ dflo (pythag (itemStart item) (itemEnd item))
  w <- vbz 'w' $ dflo (itemWidth item)
  apnd "Circle" [ ( "center" , center )
                , ( "radius" , r )
                , ( "layer" , str (layerToStr (itemLayer item)) )
                , ( "width" , w )
                ]
itemToStatement item@(PcbnewFpArc {}) = do
  center <- vbzXY (itemStart item)
  start <- vbzXY (itemEnd item) -- not sure about this
  w <- vbz 'w' $ dflo (itemWidth item)
  apnd "Arc" [ ( "center" , center )
             , ( "start" , start )
             , ( "angle" , dflo (fpArcAngle item) )
             , ( "layer" , str (layerToStr (itemLayer item)) )
             , ( "width" , w )
             ]
itemToStatement item@(PcbnewFpPoly {}) = do
  poly <- mapM vbzXY (fpPolyPts item)
  w <- vbz 'w' $ dflo (itemWidth item)
  apnd "PolygoneLine" [ ( "polygone" , List poly () )
                      , ( "layer" , str (layerToStr (itemLayer item)) )
                      , ( "width" , w )
                      ]
itemToStatement item@(PcbnewPad {}) = do
  at <- vbzXY (pcbnewAtPoint (itemAt item))
  s <- vbz 'p' $ dvect (itemSize item)
  attrs <- mapM attrToPair (padAttributes_ item)
  r <- optionalRot item
  apnd "Pad" $ [ ( "number" , str (padNumber item) )
               , ( "type" , pType (padType item) )
               , ( "shape" , pShape (padShape item) )
               , ( "at" , at )
               ] ++ r ++
               [ ( "size" , s )
               , ( "layers" , layers (map layerToStr (padLayers item)) (padType item) )
               ] ++ catMaybes attrs

-- 90 degrees clockwise rotation
rot90 :: V2Sci -> V2Sci
rot90 (x, y) = (-y, x)

transformPtFunc :: Opts -> V2Sci -> V2Sci
transformPtFunc opt =
  let translate (x, y) = (x + dx, y + dy)
      dx = oX opt
      dy = oY opt
  in case oRot opt of
       0 -> translate
       90 -> rot90 . translate
       180 -> rot90 . rot90 . translate
       270 -> rot90 . rot90 . rot90 . translate
       -- should have already been caught before this point
       _ -> error "rotation must be one of 0, 90, 180, or 270"

transformRotFunc :: Opts -> Scientific -> Scientific
transformRotFunc opt rot = rot + fromIntegral (oRot opt)

itemsToStatements :: Opts -> String -> [PcbnewItem] -> [Statement ()]
itemsToStatements opt modName items = assignVars vars' ++ [blankLine] ++ stmts
  where (stmts, MyState vars _ _ _) = runState go $ MyState [] modName tPt tRot
        tPt = transformPtFunc opt
        tRot = transformRotFunc opt
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
    callArgs = [ var footprintVar ]
    stmtExpr = callMethSt "file_handler" "writeFile" callArgs2
    callArgs2 = [ BinaryOp (Plus ()) leftOpArg rightOpArg () ]
    leftOpArg = var "footprint_name"
    rightOpArg = str ".kicad_mod"

footprintToModule :: Opts -> PcbnewModule -> Module ()
footprintToModule opts pcb =
  Module $ intercalate [blankLine] [imports, initialize pcb, items, output]
  where items =
          itemsToStatements opts (pcbnewModuleName pcb) (pcbnewModuleItems pcb)

footprintToStr :: Opts -> PcbnewModule -> String
footprintToStr opts = renderStyle sty . pretty . footprintToModule opts
  -- Unfortunately, language-python produces very long lines, despite
  -- the lineLength setting:
  -- https://github.com/bjpop/language-python/issues/3
  where sty = style { lineLength = 78 }

footprintToFile :: Opts -> PcbnewModule -> FilePath -> IO ()
footprintToFile opts pcb file = withFile file WriteMode $ \h -> do
  hPutStrLn h "#!/usr/bin/env python3"
  hPutStrLn h ""
  hPutStrLn h $ footprintToStr opts pcb

-- command-line argument parsing

data Opts =
  Opts
  { oX   :: Scientific
  , oY   :: Scientific
  , oRot :: Int
  , oIn  :: String
  , oOut :: String
  }

opts :: Parser Opts
opts = Opts <$> optX <*> optY <*> optRot <*> argIn <*> argOut

optX :: Parser Scientific
optX = option auto (short 'x' <>
                    metavar "FLOAT" <>
                    help ("Amount to translate in X (default 0)") <>
                    value 0)

optY :: Parser Scientific
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

verStr :: String
verStr =
  "footprint-to-script " ++ showVersion version ++ "\n" ++
  "Compiled with " ++ compilerName ++ " " ++ showVersion compilerVersion ++
  "\nSee https://github.com/ppelleti/footprint-to-script"

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption verStr (long "version" <>
                     short 'v' <>
                     help "Print version" <>
                     hidden)

opts' = info (helper <*> versionOpt <*> opts)
  ( fullDesc <>
    header "footprint-to-script - convert a KiCad footprint to a Python script"
  )

main = do
  o <- execParser opts'

  when (oRot o `notElem` [0, 90, 180, 270]) $ do
    hPutStrLn stderr "rotation must be one of 0, 90, 180, or 270"
    exitFailure

  contents <- readFile (oIn o)
  let eth = parse contents
  case eth of
    Left s -> hPutStrLn stderr s >> exitFailure
    Right (PcbnewExprModule x) -> footprintToFile o x (oOut o)
    _ -> hPutStrLn stderr "not a module" >> exitFailure

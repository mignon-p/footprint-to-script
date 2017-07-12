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
import qualified Data.Text as T
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

footprintNameVar :: String
footprintNameVar = "footprint_name"

data Variable =
  Variable
  { vName :: String
  , vNum :: Int
  } deriving (Eq, Ord)

vStr :: Variable -> Int -> String
vStr v 0 = vName v
vStr v digs = vName v ++ printf fmt (vNum v)
  where fmt = "%0" ++ show digs ++ "v"

data MyState =
  MyState
  { sVars :: [(Expr (), Variable)]
  , sDigits :: [(String, Int)]
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
variableize :: String
            -> Expr ()
            -> [(String, Int)]
            -> [(Expr (), Variable)]
            -> (String, [(Expr (), Variable)])
variableize c exp digs vars =
  let relevant = filter (\(_,v) -> c == vName v) vars
      found = lookup exp relevant
      largest = maximum (0 : map (vNum . snd) relevant)
      next = Variable c (largest + 1)
      nDigs = fromMaybe 1 $ lookup c digs
  in case found of
    Just x -> (vStr x nDigs, vars)
    Nothing -> (vStr next nDigs, (exp, next) : vars)

-- A version of variableize that works in the VarState monad,
-- to keep track of the variable definitions.
vbz :: String -> Expr () -> VarState (Expr ())
vbz c exp = do
  st <- get
  let vars = sVars st
      digs = sDigits st
      (name, vars') = variableize c exp digs vars
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

dvect3 :: PcbnewXyzT -> Expr ()
dvect3 (x, y, z) = List [ dflo x, dflo y, dflo z ] ()

boo :: Bool -> Expr ()
boo b = Bool b ()

pad :: String -> Expr ()
pad p = Dot (var "Pad") (Ident p ()) ()

plus :: Expr () -> Expr () -> Expr ()
plus l r = BinaryOp (Plus ()) l r ()

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

splitOn :: String -> String -> [String]
splitOn d s = map T.unpack $ T.splitOn (T.pack d) (T.pack s)

subst :: String -> String -> Expr ()
subst name path =
  let strs = map str $ splitOn name path
      exprs = intersperse (var footprintNameVar) strs
  in foldl1' plus exprs

attrToStatement :: String -> PcbnewAttribute -> Maybe (Statement ())
attrToStatement _ (PcbnewDescr s) =
  Just $ callMethSt footprintVar "setDescription" [str s]
attrToStatement _ (PcbnewTags s) =
  Just $ callMethSt footprintVar "setTags" [str s]
attrToStatement name m@(PcbnewModel {}) =
  Just $ apnd' "Model" $ [ ( "filename", subst name (pcbnewModelPath m) )
                         , ( "at", dvect3 (pcbnewModelAt m) )
                         , ( "scale", dvect3 (pcbnewModelScale m) )
                         , ( "rotate", dvect3 (pcbnewModelRotate m) )
                         ]
attrToStatement _ _ = Nothing

initialize :: PcbnewModule -> [Statement ()]
initialize pcb =
  assignments ++ mapMaybe (attrToStatement name) (pcbnewModuleAttrs pcb)
  where assignments =
          [ assign footprintNameVar (str name)
          , assign footprintVar (call "Footprint" [(var footprintNameVar)])
          ]
        name = pcbnewModuleName pcb

apnd' :: String -> [(String, Expr ())] -> Statement ()
apnd' constructor args =
  callMethSt footprintVar "append" [callKW constructor args]

apnd :: String -> [(String, Expr ())] -> VarState (Statement ())
apnd constructor args = return $ apnd' constructor args

pythag :: V2Double -> V2Double -> Double
pythag (x1, y1) (x2, y2) = sqrt $ sci2dbl $ dx * dx + dy * dy
  where dx = dbl2sci x2 - dbl2sci x1
        dy = dbl2sci y2 - dbl2sci y1

attrToPair :: PcbnewAttribute -> VarState (Maybe (String, Expr ()))
attrToPair (PcbnewDrill drill) = do
  case pcbnewDrillSize drill of
    Nothing -> return Nothing
    Just v -> do
      d <- vbz "d" (dvect v)
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
  x' <- vbz "x" $ flo x
  y' <- vbz "y" $ flo y
  return $ List [x', y'] ()

vbzW :: PcbnewItem -> VarState (Expr ())
vbzW item = do
  let w = dflo (itemWidth item)
      l = layerToStr (itemLayer item)
      afterDot = drop 1 . dropWhile (/= '.')
  vbz ('w' : afterDot l) w

vbzTxt :: String -> VarState (Expr ())
vbzTxt s = do
  st <- get
  let modName = sModule st
  if s == modName
    then return (var footprintNameVar)
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
  s <- vbz "s" $ dvect (itemSize item)
  w <- vbz "t" $ dflo (fpTextThickness item)
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
  w <- vbzW item
  apnd "Line" [ ( "start" , start )
              , ( "end" , end )
              , ( "layer" , str (layerToStr (itemLayer item)) )
              , ( "width" , w )
              ]
itemToStatement item@(PcbnewFpCircle {}) = do
  center <- vbzXY (itemStart item)
  r <- vbz "r" $ dflo (pythag (itemStart item) (itemEnd item))
  w <- vbzW item
  apnd "Circle" [ ( "center" , center )
                , ( "radius" , r )
                , ( "layer" , str (layerToStr (itemLayer item)) )
                , ( "width" , w )
                ]
itemToStatement item@(PcbnewFpArc {}) = do
  center <- vbzXY (itemStart item)
  start <- vbzXY (itemEnd item) -- not sure about this
  w <- vbzW item
  apnd "Arc" [ ( "center" , center )
             , ( "start" , start )
             , ( "angle" , dflo (fpArcAngle item) )
             , ( "layer" , str (layerToStr (itemLayer item)) )
             , ( "width" , w )
             ]
itemToStatement item@(PcbnewFpPoly {}) = do
  -- This isn't correct.  KicadModTree doesn't seem to have support
  -- for polygons.  ("PolygoneLine" is a polyline, not a polygon.)
  poly <- mapM vbzXY (fpPolyPts item)
  w <- vbzW item
  apnd "PolygoneLine" [ ( "polygone" , List poly () )
                      , ( "layer" , str (layerToStr (itemLayer item)) )
                      , ( "width" , w )
                      ]
itemToStatement item@(PcbnewPad {}) = do
  at <- vbzXY (pcbnewAtPoint (itemAt item))
  s <- vbz "p" $ dvect (itemSize item)
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

renumberGroup :: [(Expr (), Variable)]
              -> ([(Expr (), Variable)], (String, Int))
renumberGroup vars =
  let sorted = sort vars
      renumbered = zipWith renum sorted [1..]
      renum (e, v) n = (e, Variable (vName v) n)
      name = vName $ snd $ head renumbered
      num = length renumbered
      digs = ceiling $ logBase 10 $ fromIntegral num + 0.5
  in (renumbered, (name, if num == 1 then 0 else digs))

renumberVariables :: [(Expr (), Variable)]
                  -> ([(Expr (), Variable)], [(String, Int)])
renumberVariables vars =
  let groups = groupBy (\(_, v1) (_, v2) -> vName v1 == vName v2) vars
      (groups', digs) = unzip $ map renumberGroup groups
  in (concat groups', digs)

renumber :: VarState ()
renumber = do
  st <- get
  let (vars, digs) = renumberVariables (sVars st)
      st' = st { sVars = vars, sDigits = digs }
  put st'

itemsToStatements :: Opts -> String -> [PcbnewItem] -> [Statement ()]
itemsToStatements opt modName items = assignVars vars' ++ [blankLine] ++ stmts
  where (stmts, MyState vars digs _ _ _) =
          runState (go >> renumber >> go) $ MyState [] [] modName tPt tRot
        tPt = transformPtFunc opt
        tRot = transformRotFunc opt
        vars' = sortBy (comparing snd) vars
        go = mapM itemToStatement items
        assignVars = map assignVar
        assignVar (expr, v) =
          assign (vStr v $ fromMaybe 1 $ lookup (vName v) digs) expr

output :: [Statement ()]
output = [ assign asTo asExp, stmtExpr ]
  where
    asTo = "file_handler"
    asExp = call "KicadFileHandler" callArgs
    callArgs = [ var footprintVar ]
    stmtExpr = callMethSt "file_handler" "writeFile" callArgs2
    callArgs2 = [ leftOpArg `plus` rightOpArg ]
    leftOpArg = var footprintNameVar
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

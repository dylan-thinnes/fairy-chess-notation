module Notation.Debug where

import Notation
import Notation.Folds
import Notation.Parse

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game

-- import qualified Diagrams.Prelude as D
-- import Diagrams.Prelude ((|||),(===))
-- import qualified Diagrams.Backend.SVG as D
-- import Diagrams.Attributes

import Data.Colour.SRGB

import qualified Data.HashSet as S
import Data.List (transpose)
import Data.Bifunctor (bimap)
import Control.Lens (_Just, _1, _2, _3, view, (.~), (%~))

-- Utils
(#) x f = f x

-- Get the minimum and maximum for a list
minmax :: Ord a => [a] -> (a,a)
minmax xs = (minimum xs, maximum xs)

-- Get ranges of each axis, with at least coord 0 always in range
getRanges :: [[Integer]] -> [(Integer, Integer)]
getRanges coords
  = let minsmaxes = map minmax $ transpose coords
        clampedToZero = map (bimap (min 0) (max 0)) minsmaxes
     in if null clampedToZero
           then repeat (0,0)
           else clampedToZero

-- Runner
debug :: (String -> Moveset -> IO a) -> IO a
debug f = do
    input <- getLine
    let parse = parseMove input
    case parse of
      Left err   -> do
                      putStrLn ("No parse: " ++ show err)
                      putStrLn "Try again."
                      debug f
      Right tree -> f input $ treeToSet tree

-- IN TERMINAL RENDERER
terminal :: String -> Moveset -> IO ()
terminal _ ms
  = putStr $ unlines $ map concat
  $ flip map [maxY,maxY-1..minY] $ \y ->
        flip map [minX..maxX] $ \x ->
            coordChar [x,y]
    where
    coords = getCoords ms              -- Turn whole moveset to final deltas
    coordsSet = S.fromList coords      -- Turn that moveset into a set for quick lookup

    ((minX,maxX):(minY,maxY):_) = getRanges coords

    -- Turn a coord into the appropriate character
    coordChar c | c == [0,0]             = "\ESC[36;1mO\ESC[0m" -- O if origin
                | c `S.member` coordsSet = "\ESC[35;1mX\ESC[0m" -- X if coord is the endpoint of a move
                | otherwise              = "+" -- + if coord isn't anything important

-- Convert a move to its final coord
getCoord :: Move -> [Integer]
getCoord = _delta . sum . _move

-- Convert a move to its final coord
getCoordPath :: Move -> [[Integer]]
getCoordPath = map _delta . tail . scanl (+) 0 . _move

-- Turn whole moveset to final deltas
getCoords :: Moveset -> [[Integer]]
getCoords = map getCoord . _moveset

-- GLOSS RENDERER
-- Gloss-specific Utils
thickLineSeg :: (Point, Point) -> Picture
thickLineSeg (a@(x1, y1), b@(x2, y2))
  = uncurry translate a
  $ uncurry translate (mulSV 0.5 $ diff a b)
  $ rotate (negate $ radToDeg $ argV $ diff a b)
  $ Polygon (rectanglePath (magV $ diff a b) 2)

diff :: Vector -> Vector -> Vector
diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- Actual gloss runner
type Static = (String, Moveset, S.HashSet [Integer], [(Integer, Integer)])
type State = (Static, Cursor, Picture)

gloss :: String -> Moveset -> IO ()
gloss source ms
  = play 
    (InWindow "Gloss" (100,100) (0,0)) white 1
    initialState
    (view _3) handler
    (const id)
    where
    initialStatic = (source, ms, S.fromList (getCoords ms), getRanges $ getCoords ms)
    initialState = (initialStatic, Nothing, render initialStatic Nothing ms)
    handler (EventMotion pos) state@(moveset, cursor, _)
      | Just newPos == fmap fst cursor = state
      | otherwise                      = updateCursor (setPos newPos) state
        where
        newPos = roundToTile pos
    handler (EventKey (MouseButton RightButton) Down _ _) state
      = updateCursor cycleMode state
    handler _ state = state

render :: Static -> Cursor -> Moveset -> Picture
render (source, moveset, _, bounds) cursor subset
  = Pictures
  $ [ glossBoardFromRange bounds
    , cursorGloss cursor
    , glossMoveset subset
    -- , infoFromBounds bounds source
    , ThickCircle 4 2 # color black
    ]

infoFromBounds :: [(Integer, Integer)] -> String -> Picture
infoFromBounds ((minX,maxX):(minY,maxY):_) source
  = translate 0 --(fromIntegral $ maxX * 20 + 20)
              (fromIntegral $ maxY * 20 + 20)
              (scale 0.1 0.1 $ text source)

type Cursor = Maybe ((Float, Float), CursorMode)
data CursorMode = None | EndPoint | MidPoint
    deriving (Show, Eq, Enum, Bounded)
updateCursor :: (Cursor -> Cursor) -> State -> State
updateCursor f (static, cursor, _)
  = let newCur = f cursor
     in (static, newCur, render static newCur $ restrictByCursor newCur static)
setPos :: (Float, Float) -> Cursor -> Cursor
setPos newPos Nothing = Just (newPos, None)
setPos newPos x       = _Just . _1 .~ newPos $ x
cycleMode :: Cursor -> Cursor
cycleMode = _Just . _2 %~ f
    where
    f newMode | newMode == maxBound = minBound
              | otherwise           = succ newMode

restrictByCursor :: Cursor -> Static -> Moveset
restrictByCursor Nothing (_, ms, _, _) = ms
restrictByCursor (Just ((x,y), mode)) (_, Moveset ms, endpoints, bounds)
  = if pos `S.member` endpoints then Moveset newMs else Moveset ms
    where
    pos = [floor x `div` 20, floor y `div` 20]
    endsOnXY move = pos == getCoord move
    containsXY move = pos `elem` getCoordPath move
    newMs | mode == None = ms
          | mode == EndPoint = filter endsOnXY ms
          | mode == MidPoint = filter containsXY ms

cursorGloss :: Cursor -> Picture
cursorGloss Nothing = Blank
cursorGloss (Just ((x,y), mode))
  = translate x y
  $ color (modeColor mode)
  $ rectangleSolid 20 20

modeColor :: CursorMode -> Color
modeColor None       = makeColorI 0   0 0   100
modeColor EndPoint   = makeColorI 255 0 0   100
modeColor MidPoint   = makeColorI 0   0 255 100

roundToTile :: (Float, Float) -> (Float, Float)
roundToTile = bimap roundTo20 roundTo20
roundTo20 :: Float -> Float
roundTo20 x = fromIntegral $ 20 * round (x / 20)

glossMoveset :: Moveset -> Picture
glossMoveset ms@(Moveset moves) = Pictures $ map glossMove moves

glossMove :: Move -> Picture
glossMove (Move ds) = color (makeColorI 0 0 255 120) path
    where
    vs = map ((\[a,b] -> (fromIntegral a * 20, fromIntegral b * 20)) . _delta) ds
    addToPath v p = Pictures [thickLineSeg ((0,0),v), uncurry translate v p]
    path = foldr addToPath (color red $ ThickCircle 4 2) vs

glossBoard :: Moveset -> Picture
glossBoard ms = glossBoardFromRange $ getRanges $ getCoords ms

glossBoardFromRange :: [(Integer, Integer)] -> Picture
glossBoardFromRange ((minX,maxX):(minY,maxY):_) = board
    where
    board = Pictures $ concat
            $ flip map [maxY,maxY-1..minY] $ \y ->
                  flip map [minX..maxX] $ \x ->
                      tile x y
    xor x y = not $ x == y
    tile x y = Polygon (rectanglePath 20 20)
             # color (tileColor x y)
             # translate (fromIntegral $ x * 20) (fromIntegral $ y * 20)
    tileColor x y = if even x `xor` even y
                       then makeColorI 200 250 200 255
                       else makeColorI 100 200 100 255

    {-
-- DIAGRAMS RENDERER
diaMove :: Move -> D.Diagram D.B
diaMove (Move ds) = undefined
    where
    vs = map ((\[a,b] -> (fromIntegral a * 50, fromIntegral b * 50)) . _delta) ds

diaBoard :: [(Integer, Integer)] -> D.Diagram D.B
diaBoard [(minX,maxX),(minY,maxY)] = board
    where
    board = foldr1 (|||) $ map (foldr1 (===))
            $ flip map [minY..maxY] $ \y ->
                  flip map [minX..maxX] $ \x ->
                      D.square 50
                      # D.lw none
                      # D.fc (tileColor x y)
    xor x y = not $ x == y
    tileColor x y = if even x `xor` even y
                       then sRGB 0.8 1.0 0.8
                       else sRGB 0.4 0.8 0.4
    -}

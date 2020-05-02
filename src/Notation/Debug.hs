module Notation.Debug where

import Notation
import Notation.Folds
import Notation.Parse

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

-- import qualified Diagrams.Prelude as D
-- import Diagrams.Prelude ((|||),(===))
-- import qualified Diagrams.Backend.SVG as D
-- import Diagrams.Attributes

import Data.Colour.SRGB

import qualified Data.HashSet as S
import Data.List (transpose)
import Data.Bifunctor (bimap)

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
debug :: (Moveset -> IO a) -> IO a
debug f = do
    input <- getLine
    let parse = parseMove input
    case parse of
      Left err   -> do
                      putStrLn ("No parse: " ++ show err)
                      putStrLn "Try again."
                      debug f
      Right tree -> f $ treeToSet tree

-- IN TERMINAL RENDERER
terminal :: Moveset -> IO ()
terminal ms
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

getCoords :: Moveset -> [[Integer]]
getCoords ms = coords
    where
    toCoord = _delta . sum . _move     -- Convert a move to its final coord
    coords = map toCoord $ _moveset ms -- Turn whole moveset to final deltas

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
gloss :: Moveset -> IO ()
gloss = display FullScreen white . glossMoveset

glossMoveset :: Moveset -> Picture
glossMoveset ms@(Moveset moves) = Pictures (board : map glossMove moves)
    where
    board = glossBoard (getRanges $ getCoords ms)

glossMove :: Move -> Picture
glossMove (Move ds) = color (makeColorI 0 0 255 120) path
    where
    vs = map ((\[a,b] -> (fromIntegral a * 20, fromIntegral b * 20)) . _delta) ds
    addToPath v p = Pictures [thickLineSeg ((0,0),v), uncurry translate v p]
    path = foldr addToPath (color red $ ThickCircle 4 2) vs

glossBoard :: [(Integer, Integer)] -> Picture
glossBoard ((minX,maxX):(minY,maxY):_) = board
    where
    board = Pictures $ concat
            $ flip map [maxY,maxY-1..minY] $ \y ->
                  flip map [minX..maxX] $ \x ->
                      tile x y
    xor x y = not $ x == y
    tile x y = Pictures [ Polygon (rectanglePath 20 20)
                            # color (tileColor x y)
                        , if x == 0 && y == 0
                             then ThickCircle 4 2
                                    # color black
                             else Blank
                        ]
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

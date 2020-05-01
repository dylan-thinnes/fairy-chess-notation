{-# LANGUAGE TemplateHaskell #-}

module Notation where

import Control.Lens.TH (makeLenses)

-- Basic data types: Delta, Move, Moveset
newtype Delta = Delta { _delta :: [Integer] }
    deriving (Eq, Ord)
newtype Move = Move { _move :: [Delta] }
    deriving (Eq, Ord)
newtype Moveset = Moveset { _moveset :: [Move] }
    deriving (Eq, Ord)

-- Turn three nested lists to a moveset
toMoveset :: [[[Integer]]] -> Moveset
toMoveset = Moveset . map (Move . map Delta)

-- Prettier output for printing
instance Show Delta where
    show (Delta xs) = show xs
instance Show Move where
    show (Move xs) = unwords $ map show xs
instance Show Moveset where
    show (Moveset xs) = unlines $ map show xs

-- Make deltas into vectors with linear addition / multiplication etc.
instance Num Delta where
    (+) (Delta xs) (Delta ys) = Delta $ zipWith (+) xs ys
    (*) (Delta xs) (Delta ys) = Delta $ zipWith (*) xs ys
    (-) (Delta xs) (Delta ys) = Delta $ zipWith (-) xs ys
    abs (Delta xs) = Delta $ map abs xs
    signum (Delta xs) = Delta $ map signum xs
    fromInteger i = Delta $ replicate 3 i

-- Generate basic lenses (Isos) for data types
makeLenses ''Delta
makeLenses ''Move
makeLenses ''Moveset

-- Parse tree for the notation
data MoveTree = Sum [MoveTree]
              | Product [MoveTree]
              | Modified MoveTree [Modifier]
              | BaseMove Delta
    deriving Show

data Modifier = Mirror Int | Swap Int Int | MirrorXY | MirrorXYSwap | Exponents [ExpGroup]
    deriving Show

data ExpGroup = Range (Maybe Int) (Maybe Int) | Single Int
    deriving Show


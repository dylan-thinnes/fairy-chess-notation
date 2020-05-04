{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveGeneric #-}

module Notation where

import Control.Lens (Iso', view, re, mapping)
import Control.Lens.TH (makeLenses)
import Data.Functor.Foldable.TH (makeBaseFunctor)

-- Basic data types: Delta, Move, Moveset
newtype Delta = Delta { _delta :: [Integer] }
    deriving (Eq, Ord)
newtype Move = Move { _move :: [Delta] }
    deriving (Eq, Ord)
newtype Moveset = Moveset { _moveset :: [Move] }
    deriving (Eq, Ord)

-- Generate basic lenses (Isos) for data types
makeLenses ''Delta
makeLenses ''Move
makeLenses ''Moveset

-- Moveset <-> [[[Integer]]] iso
msls :: Iso' Moveset [[[Integer]]]
msls = moveset . mapping (move . mapping delta)

-- Turn three nested lists to a moveset
toMoveset :: [[[Integer]]] -> Moveset
toMoveset = view $ re msls

-- Turn a moveset into three nested lists
fromMoveset :: Moveset -> [[[Integer]]]
fromMoveset = view msls

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
    fromInteger i = Delta $ repeat i

-- Parse tree for the notation
data MoveTree
    = MoveTree :+: MoveTree
    | MoveTree :*: MoveTree
    | Modified MoveTree [Modifier]
    | BaseMove Delta
    deriving Show

data Modifier = Mirror Int
              | Swap Int Int
              | MirrorXY
              | MirrorXYSwap
              | MapMultiply [Group]
              | Exponents [Group]
              | Above Int Integer
              | Below Int Integer
    deriving Show

data Group = Range (Maybe Int) (Maybe Int) | Single Int
    deriving Show

makeBaseFunctor ''MoveTree


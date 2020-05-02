{-# LANGUAGE RankNTypes #-}

module Notation.Folds where

import Notation

import Control.Lens (Fold, (%~), (.~), (&), (^?), folded, folding, mapped, toListOf, ix, filtered, anyOf)
import Data.List (nub)
import qualified Data.Semiring as S

-- Turn any function that can change a delta to a function that maps that
-- change over ever delta in a move
-- e.g. f [1,2] = [2,1] => mmap f [[3,4],[5,6]] = [[4,3],[6,5]]
mmap :: ([Integer] -> [Integer]) -> Move -> Move
mmap f = move . mapped . delta %~ f

-- Fork and forking create a Fold that splits a value a into a and f a
forking :: Eq a => (a -> a) -> Fold a a
forking = folding . fork

fork :: Eq a => (a -> a) -> a -> [a]
fork f a = nub [a, f a]

-- Convenience function for composing folds and optics with same sub as super
endo :: [a -> a] -> a -> a
endo = foldr (.) id

-- Negate the nth index of a list - serves like a mirror on a delta
-- e.g. mirror 1 [1,2,3] => [1,-2,3]
mirror :: Num a => Int -> [a] -> [a]
mirror i = ix i %~ negate

-- Swap two indices of a list
-- There's probably a nicer way to do this...
swap :: Int -> Int -> [a] -> [a]
swap i j xs
  = case (xs ^? ix i, xs ^? ix j) of
        (Just x, Just y)
          -> xs & ix i .~ y & ix j .~ x
        _ -> xs

-- The general mirror and swap modifiers, and their composition
xm, ym, zm, pm, sm, tm :: Fold Move Move
xm = forking $ mmap $ mirror 0
ym = forking $ mmap $ mirror 1
zm = forking $ mmap $ mirror 2
pm = endo [xm,ym,zm] -- "plus"
sm = forking $ mmap $ swap 0 1
tm = endo [pm,sm] -- "times"

-- The range limiting modifiers
ipred :: Int -> (Integer -> Bool) -> Fold Move Move
ipred axis p = filtered (anyOf (move . folded . delta . ix axis) p)
above :: Int -> Integer -> Fold Move Move
above axis x = ipred axis (>=x)
below :: Int -> Integer -> Fold Move Move
below axis x = ipred axis (<=(-x))

-- Expand a set of moves using a Fold over each move
expand :: Moveset -> Fold Move Move -> Moveset
expand m f = moveset %~ toListOf (folded . f) $ m
(>>>) = expand
infixl 4 >>>

-- Addition / multiplication operators for moves
one :: Moveset
one = Moveset [Move []]

zero :: Moveset
zero = Moveset []

-- Join two moves end-to-end; only used for multiplication operator
mjoin :: Move -> Move -> Move
mjoin (Move m1) (Move m2) = Move $ m1 ++ m2

-- Multiplication of movesets
(***) :: Moveset -> Moveset -> Moveset
(***) (Moveset m1) (Moveset m2) = Moveset $ mjoin <$> m1 <*> m2
infixl 4 ***

-- Addition of movesets
(+++) :: Moveset -> Moveset -> Moveset
(+++) (Moveset m1) (Moveset m2) = Moveset $ m1 ++ m2
infixr 3 +++

-- Repeat a moveset n times
(^^^) :: Moveset -> Int -> Moveset
(^^^) m1 0 = one
(^^^) m1 i = m1 *** (m1 ^^^ (i - 1))
infixl 4 ^^^

-- Repeat a moveset n times for each value in the list, then run a sum
(^^^*) :: Moveset -> [Int] -> Moveset
(^^^*) m = foldr (+++) zero . map (m ^^^)
infixl 4 ^^^*

-- Define a semiring over movesets
instance S.Semiring Moveset where
    plus  = (+++)
    times = (***)
    zero  = zero
    one   = one

-- Converter for MoveTree to Moveset
treeToSet :: MoveTree -> Moveset
treeToSet (Sum trees) = foldr (+++) zero $ map treeToSet trees
treeToSet (Product trees) = foldr (***) one $ map treeToSet trees
treeToSet (Modified tree mods) = endo (reverse $ map modToFold mods) $ treeToSet tree 
treeToSet (BaseMove delta) = Moveset [Move [delta]]

modToFold :: Modifier -> (Moveset -> Moveset)
modToFold (Mirror i)      = (>>> (forking $ mmap $ mirror i))
modToFold (Swap i j)      = (>>> (forking $ mmap $ swap i j))
modToFold (MirrorXY)      = (>>> xm . ym)
modToFold (MirrorXYSwap)  = (>>> xm . ym . sm)
modToFold (Exponents grp) = (^^^* (foldMap expGroupToList grp))
modToFold (Above axis v)  = (>>> above axis v)
modToFold (Below axis v)  = (>>> below axis v)

expGroupToList :: ExpGroup -> [Int]
expGroupToList (Range Nothing end)
  = expGroupToList (Range (Just 1) end)
expGroupToList (Range start Nothing)
  = expGroupToList (Range start (Just maxBound))
expGroupToList (Range (Just start) (Just end))
  = [start..end]
expGroupToList (Single x)
  = [x]

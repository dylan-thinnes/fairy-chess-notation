{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Notation.Unfolds where

import Notation

import Data.Maybe (fromMaybe)
import Control.Lens.TH (makeLenses)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Functor.Compose

type State = ()
data Predicate
    = Predicate
        { _name :: Maybe String
        , _doc :: Maybe String
        , _f :: State -> Bool
        }

instance Show Predicate where
    show pred = "\"" ++ (fromMaybe "no name" $ _name pred) ++ "\""

-- The high-level readable "seed" which gets unfolded to a tree of actions
data MoveSeed
  = A (Action MoveSeed)
  | Sequence MoveSeed MoveSeed
  | Repeat MoveSeed MoveSeed
  | Choice [MoveSeed]
    deriving (Show)

-- Tree of actions, represented as a fixpoint of composition of List functor with Action functor
type ActionTreeFix = Compose [] Action (MoveSeed, [MoveSeed])

-- An action or predicate on the board
data Action a
  = DeltaMove Delta a
  | Condition Predicate a
  | Continue a
  | Finish
    deriving (Show, Functor, Foldable, Traversable)

-- More readable version of ActionTreeFix - useful for traversal
data ActionTree = T [Action ActionTree]
    deriving (Show)

makeBaseFunctor ''ActionTree

makeLenses ''MoveSeed
makeBaseFunctor ''MoveSeed
makeLenses ''Action

-- Helper functions for building MoveSeeds from actions
actions :: [MoveSeed -> Action MoveSeed] -> MoveSeed -> MoveSeed
actions as seed = foldr (\act seed -> A $ act seed) seed as

finish :: MoveSeed
finish = A Finish

-- An Example
example :: MoveSeed
example
    = Choice
    [ A $ DeltaMove (Delta [1,2])
        $ Choice
        [ A $ DeltaMove (Delta [1,0]) $ A Finish
        , A $ DeltaMove (Delta [0,1]) $ A Finish
        ]
    , A $ DeltaMove (Delta [2,3]) $ A Finish
    ]

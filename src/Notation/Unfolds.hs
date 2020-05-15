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

-- The high-level readable "seed" which gets unfolded to a tree of actions
data MoveSeed
  = A (Action MoveSeed)        -- Represents an action on MoveSeed level
  | Sequence MoveSeed MoveSeed -- Sequence two MoveSeeds
  | Repeat MoveSeed MoveSeed   -- Run the first MoveSeed 0 or more times, before running the second
  | Choice [MoveSeed]          -- Present a choice between MoveSeeds
    deriving (Show)

-- An action or predicate on the board
data Action a
  = DeltaMove Delta a     -- Moves a piece using a delta
  | Condition Predicate a -- Requires a condition to be met
  | Continue a            -- Guards against infinite recursion
  | Finish                -- Finishes a move - note that the absence of "a"
                          -- guarantees this on a type level
    deriving (Show, Functor, Foldable, Traversable)

-- Predicates on game state, used in Condition actions
type State = () -- Placeholder game state for now
data Predicate
    = Predicate
        { _name :: Maybe String
        , _doc :: Maybe String
        , _f :: State -> Bool
        }

instance Show Predicate where
    show pred = "\"" ++ (fromMaybe "no name" $ _name pred) ++ "\""

-- Tree of actions, represented as a recursion of composition of List functor
-- with Action functor
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

-- Unfold a MoveSeed into a ActionTreeFix
growTree :: MoveSeed -> ActionTree
growTree seed = ana f (seed, [])
    where
    f :: (MoveSeed, [MoveSeed]) -> ActionTreeF (MoveSeed, [MoveSeed])
    f (A Finish, x : rest)        = TF [Continue (x, rest)]
    f (A action, mrest)           = TF [fmap (,mrest) action]
    f (Sequence seed next, mrest) = TF [Continue (seed, next : mrest)]
    f (Repeat   seed next, mrest) = TF [Continue (next, mrest), Continue (seed, Repeat seed next : mrest)]
    f (Choice seeds, mrest)       = TF $ map (\seed -> Continue (seed, mrest)) seeds

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

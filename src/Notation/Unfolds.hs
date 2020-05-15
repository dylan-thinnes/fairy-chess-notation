{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

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
-- with Action functor with Either MoveSeed functor
-- This allows us to only evaluate MoveSeeds to ActionTrees up to a certain depth
data ActionTree = Expanded [Action ActionTree] | Unexpanded MoveSeed
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

-- Unfold a MoveSeed into a ActionTree, with a maximum depth
type Depth = Int
growTree :: MoveSeed -> Depth -> ActionTree
growTree seed maxDepth = ana f (seed, 0, [])
    where
    f :: (MoveSeed, Int, [MoveSeed]) -> ActionTreeF (MoveSeed, Int, [MoveSeed])
    f (seed, d, rest)
      | d > maxDepth = UnexpandedF seed
      | otherwise
      = let depth = d + 1
         in case (seed, rest) of
            (A Finish, x : rest)        -> ExpandedF [ Continue (x, depth, rest) ]
            (A action, mrest)           -> ExpandedF [ fmap (,depth,mrest) action ]
            (Sequence seed next, mrest) -> ExpandedF [ Continue (seed, depth, next : mrest) ]
            (Repeat   seed next, mrest) -> ExpandedF
                                              [ Continue (next, depth, mrest)
                                              , Continue (seed, depth, Repeat seed next : mrest)
                                              ]
            (Choice seeds, mrest)       -> ExpandedF $ map (\seed -> Continue (seed, depth, mrest)) seeds

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

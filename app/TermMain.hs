module TermMain where

import Notation
import Notation.Debug
import Control.Monad (forever)

main :: IO ()
main = forever $ debug terminal

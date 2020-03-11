module Addresses where

import Trek.Monad
import Trek.Combinators
import Trek.Optics
import Control.Lens

test :: Trek [Int] (Int, Int, Int)
test = do
    selectAll (selecting (traversed . filtered even), selecting (ix 3), selecting (ix 2))


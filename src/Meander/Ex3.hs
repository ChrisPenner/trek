{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Meander.Ex3 where

import Control.Lens
import Trek.Monad
import Trek.Combinators
import Trek.Optics
import Control.Monad.State
import Control.Applicative

-- Pattern matching

-- (def point [1 2])

-- (m/find point
--   [?x ?y] ?y
--   [?x ?y ?z] ?y)
-- ;; => 2

matchY :: Trek [Int] Int
matchY = do
    get >>= \case
      [x, y] -> return y
      [x, y, z] -> return y
      _ -> empty


-- Match filtering
--(m/find point
--  [(m/pred number?) (m/pred number? ?y)]
--  ?y
--   [(m/pred number?) (m/pred number? ?y) (m/pred number?)]
--   ?y)

-- Haskell doesn't need to filter based on type, but we can use 'guard' to kill matches we
-- don't want

evenNumbers :: Trek [Int] Int
evenNumbers = do
    x <- selecting each
    guard (even x)
    return x

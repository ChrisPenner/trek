{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Meander.Ex1 where

import Control.Lens
import Trek.Monad
import Trek.Combinators
import Trek.Optics
import qualified Data.Map as M

--- Data Declarations

type FoodsByName = M.Map String (M.Map String String)
type User = M.Map String String

data UserFav =
    UserFav { _name     :: String
            , _favorite :: M.Map String String
            }
    deriving (Show, Eq)
makeFieldsNoPrefix ''UserFav

--- Value declarations

foodsByName :: FoodsByName
foodsByName =
    M.fromList [ ("nachos",   M.fromList [("popularity", "high"), ("calories", "lots")])
               , ("smoothie", M.fromList [("popularity", "high"), ("calories", "less")])
               ]

alice :: M.Map String String
alice = M.fromList [("name", "alice"), ("food", "nachos")]

--- Handy combinator

pluck :: (Ord k, Traversable f) => f k -> Trek (M.Map k a) (f a)
pluck fs = traverse (selectEach . M.lookup) fs

--- Converter
convert :: Trek (FoodsByName, User) UserFav
convert = do
    [name, food] <- using snd $ pluck ["name", "food"]
    [popularity, calories] <- usingEach (M.lookup food . fst)
        $ pluck ["popularity", "calories"]
    return
        $ UserFav name (M.fromList [ ("food", food)
                                   , ("popularity", popularity)
                                   , ("calories", calories)
                                   ])

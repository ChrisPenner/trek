{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.Meander.Ex1Map where

import Control.Lens
import Trek
import Trek.Lens
import qualified Data.Map as M

-- https://cljdoc.org/d/meander/epsilon/0.0.397/doc/readme

---Clojure

-- (defn favorite-food-info [foods-by-name user]
--   (m/match {:user user
--             :foods-by-name foods-by-name}
--     {:user
--      {:name ?name
--       :favorite-food {:name ?food}}
--      :foods-by-name {?food {:popularity ?popularity
--                             :calories ?calories}}}
--     {:name ?name
--      :favorite {:food ?food
--                 :popularity ?popularity
--                 :calories ?calories}}))

-- (def foods-by-name
--   {:nachos {:popularity :high
--             :calories :lots}
--    :smoothie {:popularity :high
--               :calories :less}})

-- (favorite-food-info foods-by-name
--   {:name :alice
--    :favorite-food {:name :nachos}})
-- ;; =>
-- {:name :alice
--  :favorite {:food :nachos
--             :popularity :high
--             :calories :lots}}


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
    [name, food] <- mount snd $ pluck ["name", "food"]
    [popularity, calories] <- mountEach (M.lookup food . fst)
        $ pluck ["popularity", "calories"]
    return
        $ UserFav name (M.fromList [ ("food", food)
                                   , ("popularity", popularity)
                                   , ("calories", calories)
                                   ])

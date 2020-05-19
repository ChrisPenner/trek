{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.Meander.Ex2 where

import Control.Lens
import Trek.Monad
import Trek.Combinators
import Trek.Lens
import qualified Data.Map as M

-- Search Queries: Clojure

-- (defn grab-all-foods [user]
--   (m/find user
--     {:favorite-foods [{:name !foods} ...]
--      :special-food !food
--      :recipes [{:title !foods} ...]
--      :meal-plan {:breakfast [{:food !foods} ...]
--                  :lunch [{:food !foods} ...]
--                  :dinner [{:food !foods} ...]}}
--     !foods))


--- Data Declarations
data Food = Food {_name :: String}
    deriving (Show)
makeLenses ''Food

data Recipe = Recipe {_title :: String}
    deriving (Show)
makeLenses ''Recipe

data MealPlan =
    MealPlan { _breakfast :: [Food]
             , _lunch     :: [Food]
             , _dinner    :: [Food]
             } deriving (Show)
makeLenses ''MealPlan

data Foods = Foods
    { _favouriteFoods :: [Food]
    , _specialFood :: String
    , _recipes :: [Recipe]
    , _mealPlan :: MealPlan
    } deriving (Show)
makeLenses ''Foods

foods :: Foods
foods = Foods
    { _favouriteFoods = [Food "Hot Dogs", Food "Burger"]
    , _specialFood = "Dumplings"
    , _recipes = [Recipe "Pho", Recipe "Steak"]
    , _mealPlan = MealPlan { _breakfast = [Food "Eggs"]
                           , _lunch = [Food "Salad"]
                           , _dinner = [Food "Soup"]
                           }
    }

-- Query
grabAllFoods :: Trek Foods String
grabAllFoods = do
        selecting (   favouriteFoods . folded . name
                  <+> specialFood
                  <+> recipes . folded . title
                  <+> mealPlan . (breakfast <+> lunch <+> dinner) . folded . name
                  )


grabAllFoods' :: Foods -> [String]
grabAllFoods' = toListOf
                  (   favouriteFoods . folded . name
                  <+> specialFood
                  <+> recipes . folded . title
                  <+> mealPlan . (breakfast <+> lunch <+> dinner) . folded . name
                  )

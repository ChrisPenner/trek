{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Meander.Ex2 where

import Control.Lens
import Trek.Monad
import Trek.Combinators
import Trek.Optics
import qualified Data.Map as M

-- (defn grab-all-foods [user]
--   (m/find user
--     {:favorite-foods [{:name !foods} ...]
--      :special-food !food
--      :recipes [{:title !foods} ...]
--      :meal-plan {:breakfast [{:food !foods} ...]
--                  :lunch [{:food !foods} ...]
--                  :dinner [{:food !foods} ...]}}
--     !foods))


data Food = Food {_name :: String}
makeLenses ''Food

data Recipe = Recipe {_title :: String}
makeLenses ''Recipe

data MealPlan =
    MealPlan { _breakfast :: [Food]
             , _lunch     :: [Food]
             , _dinner    :: [Food]
             }
makeLenses ''MealPlan

data Foods = Foods
    { _favouriteFoods :: [Food]
    , _specialFood :: String
    , _recipes :: [String]
    , _mealPlan :: MealPlan
    }
makeLenses ''Foods

grabAllFoods :: Trek Foods [String]
grabAllFoods = do
    collectList $ do
        selecting (   favouriteFoods . folded . name
                  <+> specialFood
                  <+> recipes . folded
                  <+> mealPlan . (breakfast <+> lunch <+> dinner) . folded . name
                  )


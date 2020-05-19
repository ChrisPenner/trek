{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.Meander.Ex1 where

import Control.Lens
import Trek.Monad
import Trek.Combinators
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

data Food =
    Food { _popularity :: String
         , _calories   :: String
         }
    deriving (Show, Eq)
makeFieldsNoPrefix ''Food

type FoodsByName = M.Map String Food
data User =
    User { _name    :: String
         , _favFood :: String
         }
    deriving (Show, Eq)
makeFieldsNoPrefix ''User

data Favourite =
    Favourite { _food       :: String
              , _popularity :: String
              , _calories   :: String
              }
    deriving (Show, Eq)
makeFieldsNoPrefix ''Favourite

data UserFav =
    UserFav { _name     :: String
            , _favorite :: Favourite
            }
    deriving (Show, Eq)
makeFieldsNoPrefix ''UserFav

--- Value declarations

foodsByName :: FoodsByName
foodsByName =
    M.fromList [ ("nachos", Food "high" "lots")
               , ("smoothie", Food "high" "less")
               ]

alice :: User
alice = User "alice" "nachos"

--- Converter

convert :: Trek (FoodsByName, User) UserFav
convert = do
    User name food <- select snd
    Food pop cal   <- selectEach (M.lookup food . fst)
    return $ UserFav name (Favourite food pop cal)

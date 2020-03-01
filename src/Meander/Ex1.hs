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

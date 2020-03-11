{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
module Trek
    ( Trek(..)
    , select
    , selectEach
    , iter
    , collectList
    , using
    , usingEach
    , fill
    , with
    , withEach
    , evalTrek
    , evalTrek1
    , execTrek
    , execTrek1
    , runTrek
    , runTrek1
    ) where

import Control.Monad.State
import Control.Monad.Cont
import Control.Lens
import Data.Aeson hiding ((.=))
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens
import qualified Data.Map as M
import Text.RawString.QQ (r)
import Data.Text as T
import Data.Foldable
import Data.Semigroup
import qualified Data.ByteString.Lazy.Char8 as BL
import Trek.Monad
import Trek.Combinators
import Trek.Optics
import Trek.HKD

-- val :: Value
-- val = view @T.Text (singular _JSON) [r|
-- {
--   "users": [
--     {
--       "name": "Bob",
--       "favoriteFood": "apple"
--     },
--     {
--       "name": "Cindy",
--       "favoriteFood": "cake"
--     }
--   ],
--   "foodsByName": {
--     "apple": {
--       "category": "fruit"
--     },
--     "cake": {
--       "category": "junk"
--     }
--   }
-- }
--     |]

-- peeps :: Value
-- peeps = view @T.Text (singular _JSON) [r|
-- {
--   "users": [
--     {
--       "firstName": "Mal",
--       "lastName": "Reynolds"
--     },
--     {
--       "firstName": "Hoban",
--       "lastName": "Washburne"
--     },
--     {
--       "firstName": "Kaywinnet",
--       "lastName": "Frye"
--     }
--   ],
--   "occupations" : {
--     "Mal Reynolds": "Captain",
--     "Hoban Washburne": "Pilot",
--     "Kaywinnet Frye": "Mechanic"
--   }
-- }
--     |]


-- addFullName :: Trek Value ()
-- addFullName = do
--     occupations <- selecting (key "occupations")
--     focusing (key "users" . values) $ do
--         firstName <- selecting (key "firstName" . _String)
--         lastName <- selecting (key "lastName" . _String)
--         let fullName = firstName <> " " <> lastName
--         _Object . at "fullName" . non "" . _String .= fullName
--         _Object . at "job" . non "" <~ selectingFrom occupations (key fullName)


-- testEval :: ToJSON a => Trek s a -> s -> IO ()
-- testEval exp s = BL.putStrLn . encodePretty $ evalTrek exp s
-- testExec :: ToJSON s => Trek s a -> s -> IO ()
-- testExec exp s = BL.putStrLn . encodePretty $ execTrek exp s

-- exampleB :: Trek Value Value
-- exampleB = do
--     foods <- selecting (key "foodsByName")
--     withEachOf (key "users" . values) . fmap toJSON . runHKD $ do
--         FoodMap
--             { favoriteFood = Food
--                   { food = selecting (key "favoriteFood" . _String)
--                   , category = do
--                         userFood <- selecting (key "favoriteFood" . _String)
--                         with foods $ selecting (ix userFood . key "category" . _String)
--                   }
--             , name = selecting (key "name" . _String)
--             }

-- example :: Trek Value [M.Map T.Text Value]
-- example = collectList $ do
--     foods <- selecting (key "foodsByName")
--     withEachOf (key "users" . values) $ do
--         userFavFood <- selecting (key "favoriteFood")
--         collectMap $ [ ("name", selecting (key "name"))
--                      , ("favoriteFood", fmap toJSON . collectMap $
--                                       [ ("food" :: T.Text , return userFavFood)
--                                       , ("category", do
--                                            userFood <- selecting (key "favoriteFood" . _String)
--                                            with foods $ do
--                                              selecting (ix userFood . key "category")
--                                         )
--                                       ]
--                          )
--                        ]




-- -- example :: Explorer Value ()
-- -- example = do
-- --     foods <- use foodsByName
-- --     collectList $ do
-- --         user <- flatten users
-- --         with user $ do
-- --         result <- sequence' . M.fromList
-- --                    $ [ ("name": use name)
-- --                      , ("category": do
-- --                          userFood <- use favoriteFood
-- --                          with foods
-- --                          use (ix userFood . category)
-- --                        )]




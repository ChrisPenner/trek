{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Trek where

import Control.Monad.State
import Control.Monad.Cont
import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens
import qualified Data.Map as M
import Text.RawString.QQ (r)
import Data.Text as T
import Data.Foldable
import Data.Semigroup
import qualified Data.ByteString.Lazy.Char8 as BL
import Trek.Monad

val :: Value
val = view @T.Text (singular _JSON) [r|
{
  "users": [
    {
      "name": "Bob",
      "favoriteFood": "apple"
    },
    {
      "name": "Cindy",
      "favoriteFood": "cake"
    }
  ],
  "foodsByName": {
    "apple": {
      "category": "fruit"
    },
    "cake": {
      "category": "junk"
    }
  }
}
    |]


-- sequence' :: f (Explorer r s a) -> Explorer r s (f a)

test :: ToJSON a => Trek s a -> s -> IO ()
test exp s = BL.putStrLn . encodePretty $ runTrek1 exp s

example :: Trek Value [M.Map T.Text Value]
example = collectList $ do
    foods <- fetch (key "foodsByName")
    withEachOf (key "users" . values) $ do
    userFavFood <- fetch (key "favoriteFood")
    collectMap $ [ ("name", fetch (key "name"))
                 , ("favoriteFood", fmap toJSON . collectMap @T.Text $
                                  [ ("food", return $ userFavFood)
                                  , ("category", do
                                       userFood <- fetch (key "favoriteFood" . _String)
                                       with foods $ do
                                       fetch (ix userFood . key "category")
                                    )
                                  ]
                     )
                   ]




-- example :: Explorer Value ()
-- example = do
--     foods <- use foodsByName
--     collectList $ do
--         user <- flatten users
--         with user $ do
--         result <- sequence' . M.fromList
--                    $ [ ("name": use name)
--                      , ("category": do
--                          userFood <- use favoriteFood
--                          with foods
--                          use (ix userFood . category)
--                        )]




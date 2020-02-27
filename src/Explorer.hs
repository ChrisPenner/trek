{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Explorer where

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

newtype Explorer r s a = Explorer (ContT r (State s) a)
  deriving newtype (Functor, Applicative, Monad)


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

fetch :: Monoid r => Fold s a -> Explorer r s a
fetch fld = Explorer . ContT $ \cont -> do
   as <- gets (toListOf fld)
   rs <- traverse cont as
   return $ fold rs

collectList :: forall r s a. Explorer [a] s a -> Explorer r s [a]
collectList (Explorer r) =
    Explorer . ContT $ \cont -> do
        as <- p
        cont as
  where
    p :: State s [a]
    p = runContT r (return . pure)

flatten :: Monoid r => Fold s a -> Explorer r s a
flatten = fetch

flatten' :: Monoid r => [a] -> Explorer r s a
flatten' xs = Explorer . ContT $ \cont ->
    do
        as <- traverse cont xs
        return (fold as)

with :: s -> Explorer [r] s r -> Explorer r' t [r]
with s exp = return $ runExplorerWith (pure :: x -> [x]) exp s

withAll :: Monoid r' => s -> Explorer [r] s r -> Explorer r' t r
withAll s exp = do
    xs <- with s exp
    flatten' xs

with' :: (a -> r) -> s -> Explorer r s a -> Explorer r' t r
with' f s exp = return $ runExplorerWith f exp s

runExplorer :: Explorer a s a -> s -> a
runExplorer (Explorer c) s = flip evalState s $ runContT c pure

runExplorerWith :: (a -> r) -> Explorer r s a -> s -> r
runExplorerWith f (Explorer c) s = flip evalState s $ runContT c (pure . f)

test :: ToJSON a => Explorer a s a -> s -> IO ()
test exp s = BL.putStrLn . encodePretty $ runExplorer exp s

-- sequence' :: f (Explorer r s a) -> Explorer r s (f a)


example :: Monoid r => Explorer r Value [M.Map T.Text Value]
example = do
    foods <- fetch (key "foodsByName")
    user <- flatten (key "users" . values)
    with user $ do
    userFavFood <- fetch (key "favoriteFood")
    sequenceA . M.fromList
                $ [ ("name", fetch (key "name"))
                  , ("favoriteFood", fmap toJSON . sequenceA $
                        M.fromList @T.Text [ ("food", return $ userFavFood)
                                   , ("category", do
                                        userFood <- fetch (key "favoriteFood" . _String)
                                        withAll foods $ do
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




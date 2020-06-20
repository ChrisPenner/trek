{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Examples.Simple.JSON where

import Control.Lens
import Control.Monad.State
import Text.RawString.QQ (r)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Applicative
import Data.Foldable

-- No need for anything tricky
names :: Value -> [T.Text]
names = toListOf (key "items" . values . key "spec" . key "containers" . values . key "name" . _String)

addImageToName :: State Value ()
addImageToName = do
    zoom (key "items" . values . key "spec" . key "containers" . values) $ do
        image <- use (key "image" . _String)
        key "name" . _String %= \n -> (image <> "-" <> n)

selecting :: (Alternative m, Monad m) => Traversal' s a -> StateT s m a
selecting t = gets (toListOf t) >>= lift . asum . fmap pure

normalizeContainers :: StateT Value Maybe Value
normalizeContainers = do
    fmap toJSON . zoom (key "items" . values . key "spec" . key "containers" . values) $ do
        container <- get
        name <- selecting (key "name" . _String)
        return $ M.singleton name container

k8s :: Value
k8s = view @T.Text (singular _JSON) [r|
{
    "kind": "List",
    "apiVersion": "v1",
    "items": [
        {
            "kind": "Pod",
            "apiVersion": "v1",
            "metadata": {
                "name": "redis-h315w",
                "creationTimestamp": "2019-03-23T19:42:21Z",
                "labels": {
                    "name": "redis",
                    "region": "usa"
                }
            },
            "spec": {
                "containers": [
                    {
                        "name": "cache",
                        "image": "redis",
                        "ports": [
                            {
                                "name": "redis",
                                "hostPort": 27017,
                                "containerPort": 27017,
                                "protocol": "TCP"
                            }
                        ],
                        "resources": {
                            "requests": {
                                "cpu": "100m"
                            }
                        }
                    }
                ]
            }
        },
        {
            "kind": "Pod",
            "apiVersion": "v1",
            "metadata": {
                "name": "web-4c5bj",
                "creationTimestamp": "2019-02-24T20:23:56Z",
                "labels": {
                    "name": "web",
                    "region": "usa"
                }
            },
            "spec": {
                "containers": [
                    {
                        "name": "web",
                        "image": "server",
                        "ports": [
                            {
                                "name": "http-server",
                                "containerPort": 3000,
                                "protocol": "TCP"
                            }
                        ],
                        "resources": {
                            "requests": {
                                "cpu": "100m"
                            }
                        }
                    }
                ]
            }
        }
    ]
}
|]

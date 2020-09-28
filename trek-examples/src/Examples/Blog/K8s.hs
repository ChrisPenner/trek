{-# LANGUAGE OverloadedStrings #-}
module K8s where

import Data.Aeson hiding ((.=))
import Data.Aeson.Lens
import Control.Lens
import Control.Monad.State
import System.IO.Unsafe
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Foldable

info :: Value
info = unsafePerformIO $ do
    readFile "./k8s.json" <&> view (singular _JSON)

transformation :: M.Map T.Text Int -> State Value (M.Map T.Text T.Text)
transformation ports = do
    zoom (key "items" . values) $ do
        containerImages <- zoom (key "spec" . key "containers" . values) $ do
            containerName <- use (key "name" . _String)
            imageName <- use (key "image" . _String . to (T.takeWhile (/= ':')))
            zoom (key "ports" . values) $ do
                let hostPort = M.findWithDefault 8080 imageName ports
                key "hostPort" . _Integral .= hostPort
                key "containerPort" . _Integral .= hostPort + 1000
            return $ M.singleton containerName imageName
        zoom (key "metadata" . key "labels") $ do
          for_ containerImages $ \imageName ->
              _Object . at imageName ?= "true"
        return containerImages

imagePorts :: M.Map T.Text Int
imagePorts = M.fromList [ ("redis", 6379)
                        , ("my-app", 80)
                        , ("postgres", 5432)
                        ]

result :: (M.Map T.Text T.Text, Value)
result = runState (transformation imagePorts) info

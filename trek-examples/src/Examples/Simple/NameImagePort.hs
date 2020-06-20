{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Examples.Simple.NameImagePort where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Aeson
import Data.Either
import Data.Aeson.Lens
import qualified Data.List as L
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Lens

main :: IO ()
main = do
    k8s <- eitherDecodeFileStrict' "./src/Examples/Simple/k8s.json" >>= either fail pure
    print (nameImagePort' k8s)

nameImagePort :: Value -> [T.Text]
nameImagePort = do
    magnify (key "items" . values . key "spec" . key "containers" . values) $ do
        name <- view (key "name" . _String)
        image <- view (key "image" . _String)
        port <- view (key "ports" . nth 0 . key "containerPort" . _Integer . re _Show . packed)
        return . (:[]) . T.intercalate "-" $ [name, image, port]


nameImagePort' :: Value -> [T.Text]
nameImagePort' = do
    view $ key "items" . values . key "spec" . key "containers" . values . to
      (pure . foldMap view [(key "name" . _String), (key "image" . _String), (key "ports" . nth 0 . key "containerPort" . _Integer . re _Show . packed)])



line :: T.Text -> ReaderT Value (Writer T.Text) ()
line = tell . (<> "\n")

-- viewList' :: (MonadReader s m, MonadReader s n) => Traversal' s s -> n a -> m a
-- viewList' t n = magnify t n


runReport :: IO ()
runReport = do
    k8s <- eitherDecodeFileStrict' "./src/Examples/Simple/k8s.json" >>= either fail pure
    T.putStrLn (execWriter $ runReaderT report k8s)

indented :: MonadWriter T.Text m => m a -> m a
indented = censor (T.unlines . fmap ("\t" <>) . T.lines)

report :: ReaderT Value (Writer T.Text) ()
report = do
    line "Let me tell you about my k8s setup:"
    line "I have some pods"
    indented $ magnify (key "items" . values) $ do
        podName <- view (key "metadata" . key "name" . _String)
        line $ "Pod " <> podName <> " has the following containers:"
        indented $ magnify (key "spec" . key "containers" . values) $ do
            containerName <- view (key "name" . _String)
            ports <- view (key "ports" . nth 0 . key "containerPort" . _Integer . to (:[]) . re _Show . packed)
            line . T.unwords $ ["-", containerName, ports]
    line "that's all!"

runMutation :: IO ()
runMutation = do
    k8s <- eitherDecodeFileStrict' "./src/Examples/Simple/k8s.json" >>= either fail pure
    print $ (execState mutation k8s)

-- Add pod name to container name
-- multiply port number by 10
mutation :: State Value ()
mutation = do
    zoom (key "items" . values) $ do
        podName <- use (key "metadata" . key "name" . _String)
        zoom (key "spec" . key "containers" . values) $ do
            key "name" . _String %= (\n -> podName <> "-" <> n)
            key "ports" . values . key "containerPort" . _Integer *= 10

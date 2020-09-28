{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module CSV where

import Control.Lens
import Data.Csv.Lens
import qualified Data.ByteString.Lazy as BL
import Control.Monad.State

import System.IO.Unsafe
doc :: BL.ByteString
doc = unsafePerformIO $ do
    BL.readFile "./doc.csv"

recomputeAges :: State BL.ByteString ()
recomputeAges = do
    zoom (namedCsv . rows) $ do
        preuse (column @Int "birthYear") >>= \case
            Nothing -> return ()
            Just birthYear -> do
                column @Int "age" .= 2020 - birthYear

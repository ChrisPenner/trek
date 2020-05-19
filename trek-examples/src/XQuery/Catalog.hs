{-# LANGUAGE OverloadedStrings #-}
module XQuery.Catalog where

import Control.Lens hiding (elements)
import Trek.Monad
import Trek.Combinators
import Trek.Lens
import qualified Data.Map as M
import Data.Text.Lazy.IO as TIO
import Data.Text.Lazy as T
import Text.Taggy.Lens
import Control.Monad


catalogEx :: IO ()
catalogEx = do
    src <- TIO.readFile "src/XQuery/catalog.xml"
    print $ evalTrek handler src

handler :: Trek T.Text Element
handler = do
    parsed <- selecting html
    product <- selectingFrom parsed (allNamed (only "product"))
    dept <- selectingFrom product (attr "dept" . _Just)
    guard (dept  `elem` ["ACC", "WMN"])
    selectingFrom product (allNamed (only "name"))


    -- withEachOf (allNamed (only "products")) $ do
    -- dept <- selecting (attr "dept" . _Just)
    -- guard (dept  `elem` ["ACC", "WMN"])
    -- selecting (allNamed (only "named"))

-- for $prod in doc("catalog.xml")//product
-- let $prodDept := $prod/@dept
-- where $prodDept = "ACC" or $prodDept = "WMN"
-- return $prod/name

{-# LANGUAGE OverloadedStrings #-}
module XQuery.Catalog where

import Control.Lens hiding (elements)
import Trek
import Trek.Lens
import qualified Data.Map as M
import Data.Text.Lazy.IO as TIO
import Data.Text.Lazy as T
import Text.Taggy.Lens
import Control.Monad


catalogEx :: IO ()
catalogEx = do
    src <- TIO.readFile "src/XQuery/catalog.xml"
    runTrekT handler' src >>= print

-- http://www.datypic.com/books/xquery/chapter06.html
handler :: TrekT T.Text IO Element
handler = do
    parsed <- selecting html
    product <- iter $ parsed ^.. allNamed (only "product")
    dept <- iter $ product ^.. attr "dept" . _Just
    guard (dept  `elem` ["ACC", "WMN"])
    iter $ product ^.. allNamed (only "name")


handler' :: TrekT T.Text IO Element
handler' = do
    mounting (html . allNamed (only "product")) $ do
      dept <- selecting (attr "dept" . _Just)
      guard (dept  `elem` ["ACC", "WMN"])
      selecting (allNamed (only "name"))


-- for $prod in doc("catalog.xml")//product
-- let $prodDept := $prod/@dept
-- where $prodDept = "ACC" or $prodDept = "WMN"
-- return $prod/name

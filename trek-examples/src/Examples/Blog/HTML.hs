{-# LANGUAGE OverloadedStrings #-}
module HTML where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Control.Monad.State
import Text.Taggy.Lens
import Control.Lens hiding (elements)

import System.IO.Unsafe
haskellHTML :: TL.Text
haskellHTML = unsafePerformIO $ do
    TL.readFile "./haskell.html"

transformation :: State TL.Text [T.Text]
transformation = do
    -- Select all tags which have an "img" as a direct child
    zoom (html . elements . deep (filteredBy (elements . named (only "img")))) $ do
        -- Get the current node's text contents
        altText <- use contents
        -- Set the text contents as the "alt" tag for all img children
        elements . named (only "img") . attr "alt" ?= altText

    -- Transform all "a" tags recursively
    (html . elements . transformM . named (only "a")) 
      -- Wrap them in a <strong> tag while also returning their href value
      %%= \tag -> (tag ^.. attr "href" . _Just, Element "strong" mempty [NodeElement tag])

result :: ([T.Text], TL.Text)
result = runState transformation haskellHTML

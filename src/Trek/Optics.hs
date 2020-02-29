{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Trek.Optics where

import Control.Lens
import Control.Monad.State
import Control.Monad.Logic
import Trek.Monad
import Trek.Combinators
import Data.Foldable

selectEachOf :: Fold s a -> Trek s a
selectEachOf fld = gets (toListOf fld) >>= iter

withEachOf :: Fold t s -> Trek s a -> Trek t a
withEachOf fld exp = do
    xs <- collectList (selectEachOf fld)
    withEach xs exp

infixr 4 %>
-- | Zoom a trek through a traversal
(%>) :: forall s t a. Traversal' s t -> Trek t a -> Trek s a
(%>) = using

infixr 4 `using`
-- | Zoom a trek through a traversal
using :: forall s t a. Traversal' s t -> Trek t a -> Trek s a
using trav (Trek logt) = do
    let st = observeAllT logt
    let zst :: State s [a] = zoom trav st
    xs <- Trek (lift zst)
    iter xs

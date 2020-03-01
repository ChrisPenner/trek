{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Trek.Optics where

import Control.Lens
import Control.Monad.State
import Control.Monad.Logic
import Trek.Monad
import Trek.Combinators
import Data.Foldable

selecting :: Fold s a -> Trek s a
selecting fld = selectEach (toListOf fld)

selectingFrom :: s -> Fold s a -> Trek t a
selectingFrom s fld = with s $ selecting fld

selectAll :: Each x y (Trek s b) b => x -> Trek s y
selectAll = sequenceAOf each

withEachOf :: Fold t s -> Trek s a -> Trek t a
withEachOf fld exp = do
    xs <- collectList (selecting fld)
    withEach xs exp

infixr 4 %>
-- | Zoom a trek through a traversal
(%>) :: forall s t a. Traversal' s t -> Trek t a -> Trek s a
(%>) = focusing

infixr 4 `focusing`
-- | Zoom a trek through a traversal
focusing :: forall s t a. Traversal' s t -> Trek t a -> Trek s a
focusing trav (Trek logt) = do
    let st = observeAllT logt
    let zst :: State s [a] = zoom trav st
    xs <- Trek (lift zst)
    iter xs

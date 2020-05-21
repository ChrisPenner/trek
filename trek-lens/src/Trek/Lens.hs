{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Trek.Lens (
    selecting
    , mounting
    , focusing
    , (%>)
    )
    where

import Control.Lens
import Trek
import Control.Monad.State
import Control.Monad.Logic

-- infixr 4 <+@>
-- (<+@>) :: (Indexable i p, Contravariant f, Applicative f) => IndexedFold i s a -> IndexedFold i s a -> p a (f a) -> s -> f s
-- fldA <+@> fldB = conjoined (fldA <+> fldB) (ifolding (\s -> (s ^@.. fldA) <> (s ^@.. fldB)))

-- infixr 4 <+>
-- -- | (<+>) allows you to append multiple folds into one
-- (<+>) :: Fold s a -> Fold s a -> Fold s a
-- fldA <+> fldB = (folding (\s -> (s ^.. fldA) <> (s ^.. fldB)))

-- | Optical version of 'select'/'selectEach'. Iterates over all result(s) of the provided optic in the structure.
-- Accepts a Getter, Traversal, Prism, Iso or Fold.
selecting :: Monad m => Fold s a -> TrekT s m a
selecting fld = selectEach (toListOf fld)

-- | Allows sequencing a tuple or list of Trek blocks into the values that they return.
-- selectAll :: Monad m => Each x y (TrekT s m b) b => x -> TrekT s m y
-- selectAll = sequenceAOf each

-- | The optical version of 'mount'/'mountEach'. Runs a 'TrekT' block over each focus of
-- the provided optic.
-- All state updates are discarded.
mounting :: Monad m => Fold t s -> TrekT s m a -> TrekT t m a
mounting fld exp = do
    xs <- collect (selecting fld)
    withEach xs exp

infixr 4 `focusing`

-- | Run a 'TrekT' block over a subset of your state. Unlike 'using', updates to the state
-- are KEPT.
focusing :: forall s t m a. (Monad m) => Traversal' s t -> TrekT t m a -> TrekT s m a
focusing trav (TrekT logt) = do
    let st = observeAllT logt
    let zst :: StateT s m [a] = zoom trav st
    xs <- TrekT (lift zst)
    iter xs

-- Infix alias for 'focusing'
infixr 4 %>
-- | Zoom a trek through a traversal
(%>) :: forall m s t a. Monad m => Traversal' s t -> TrekT t m a -> TrekT s m a
(%>) = focusing

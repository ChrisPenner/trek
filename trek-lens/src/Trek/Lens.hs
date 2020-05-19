{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Trek.Lens where

import Control.Lens
import Trek
import Control.Monad.State
import Control.Monad.Logic

infixr 4 <+@>
(<+@>) :: (Indexable i p, Contravariant f, Applicative f) => IndexedFold i s a -> IndexedFold i s a -> p a (f a) -> s -> f s
fldA <+@> fldB = conjoined (fldA <+> fldB) (ifolding (\s -> (s ^@.. fldA) <> (s ^@.. fldB)))

infixr 4 <+>
-- | (<+>) allows you to append multiple folds into one
(<+>) :: Fold s a -> Fold s a -> Fold s a
fldA <+> fldB = (folding (\s -> (s ^.. fldA) <> (s ^.. fldB)))

selecting :: Monad m => Fold s a -> TrekT s m a
selecting fld = selectEach (toListOf fld)

selectingFrom :: Monad m => s -> Fold s a -> TrekT t m a
selectingFrom s fld = with s $ selecting fld

selectAll :: Monad m => Each x y (TrekT s m b) b => x -> TrekT s m y
selectAll = sequenceAOf each

withEachOf :: Monad m => Fold t s -> TrekT s m a -> TrekT t m a
withEachOf fld exp = do
    xs <- collectList (selecting fld)
    withEach xs exp

infixr 4 %>
-- | Zoom a trek through a traversal
(%>) :: forall m s t a. Monad m => Traversal' s t -> TrekT t m a -> TrekT s m a
(%>) = focusing

infixr 4 `focusing`
-- | Zoom a trek through a traversal
focusing :: forall s t m a. (Monad m) => Traversal' s t -> TrekT t m a -> TrekT s m a
focusing trav (TrekT logt) = do
    let st = observeAllT logt
    let zst :: StateT s m [a] = zoom trav st
    xs <- TrekT (lift zst)
    iter xs

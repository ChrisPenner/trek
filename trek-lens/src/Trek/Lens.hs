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
(<+>) :: Fold s a -> Fold s a -> Fold s a
fldA <+> fldB = (folding (\s -> (s ^.. fldA) <> (s ^.. fldB)))


-- testing :: String
-- testing = ['a', 'b'] ^.. (folded <+> folded)

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
focusing trav (TrekT logt) = do
    let st = observeAllT logt
    let zst :: State s [a] = zoom trav st
    xs <- TrekT (lift zst)
    iter xs

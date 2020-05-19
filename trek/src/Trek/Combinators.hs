{-# LANGUAGE RankNTypes #-}
module Trek.Combinators where

import Trek.Monad
import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative
import Control.Monad.Logic
import Data.Foldable

select :: (s -> a) -> Trek s a
select getter = gets getter

selectEach :: Foldable f => (s -> f a) -> Trek s a
selectEach getter = gets getter >>= iter

iter :: Foldable f => f a -> Trek s a
iter = asum . fmap pure . toList

collectList :: Trek s a -> Trek s [a]
collectList trek = do
    s <- get
    return $ evalTrek trek s

using :: (t -> s) -> Trek s a -> Trek t a
using f trek = do
    s <- select f
    with s trek

usingEach :: Foldable f => (t -> f s) -> Trek s a -> Trek t a
usingEach f trek = do
    s <- selectEach f
    with s trek

fill :: Functor f => f (s -> a) -> Trek s (f a)
fill fs = do
    s <- get
    return $ fmap ($ s) fs

with :: s -> Trek s a -> Trek t a
with s trek =
    let xs = evalTrek trek s
     in iter xs

withEach :: Foldable f => f s -> Trek s a -> Trek t a
withEach xs trek =
    iter xs >>= flip with trek

evalTrek :: Trek s a -> s -> [a]
evalTrek t s = fst $ runTrek t s

evalTrek1 :: Trek s a -> s -> a
evalTrek1 t s = fst $ runTrek1 t s

execTrek :: Trek s a -> s -> s
execTrek t s = snd $ runTrek t s

execTrek1 :: Trek s a -> s -> s
execTrek1 t s = snd $ runTrek1 t s

runTrek :: Trek s a -> s -> ([a], s)
runTrek (TrekT logt) s = flip runState s $ observeAllT logt

runTrek1 :: Trek s a -> s -> (a, s)
runTrek1 (TrekT logt) s = flip runState s $ observeT logt

runTrekT :: Monad m => TrekT s m a -> s -> m ([a], s)
runTrekT (TrekT m) s = flip runStateT s $ observeAllT m

runTrekT1 :: Monad m => TrekT s m a -> s -> m (a, s)
runTrekT1 (TrekT m) s = flip runStateT s $ observeT m


collectMap :: forall k v s. Ord k => [(k, Trek s v)] -> Trek s (M.Map k v)
collectMap = sequenceA . M.fromList

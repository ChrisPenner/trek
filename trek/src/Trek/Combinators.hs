{-# LANGUAGE RankNTypes #-}
module Trek.Combinators where

import Trek.Monad
import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative
import Control.Monad.Logic
import Data.Foldable

select :: Monad m => (s -> a) -> TrekT s m a
select getter = gets getter

selectEach :: (Monad m, Foldable f) => (s -> f a) -> TrekT s m a
selectEach getter = gets getter >>= iter

iter :: Foldable f => f a -> TrekT s m a
iter = asum . fmap pure . toList

collectList :: Monad m => TrekT s m a -> TrekT s m [a]
collectList trek = do
    s <- get
    lift . fmap fst $ runTrekT trek s

using :: Monad m => (t -> s) -> TrekT s m a -> TrekT t m a
using f trek = do
    s <- select f
    with s trek

usingEach :: (Monad m, Foldable f) => (t -> f s) -> TrekT s m a -> TrekT t m a
usingEach f trek = do
    s <- selectEach f
    with s trek

-- fill :: (Monad m, Functor f) => f (s -> a) -> TrekT s m (f a)
-- fill fs = do
--     s <- get
--     return $ fmap ($ s) fs

with :: Monad m => s -> TrekT s m a -> TrekT t m a
with s trek = do
    (xs, _) <- lift $ runTrekT trek s
    iter xs

withEach :: (Monad m, Foldable f) => f s -> TrekT s m a -> TrekT t m a
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

-- collectMap :: forall k m v s. Ord k => [(k, TrekT s m v)] -> TrekT s m (M.Map k v)
-- collectMap = sequenceA . M.fromList

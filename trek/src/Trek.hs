{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
module Trek
    (

    -- * Types
    TrekT(..)
    , Trek

    -- * Combinators
    , select
    , selectEach
    , iter
    , collect
    , mount
    , mountEach
    , with
    , withEach

    -- * Running Trek
    , evalTrek
    , evalTrek1
    , evalTrekT
    , evalTrekT1
    , execTrek
    , execTrekT
    , runTrek
    , runTrek1
    , runTrekT
    , runTrekT1

    -- * Re-Exports
    , get
    , gets
    , put
    , modify
    , ask
    , asks
    ) where

import Control.Applicative
import Control.Monad.Fail
import Control.Monad.Identity
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid

-- | Pure Trek Monad
type Trek s a = TrekT s Identity a

-- | The Trek Monad Transformer.
-- Implements both MonadReader and MonadState.
newtype TrekT s m a = TrekT (StateT s (LogicT m) a)
  deriving newtype (Functor, Applicative, Monad, MonadState s, Alternative, MonadFail)
  deriving (Semigroup, Monoid) via Ap (StateT s (LogicT m)) a

instance MonadTrans (TrekT s) where
  lift m = TrekT (lift . lift $ m)

instance (Monad m) => MonadReader s (TrekT s m) where
  ask = get
  local f m = do
      s <- get
      modify f
      a <- m
      put s
      pure a


-- | Get a value from your state
select :: Monad m => (s -> a) -> TrekT s m a
select getter = gets getter

-- | Iterate over several values from your state. An alias for @'select' >=> 'iter'@
selectEach :: (Monad m, Foldable f) => (s -> f a) -> TrekT s m a
selectEach getter = select getter >>= iter

-- | Iterate over each of the provided values one at a time.
iter :: Foldable f => f a -> TrekT s m a
iter = asum . fmap pure . toList

-- | Run a full 'TrekT' block collecting all results into a list
collect :: Monad m => TrekT s m a -> TrekT s m [a]
collect trek = do
    s <- get
    lift (fmap fst <$> runTrekT trek s)

-- | Run a 'TrekT' block over a portion of state. All state changes from the block are
-- discarded.
mount :: Monad m => (t -> s) -> TrekT s m a -> TrekT t m a
mount f trek = do
    s <- select f
    with s trek

-- | Like 'mount' but focuses each of a list of values one at a time.
-- All state changes from the block are discarded.
mountEach :: (Monad m, Foldable f) => (t -> f s) -> TrekT s m a -> TrekT t m a
mountEach f trek = do
    s <- selectEach f
    with s trek

-- | Run a 'TrekT' block over a piece of state.
-- All state changes from the block are discarded.
with :: Monad m => s -> TrekT s m a -> TrekT t m a
with s = mount (const s)

-- | Like 'with' but focuses each of a list of values one at a time.
-- All state changes from the block are discarded.
withEach :: (Monad m, Foldable f) => f s -> TrekT s m a -> TrekT t m a
withEach xs trek =
    iter xs >>= flip with trek

-- | Evaluate the results of a 'Trek'.
evalTrek :: Trek s a -> s -> [a]
evalTrek t s = runIdentity $ evalTrekT t s

-- | Evaluate the first result of a 'Trek'.
evalTrek1 :: Trek s a -> s -> a
evalTrek1 t s = runIdentity $ evalTrekT1 t s

-- | Evaluate the results of a 'TrekT'.
evalTrekT :: Monad m => TrekT s m a -> s -> m [a]
evalTrekT t s = fmap fst <$> runTrekT t s

-- | Evaluate the first result of a 'TrekT'.
evalTrekT1 :: Monad m => TrekT s m a -> s -> m a
evalTrekT1 t s = fst <$> runTrekT1 t s

-- | Return the altered state after running a 'Trek'.
execTrek :: Trek s a -> s -> [s]
execTrek t s = runIdentity $ execTrekT t s

-- | Return the altered state after running a 'TrekT'.
execTrekT :: Monad m => TrekT s m a -> s -> m [s]
execTrekT t s = fmap snd <$> runTrekT t s

-- | Run a 'Trek'
runTrek :: Trek s a -> s -> [(a, s)]
runTrek t s = runIdentity $ runTrekT t s

-- | Run to get the first result of a 'Trek'
runTrek1 :: Trek s a -> s -> (a, s)
runTrek1 t s = runIdentity $ runTrekT1 t s

-- | Run a 'TrekT'
runTrekT :: Monad m => TrekT s m a -> s -> m [(a, s)]
runTrekT (TrekT m) s = observeAllT . flip runStateT s $ m

-- | Run to get the first result of a 'TrekT'
runTrekT1 :: Monad m => TrekT s m a -> s -> m (a, s)
runTrekT1 (TrekT m) s = observeT . flip runStateT s $ m

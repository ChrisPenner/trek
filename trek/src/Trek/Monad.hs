{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}

module Trek.Monad where

import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Fail
import Control.Monad.Identity
import Data.Monoid

type Trek s a = TrekT s Identity a

newtype TrekT s m a = TrekT (LogicT (StateT s m) a)
  deriving newtype (Functor, Applicative, Monad, MonadState s, Alternative, MonadFail)
  deriving (Semigroup, Monoid) via Ap (LogicT (StateT s m)) a

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

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Trek.Monad where

import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Cont
import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Lens
import qualified Data.Map as M
import Text.RawString.QQ (r)
import Data.Text as T
import Data.Foldable
import Data.Semigroup
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative

newtype Trek s a = Trek (LogicT (State s) a)
  deriving newtype (Functor, Applicative, Monad, MonadState s, Alternative)

-- type instance Zoomed (Trek s) = Zoomed (State s)

-- instance Zoom (Trek s) (Trek t) s t where
--   zoom :: forall s t c. LensLike' (Zoomed (Trek s) c) t s -> Trek s c -> Trek t c
--   zoom trav (Trek logt) = do
--     let st = observeAllT logt
--     let zst = zoom trav st
--     xs <- Trek (lift zst)
--     iter xs
--     where
--       iter = asum . fmap pure . toList

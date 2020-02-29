{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

iter :: Foldable f => f a -> Trek s a
iter = asum . fmap pure . toList

fetch :: Fold s a -> Trek s a
fetch fld = gets (toListOf fld) >>= iter

collectList :: Trek s a -> Trek s [a]
collectList trek = do
    s <- get
    return $ runTrek trek s

with :: s -> Trek s a -> Trek t a
with s trek =
    let xs = runTrek trek s
     in iter xs

withAll :: [s] -> Trek s a -> Trek t a
withAll xs trek =
    iter xs >>= flip with trek

withEachOf :: Fold t s -> Trek s a -> Trek t a
withEachOf fld exp = do
    xs <- collectList (fetch fld)
    asum $ fmap (flip with exp) xs

runTrek :: Trek s a -> s -> [a]
runTrek (Trek logt) s = flip evalState s $ observeAllT logt

runTrek1 :: Trek s a -> s -> a
runTrek1 (Trek logt) s = flip evalState s $ observeT logt

collectMap :: (Ord k, Applicative f) => [(k, f v)] -> f (M.Map k v)
collectMap = sequenceA . M.fromList



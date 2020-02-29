module Trek.Combinators where

import Trek.Monad
import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative
import Control.Monad.Logic
import Data.Foldable

select :: (s -> a) -> Trek s a
select getter = gets getter

selectEach :: (s -> [a]) -> Trek s a
selectEach getter = gets getter >>= iter

iter :: Foldable f => f a -> Trek s a
iter = asum . fmap pure . toList

collectList :: Trek s a -> Trek s [a]
collectList trek = do
    s <- get
    return $ runTrek trek s

with :: s -> Trek s a -> Trek t a
with s trek =
    let xs = runTrek trek s
     in iter xs

withEach :: [s] -> Trek s a -> Trek t a
withEach xs trek =
    iter xs >>= flip with trek

runTrek :: Trek s a -> s -> [a]
runTrek (Trek logt) s = flip evalState s $ observeAllT logt

runTrek1 :: Trek s a -> s -> a
runTrek1 (Trek logt) s = flip evalState s $ observeT logt

collectMap :: (Ord k, Applicative f) => [(k, f v)] -> f (M.Map k v)
collectMap = sequenceA . M.fromList

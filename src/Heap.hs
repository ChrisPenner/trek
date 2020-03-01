module Heap where

import Trek.Monad
import Trek.Combinators
import Trek.Optics


data Heap a = Node (Heap a) a (Heap a) | Empty

left :: Heap a -> Maybe (Heap a)
left Empty = Nothing
left (Node l _ _) = Just l
right :: Heap a -> Maybe (Heap a)
right Empty = Nothing
right (Node _ _ r) = Just r

heapLE :: Ord a => Heap a -> Heap a -> Bool
heapLE Empty _ = True
heapLE _ Empty = False
heapLE (Node _ l _) (Node _ r _) = l <= r

addVal :: Ord a => Heap a -> a -> Heap a
addVal Empty a = Node Empty a Empty
addVal (Node l v r) a
  | a < v = if heapLE l r then Node (addVal l v) a r
                          else Node l a (addVal r v)
  | heapLE (Node Empty a Empty) r = Node (addVal l a) v r
  | otherwise = Node l v (addVal r a)


-- addVal' :: Ord a => a -> Trek (Heap a) (0)
-- addVal' a = do
--     select left

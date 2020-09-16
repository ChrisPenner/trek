{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Examples.Blog.Scope where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Category ((>>>))
import Data.Function
import Control.Monad.Logic
import Control.Applicative
import Data.Foldable
import Data.Traversable


data Address =
    Address { _street :: String
            }
    deriving Show

data Book =
    Book { _title  :: String
         , _author :: String
         }
    deriving Show

data BookStore =
    BookStore { _address :: Address
              , _books   :: [Book]
              }
    deriving Show

makeLenses ''BookStore
makeLenses ''Address
makeLenses ''Book

bobsBooks :: BookStore
bobsBooks =
    BookStore (Address "213 Drury Lane")
              [ Book "The Great Gatsby" "F. Scott Fitzgerald"
              , Book "Moby Dick" "Herman Melville"
              ]

gatsbyAuthor :: BookStore -> String
gatsbyAuthor =
    view (books . ix 0 . author)

gatsbySummary :: BookStore -> String
gatsbySummary = do
    magnify (books . ix 0) $ do
        theTitle <- view title
        theAuthor <- view author
        return $ theTitle <> " by " <> theAuthor

-- withValue :: x -> Reader x a -> Reader e a
-- withValue x r = return $ runReader r x

-- focusing :: Monad m => (e -> x) -> ReaderT x m a -> ReaderT e m a
-- focusing f r = do
--     x <- asks f
--     lift $ runReaderT r x

-- gatsbySummary' :: Reader BookStore String
-- gatsbySummary' = do
--     book <- asks (books >>> head)
--     withValue book $ do
--         theTitle <- asks title
--         theAuthor <- asks author
--         return $ theTitle <> " by " <> theAuthor


-- listMonad :: [String]
-- listMonad = do
--     name <- ["Bob", "Steven"]
--     return $ "Hello " <> name


-- listMonad2 :: [(Char, Int)]
-- listMonad2 = do
--     char <- ['a', 'b']
--     num <- [1, 2, 3]
--     return (char, num)

-- focusingEach :: (e -> [x]) -> ReaderT x [] a -> ReaderT e [] a
-- focusingEach f r = do
--     xs <- asks f
--     x <- lift xs
--     lift $ runReaderT r x


allSummaries :: BookStore -> [String]
allSummaries = do
    magnify (books . folded) $ do
        theTitle <- view title
        theAuthor <- view author
        return [theTitle <> " by " <> theAuthor]

allSummariesNoLongAuthors :: BookStore -> [String]
allSummariesNoLongAuthors = do
    magnify (books . folded) $ do
        theTitle <- view title
        theAuthor <- view author
        if length theAuthor > 16
           then return []
           else return [theTitle <> " by " <> theAuthor]

-- magnifyEach :: Monoid r => Fold s a -> ReaderT a [] r -> ReaderT s [] r
-- magnifyEach fld m = do
--     magnify fld $ (m >>= lift . (:[]))

magnifyEach :: Fold s a -> ReaderT a [] r -> ReaderT s [] r
magnifyEach fld m = do
    xs <- magnify fld $ do
        a <- ask
        return $ runReaderT m a
    lift xs

allSummaries' :: ReaderT BookStore [] String
allSummaries' = do
    magnifyEach (books . folded) $ do
        theTitle <- view title
        theAuthor <- view author
        return $ theTitle <> " by " <> theAuthor

allSummariesNoLongAuthors' :: ReaderT BookStore [] String
allSummariesNoLongAuthors' = do
    magnifyEach (books . folded) $ do
        theTitle <- view title
        theAuthor <- view author
        guard (length theAuthor <= 16)
        return $ theTitle <> " by " <> theAuthor


allSummariesNoLongAuthors'' :: Reader BookStore [String]
allSummariesNoLongAuthors'' = do
    summaries <- magnify (books . folded) $ do
        theTitle <- view title
        theAuthor <- view author
        -- guard (length theAuthor <= 16)
        return $ [theTitle <> " by " <> theAuthor]
    for summaries $ \summary -> do
        return summary


zoomEach :: Traversal' s a -> StateT a [] r -> StateT s [] r
zoomEach trav m = do
    xs <- zoom trav $ do
        a <- get
        let rs = runStateT m a
        a' <- lift $ if null rs
                        then [a]
                        else snd <$> rs
        put a'
        return (fst <$> rs)
    lift xs

-- zoomEach' :: forall s a r. Traversal' s a -> LogicT (State a) r -> LogicT (State s) r
-- zoomEach' trav m = do
--     xs <- zoom trav $ do
--         a <- get
--         lift $ m
--     asum $ pure <$> xs


-- truncateTitles :: StateT BookStore [] ()
-- truncateTitles = do
--     -- (books' . traversed . title') %= \t ->  take 5 t <> "..."
--     zoom (books' . traversed . title') $ do
--         t <- get
--         t' <- lift [t, take 5 t <> "..."]
--         put t'

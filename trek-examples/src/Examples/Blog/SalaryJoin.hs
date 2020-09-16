{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Examples.Blog.SalaryJoin where

import System.IO.Unsafe
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Applicative
import Control.Monad.Logic
import Data.Foldable
import Control.Lens.Internal.Zoom
import ListT

type instance Zoomed (LogicT m) = FocusingOn [] (Zoomed m)
instance Zoom m n s t => Zoom (LogicT m) (LogicT n) s t where
  zoom l = liftLogic . zoom (\afb -> unfocusingOn . l (FocusingOn . afb)) . observeAllT

liftLogic :: Monad m => m [a] -> LogicT m a
liftLogic m = lift m >>= asum . fmap pure

info :: Value
info = unsafePerformIO $ do
    readFile "./join.json" <&> view (singular _JSON)

safeView t =
    asks (preview t) >>= maybe empty pure

safeUse t = preuse t >>= maybe empty pure


x ^% l = maybe empty pure $ x ^? l


infixr 0 %>
(%>) :: Traversal' s e -> MaybeT (State e) a -> MaybeT (State s) [a]
l %> m = do
    zoom l $ do
        e <- get
        a <- lift $ runMaybeT m
        return (maybe [] (:[]) a)

(-%>) :: Traversal' s e -> MaybeT (State e) a -> MaybeT (State s) ()
l -%> m = do
    void (l %> m)

(..%>) :: Traversal' s e -> LogicT (State e) a -> LogicT (State s) a
l ..%> m = do
    xs <- zoom l $ lift $ observeAllT m
    asum . fmap pure $ xs

(-..%>) :: Traversal' s e -> LogicT (State e) a -> LogicT (State s) ()
l -..%> m = do
    void (l ..%> m)


query :: MaybeT (State Value) [T.Text]
query = do
    salaries <- safeUse (key "salaries" . _JSON @Value @(M.Map T.Text Int))
    (key "staff" . values) %> do
        theID <- safeUse (key "id" . _String) 
        theName <- safeUse (key "name" . _String) 
        salary <- salaries ^% ix theID
        key "name" . _String <>= " - before"
        guard (theID == "2")
        key "name" . _String <>= " - done"
        return $ theName <> " makes $" <> T.pack (show salary)

query' :: LogicT (State Value) T.Text
query' = do
    salaries <- safeUse (key "salaries" . _JSON @Value @(M.Map T.Text Int))
    (key "staff" . values) ..%> do
        theID <- safeUse (key "id" . _String) 
        theName <- safeUse (key "name" . _String) 
        salary <- salaries ^% ix theID
        key "name" . _String <>= " - before"
        guard (theID == "2")
        suffix <- pure "ONE" <|> pure "TWO"
        key "name" . _String <>= " - " <> suffix
        return $ theName <> " makes $" <> T.pack (show salary) <> suffix

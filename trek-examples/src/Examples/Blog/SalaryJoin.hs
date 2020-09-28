{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
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
import List.Transformer as LT
import Data.Traversable
import Data.Monoid

type instance Zoomed (ListT m) = FocusingOn [] (Zoomed m)
instance Zoom m n s t => Zoom (ListT m) (ListT n) s t where
  zoom l = liftLogic . zoom (\afb -> unfocusingOn . l (FocusingOn . afb)) . (LT.fold (\xs a -> a : xs) mempty id)

liftLogic :: Monad m => m [a] -> ListT m a
liftLogic m = lift m >>= LT.select

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
        a <- lift $ runMaybeT m
        return (maybe [] (:[]) a)

(-%>) :: Traversal' s e -> MaybeT (State e) a -> MaybeT (State s) ()
l -%> m = do
    void (l %> m)

(..%>) :: Traversal' s e -> ListT (State e) a -> ListT (State s) a
l ..%> m = do
    xs <- zoom l $ lift $ LT.fold (\xs a -> a : xs) mempty id m
    asum . fmap pure $ xs

(-..%>) :: Traversal' s e -> ListT (State e) a -> ListT (State s) ()
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

query' :: ListT (State Value) T.Text
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



data Company = Company { _staff :: [Employee]
                       , _salaries :: M.Map Int Int
                       }
  deriving Show
data Pet = Pet { _petName :: String
               , _petType :: String
               }
  deriving Show
data Employee = Employee { _employeeId :: Int
                         , _employeeName :: String
                         , _employeePets :: [Pet]
                         }
  deriving Show

company :: Company
company = Company [ Employee 1 "bob" [Pet "Rocky" "cat", Pet "Bullwinkle" "dog"] 
                  , Employee 2 "sally" [Pet "Inigo" "cat"]
                  ] (M.fromList [ (1, 12)
                                , (2, 15)
                                ])

makeLenses ''Company
makeLenses ''Pet
makeLenses ''Employee

cats :: [Pet]
cats = company ^.. staff . folded . employeePets . folded . filteredBy (petType . only "cat") 

owners :: [String]
owners = company ^.. (staff . folded . reindexed _employeeName selfIndex <. employeePets . folded . petName) . withIndex . to (\(eName, pName) -> pName <> " belongs to " <> eName)

owners' :: Reader Company [String]
owners' = do
    magnify (staff . folded) $ do
        eName <- view employeeName
        magnify (employeePets . folded) $ do
            pName <- view petName
            return [pName <> " belongs to " <> eName]

salaryBump :: State Company ()
salaryBump = do
    ids <- zoom (staff . traversed . filteredBy (employeePets . traversed . petType . only "dog")) $ do
        uses employeeId (:[])
    for_ ids $ \id' ->
        salaries . ix id' += 5

salaryBump' :: State Company ()
salaryBump' = do
    ids <- gets $ toListOf (staff . traversed . filteredBy (employeePets . traversed . petType . only "dog") . employeeId)
    for_ ids $ \id' ->
        salaries . ix id' += 5


salaryBump'' :: MaybeT (State Company) ()
salaryBump'' = do
    ids <- staff . traversed %> do
        isDog <- employeePets . traversed %> do
                        pType <- use petType
                        return $ pType == "dog"
        guard (or isDog)
        use employeeId
    for_ ids $ \id' ->
        salaries . ix id' += 5


salaryBumpJSON :: MaybeT (State Value) ()
salaryBumpJSON = do
    ids <- key "staff" . values %> do
        isDog <- key "pets" . values %> do
                        pType <- use (key "type" . _String)
                        return $ pType == "dog"
        guard (or isDog)
        use (key "id" . _String)
    for_ ids $ \id' ->
        key "salaries" . key id' . _Integer += 5








-- richCats :: [Pet]
-- richCats = company ^.. staff . folded . pets . folded . filteredBy (petType . only "cat") 
--   where
--     salaries


-- {
--     "staff":
--       [
--         { "id": "1"
--         , "name": "bob"
--         , "pets": [
--               { "name": "Rocky"
--               , "type": "cat"
--               },
--               { "name": "Bullwinkle"
--               , "type": "cat"
--               }
--             ]
--         },
--         { "id": "2"
--         , "name": "sally"
--         , "pets": [
--               { "name": "Inigo"
--               , "type": "dog"
--               }
--             ]
--         }
--       ],
--     "salaries": {
--         "1": 12,
--         "2": 15
--     }
-- }

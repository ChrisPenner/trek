{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Trek.HKD where

import Barbies
import Trek.Monad
import qualified Data.Text as T
import Data.Functor.Identity
import GHC.Generics (Generic)
import Data.Aeson

data Food f =
    Food { food     :: f T.Text
         , category :: f T.Text
         }
    deriving (Generic, FunctorB, TraversableB, ApplicativeB
            , ConstraintsB)

deriving instance (forall x. ToJSON x => ToJSON (f x)) => ToJSON (Food f)
deriving instance (forall x. FromJSON x => FromJSON (f x)) => FromJSON (Food f)

data FoodMap f =
    FoodMap { favoriteFood :: Food f
            , name         :: f T.Text
            }
    deriving (Generic, FunctorB, TraversableB, ApplicativeB
            , ConstraintsB)

deriving instance (forall x. ToJSON x => ToJSON (f x)) => ToJSON (FoodMap f)
deriving instance (forall x. FromJSON x => FromJSON (f x)) => FromJSON (FoodMap f)

runHKD :: TraversableB b => b (Trek s) -> Trek s (b Identity)
runHKD = bsequence'

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Trek.HKD where

import Barbies
import Trek
import Data.Functor.Identity

runHKD :: TraversableB b => b (Trek s) -> Trek s (b Identity)
runHKD = bsequence'

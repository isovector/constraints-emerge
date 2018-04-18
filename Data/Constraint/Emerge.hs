{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Constraint.Emerge
  ( Emerge
  , emerge
  , Dict (..)
  ) where

import Data.Constraint


------------------------------------------------------------------------------
-- | Typeclass asserting we can determine the existence of the constraint 'c'
-- at runtime.
class Emerge (c :: Constraint) where
  emerge' :: Maybe (Dict c)


------------------------------------------------------------------------------
-- | Get a dictionary for 'c' if it exists.
emerge :: Emerge c => Maybe (Dict c)
emerge = emerge'


------------------------------------------------------------------------------
-- | A typeclass whose dictionaries we can coerce into those for 'Emerge' if
-- our constraint wasn't satisfied.
class AlwaysFail where
  alwaysFail :: Maybe (Dict a)

instance AlwaysFail where
  alwaysFail = Nothing


------------------------------------------------------------------------------
-- | A typeclass whose dictionaries we can coerce into those for 'Emerge' if
-- our constraint was satisfied.
class Succeed (a :: Constraint) where
  succeed :: Maybe (Dict a)

instance c => Succeed c where
  succeed = Just Dict


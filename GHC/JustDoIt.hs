{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module GHC.JustDoIt ( JustDoIt, justDoIt) where

import Data.Kind
import Data.Constraint

class JustDoIt (a :: Constraint) where
  justDoIt' :: Maybe (Dict a)

class AlwaysFail where
  alwaysFail :: Maybe (Dict a)

instance AlwaysFail where
  alwaysFail = Nothing

class Succeed (a :: Constraint) where
  succeed :: Maybe (Dict a)

instance c => Succeed c where
  succeed = Just Dict

justDoIt :: JustDoIt c => Maybe (Dict c)
justDoIt = justDoIt'


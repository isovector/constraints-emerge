{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHC.JustDoIt ( JustDoIt, justDoIt) where

import Data.Kind
import Data.Constraint

class JustDoIt (a :: Constraint) where
  justDoIt' :: Maybe (Dict a)

class Working

justDoIt :: JustDoIt c => Maybe (Dict c)
justDoIt = justDoIt'


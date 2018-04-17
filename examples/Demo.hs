{-# LANGUAGE GADTs                                  #-}
{-# LANGUAGE ScopedTypeVariables                    #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, EmptyCase #-}
{-# LANGUAGE TypeApplications                       #-}
{-# OPTIONS_GHC -fplugin=GHC.JustDoIt.Plugin        #-}

import GHC.JustDoIt

import Prelude hiding (id, flip, const, curry)
import Data.Constraint

-- showFunc :: Maybe (Dict (Show (Int -> Bool)))
-- showFunc = justDoIt

-- showInt :: Maybe (Dict (Show Int))
-- showInt = justDoIt

showIt :: forall c. c -> String
showIt c =
  case justDoIt @(Show c) of
    Just Dict -> show c
    Nothing   -> "<uncool>"

-- showAll :: forall c. c -> String
-- showAll c =
--   case justDoIt @(Show c) of
--     Just Dict -> show c
--     Nothing   -> show "<<unshowable>>"


{-# LANGUAGE ConstraintKinds                        #-}
{-# LANGUAGE FlexibleContexts                       #-}
{-# LANGUAGE GADTs                                  #-}
{-# LANGUAGE ScopedTypeVariables                    #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, EmptyCase #-}
{-# LANGUAGE TypeApplications                       #-}
{-# OPTIONS_GHC -fplugin=GHC.JustDoIt.Plugin        #-}

import GHC.JustDoIt

import Data.Constraint

-- showFunc :: Maybe (Dict (Show (Int -> Bool)))
-- showFunc = justDoIt

-- showInt :: Maybe (Dict (Show Int))
-- showInt = justDoIt


-- showIt :: forall c. JustDoIt (Show c) => c -> String
-- showIt c =
--   case justDoIt @(Show c) of
--     Just Dict -> show c
--     Nothing   -> "<<not gonna do it>>"

-- showId   = showIt id
-- showBool = showIt True

-- showFoo :: forall cbar. JustDoIt (Show cbar) => cbar -> String
-- showFoo c =
--     case justDoIt @(Show cbar) of
--       Just Dict -> show c
--       Nothing ->  "fail2"




-- asInt :: forall c. JustDoIt (c ~ Int) => c -> Int
-- asInt c =
--   case justDoIt @(c ~ Int) of
--     Just Dict -> c
--     Nothing -> 0

breakingBad :: forall c. JustDoIt c => Maybe (Dict c)
breakingBad = justDoIt @c

-- showBad = showFoo (5 :: Int)
-- showBad2 = showFoo True
-- showBad3 = showFoo id

-- showAll :: forall c. c -> String
-- showAll c =
--   case justDoIt @(Show c) of
--     Just Dict -> show c
--     Nothing   -> show "<<unshowable>>"


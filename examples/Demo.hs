{-# LANGUAGE ConstraintKinds                           #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE GADTs                                     #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, EmptyCase    #-}
{-# LANGUAGE TypeApplications                          #-}
{-# OPTIONS_GHC -fplugin=Data.Constraint.Emerge.Plugin #-}

import Data.Constraint.Emerge

showFunc :: Maybe (Dict (Show (Int -> Bool)))
showFunc = emerge

showInt :: Maybe (Dict (Show Int))
showInt = emerge


showIt :: forall c. Emerge (Show c) => c -> String
showIt c =
  case emerge @(Show c) of
    Just Dict -> show c
    Nothing   -> "<<not gonna do it>>"

showId   = showIt id
showBool = showIt True


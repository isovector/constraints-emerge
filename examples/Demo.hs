{-# LANGUAGE ConstraintKinds                           #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE GADTs                                     #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, EmptyCase    #-}
{-# LANGUAGE TypeApplications                          #-}
{-# OPTIONS_GHC -fplugin=Data.Constraint.Emerge.Plugin #-}

import Data.Constraint.Emerge

import Control.Monad.Trans.Writer

lol :: forall e m. Emerge (Monad (WriterT e m)) => Maybe (Dict (Monad (WriterT e m)))
lol = emerge @(Monad (WriterT e m))

showAnything :: forall c. Emerge (Show c) => c -> String
showAnything c =
  case emerge @(Show c) of
    Just Dict -> show c
    Nothing   -> "<<unshowable>>"

showBool = showAnything True
showId = emerge


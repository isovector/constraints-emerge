{-# OPTIONS_GHC -fplugin=GHC.JustDoIt.Plugin #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, EmptyCase #-}
import GHC.JustDoIt

import Prelude hiding (id, flip, const, curry)
import Data.Constraint

foo :: Maybe (Dict (Show (Int)))
foo = justDoIt

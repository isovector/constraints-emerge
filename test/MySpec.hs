{-# LANGUAGE ConstraintKinds                 #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE GADTs                           #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeApplications                #-}
{-# OPTIONS_GHC -fplugin=GHC.JustDoIt.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MySpec where

import Data.Maybe (isJust, isNothing)
import GHC.JustDoIt
import Data.Constraint (Dict (..))
import Test.Hspec


getMultiParam :: forall a b. JustDoIt (MultiParam a b) => Maybe (a, b)
getMultiParam =
  case justDoIt @(MultiParam a b) of
    Just Dict -> Just multiParam
    Nothing -> Nothing


showAnything :: forall c. JustDoIt (Show c) => c -> String
showAnything c =
  case justDoIt @(Show c) of
    Just Dict -> show c
    Nothing -> "<<unshowable>>"


brokenToInt :: forall c. JustDoIt (c ~ Int) => c -> Int
brokenToInt c =
  case justDoIt @(c ~ Int) of
    Just Dict -> c
    Nothing   -> 17


spec :: Spec
spec = do
  describe "dictionary lookups" $ do
    it "Show Int" $ do
      justDoIt @(Show Int) `shouldBe` Just Dict

    it "Show function" $ do
      justDoIt @(Show (Bool -> Int)) `shouldBe` Nothing

    it "Show locally defined instance" $ do
      justDoIt @(Show (MyType -> MyType)) `shouldBe` Just Dict

    it "Show orphan instance" $ do
      justDoIt @(Show (String -> String)) `shouldBe` Just Dict


  describe "dictionary usages" $ do
    it "showAnything 5" $ do
      showAnything (5 :: Int) `shouldBe` show (5 :: Int)

    it "showAnything True" $ do
      showAnything True `shouldBe` show True

    it "showAnything id" $ do
      showAnything id `shouldBe` "<<unshowable>>"

    it "getMultiParam @Int @Bool" $ do
      getMultiParam `shouldBe` Just (1 :: Int, True)
      getMultiParam @Int @Bool `shouldBe` Just (1, True)

    it "getMutliParam @Bool @Bool" $ do
      getMultiParam @Bool @Bool `shouldBe` Nothing

    it "lookup overlapping instances" $ do
      isOverlapping "hello" `shouldBe` False
      isOverlapping True    `shouldBe` True


  describe "bugs" $ do
    it "BROKEN: bool to int" $ do
      brokenToInt True `shouldNotBe` 17

    it "WORKS: int to int" $ do
      brokenToInt 5 `shouldBe` 5



-- local instance of 'Show'
data MyType
instance Show (MyType -> MyType) where
  show = const "mytype function"

-- orphan instance of 'Show'
instance Show (String -> String) where
  show _ = "orphan show"


-- overlapping instances
class Overlapping z where
  isOverlapping :: z -> Bool

instance Overlapping a where
  isOverlapping = const False

instance {-# OVERLAPPING #-} Overlapping Bool where
  isOverlapping = const True


-- multi param typeclasses
class MultiParam a b where
  multiParam :: (a, b)

instance MultiParam Int Bool where
  multiParam = (1, True)

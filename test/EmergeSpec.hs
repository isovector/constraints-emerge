{-# LANGUAGE ConstraintKinds                           #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE FlexibleInstances                         #-}
{-# LANGUAGE GADTs                                     #-}
{-# LANGUAGE MultiParamTypeClasses                     #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
{-# LANGUAGE TypeApplications                          #-}
{-# OPTIONS_GHC -fno-warn-orphans                      #-}
{-# OPTIONS_GHC -fplugin=Data.Constraint.Emerge.Plugin #-}

module EmergeSpec where

import Control.Monad.Trans.Writer (WriterT)
import Data.Constraint.Emerge
import Test.Hspec


getWriterTMonad
    :: forall e m z
     . ( z ~ Monad (WriterT e m)
       , Emerge z
       )
    => Maybe (Dict z)
getWriterTMonad = emerge @z


getMultiParam :: forall a b. Emerge (MultiParam a b) => Maybe (a, b)
getMultiParam =
  case emerge @(MultiParam a b) of
    Just Dict -> Just multiParam
    Nothing -> Nothing


showAnything :: forall c. Emerge (Show c) => c -> String
showAnything c =
  case emerge @(Show c) of
    Just Dict -> show c
    Nothing -> "<<unshowable>>"


brokenToInt :: forall c. Emerge (c ~ Int) => c -> Int
brokenToInt c =
  case emerge @(c ~ Int) of
    Just Dict -> c
    Nothing   -> 17

-- showTree :: Emerge (Show a) => a -> Int -> String
-- showTree v 0 = showAnything v
-- showTree v n = showTree (v, v) (n - 1)


spec :: Spec
spec = do
  describe "dictionary lookups" $ do
    it "Show Int" $ do
      emerge @(Show Int) `shouldBe` Just Dict

    it "Show function" $ do
      emerge @(Show (Bool -> Int)) `shouldBe` Nothing

    it "Show locally defined instance" $ do
      emerge @(Show (MyType -> MyType)) `shouldBe` Just Dict

    it "Show orphan instance" $ do
      emerge @(Show (String -> String)) `shouldBe` Just Dict

    it "complicated subdicts for WriterT" $ do
      getWriterTMonad @[Int] @IO `shouldBe` Just Dict
      getWriterTMonad @Int   @IO `shouldBe` Nothing


  describe "dictionary usages" $ do
    it "showAnything 5" $ do
      showAnything (5 :: Int) `shouldBe` show (5 :: Int)

    it "showAnything True" $ do
      showAnything True `shouldBe` show True

    it "showAnything id" $ do
      showAnything (id @Int) `shouldBe` "<<unshowable>>"

    it "showAnything (5, (6, True)" $ do
      showAnything (5 :: Double, (6 :: Int, True))
        `shouldBe` show (5 :: Double, (6 :: Int, True))

    it "getMultiParam @Int @Bool" $ do
      getMultiParam `shouldBe` Just (1 :: Int, True)
      getMultiParam @Int @Bool `shouldBe` Just (1, True)

    it "getMutliParam @Bool @Bool" $ do
      getMultiParam @Bool @Bool `shouldBe` Nothing

    it "lookup overlapping instances" $ do
      isOverlapping "hello" `shouldBe` False
      isOverlapping True    `shouldBe` True


  describe "bugs" $ do
    it "WORKS: bool to int" $ do
      brokenToInt True `shouldBe` 17

    it "BROKEN: int to int" $ do
      brokenToInt (5 :: Int) `shouldNotBe` 5

    it "BROKEN: get self" $ do
      emerge @(Emerge (Emerge (Show Int))) `shouldBe` Nothing



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

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs, PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Sleepiness where

import Ivory.Language as Ivory

data Sleepiness
  = Energetic -- not going to fell asleep until become sleepy
  | Sleepy -- going to fell asleep asap
  | Lethargic -- going fell asleep and never wake up
  | Insomniac -- not ever going to fell asleep
  deriving (Bounded, Enum)

-- | A C representation of Sleepiness
newtype CSleepiness = MkCSleepiness Uint8
  deriving newtype (IvoryType, IvoryVar, IvoryStore, IvoryExpr, IvoryEq, Num)

sleepinessToC :: Sleepiness -> CSleepiness
sleepinessToC = MkCSleepiness . fromInteger . toEnum . fromEnum

-- | Match the haskell enum represented by some C number (as enum?)
-- Inlines all the branches as nested if-then-else statements
matchEnum :: forall eff b conId.
  (IvoryEq conId, Num conId) =>
  (Bounded b, Enum b) =>
  conId -> (b -> Ivory eff ()) -> Ivory eff ()
matchEnum conId cont = do
  foldr
    (\con else_ -> ifte_ (conId ==? toConId con) (cont con) else_)
    (assert false)
  $ enumFrom (minBound @b)
  where
    toConId :: b -> conId
    toConId = fromInteger . toEnum . fromEnum

getEnergized :: forall s.
  IvoryType CSleepiness =>
  IvoryStore CSleepiness =>
  IvoryStore CSleepiness =>
  Def ('[Ref s (Stored CSleepiness)] :-> ())
getEnergized = proc "getEnergized" \state -> body do
  csleepiness <- deref state
  matchEnum csleepiness \sleepiness -> case sleepiness of
    Energetic -> retVoid
    Sleepy -> store state (sleepinessToC Energetic)
    Lethargic -> retVoid -- lethargics can not
    Insomniac -> retVoid -- insomniacs can not

getLethargicSleep :: forall s.
  IvoryType CSleepiness =>
  IvoryStore CSleepiness=>
  IvoryStore CSleepiness =>
  Def ('[Ref s (Stored CSleepiness)] :-> ())
getLethargicSleep = proc "getLethargicSleep" \state -> body do
  csleepiness <- deref state
  matchEnum csleepiness \sleepiness -> case sleepiness of
    Energetic -> store state (sleepinessToC Lethargic)
    Sleepy -> store state (sleepinessToC Lethargic)
    Lethargic -> retVoid
    Insomniac -> retVoid -- insomniacs can not

getInsomnia :: forall s.
  IvoryType CSleepiness =>
  IvoryStore CSleepiness =>
  IvoryStore CSleepiness =>
  Def ('[Ref s (Stored CSleepiness)] :-> ())
getInsomnia = proc "getInsomnia" \state -> body do
  csleepiness <- deref state
  matchEnum csleepiness \sleepiness -> case sleepiness of
    Energetic -> store state (sleepinessToC Insomniac)
    Sleepy -> store state (sleepinessToC Insomniac)
    Lethargic -> retVoid -- lethargic can not
    Insomniac -> retVoid

getSleepy :: forall s.
  IvoryType CSleepiness =>
  IvoryStore CSleepiness =>
  IvoryStore CSleepiness =>
 -- (Equal sleepiness Insomniac -> Void) -> -- insomniacs can not
  Def ('[Ref s (Stored CSleepiness)] :-> ())
getSleepy = proc "getSleepy" \state -> body do
  csleepiness <- deref state
  matchEnum csleepiness \sleepiness -> case sleepiness of
    Energetic -> store state (sleepinessToC Sleepy)
    Sleepy -> retVoid
    Lethargic -> store state (sleepinessToC Sleepy)
    Insomniac -> retVoid



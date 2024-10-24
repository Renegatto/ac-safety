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


import {-qualified-} Ivory.Language as Ivory
import Ivory.Language.Pointer (Pointer, KnownConstancy, Nullability(Valid))
import qualified Ivory.Language.Proc as Proc
import Data.Void (Void, absurd)
import Data.Kind (Type)
import Control.Arrow ((>>>))
import Unsafe.Coerce (unsafeCoerce)
import qualified Ivory.Language.Pointer as Pointer

data Equal a b where
  EqRefl :: Equal a a

data SleepinessLevel
  = Energetic -- not going to fell asleep until become sleepy
  | Sleepy -- going to fell asleep asap
  | Lethargic -- going fell asleep and never wake up
  | Insomniac -- not ever going to fell asleep
  deriving (Bounded, Enum)

type Forget :: forall k. (k -> Type) -> Type
data Forget f = forall a. MkForget (f a) 

data Sleepiness s where
  EnergeticT :: Sleepiness Energetic 
  SleepyT :: Sleepiness Sleepy 
  LethargicT :: Sleepiness Lethargic 
  InsomniacT :: Sleepiness Insomniac 

-- | A C representation of Sleepiness
newtype CSleepiness a = MkCSleepiness Uint8
  deriving newtype (IvoryType, IvoryVar, IvoryStore, IvoryExpr, IvoryEq, Num)

-- | Update CSleepiness and returning the same pointer with updated type
writeSleepiness :: forall s a b eff.
  IvoryStore (CSleepiness b) =>
  Ref s (Stored (CSleepiness a)) ->
  CSleepiness b ->
  Ivory eff (Ref s (Stored (CSleepiness b)))
writeSleepiness (Pointer.Pointer ref) b = do
  let refB = Pointer.Pointer ref
  store refB b
  pure refB

toLevel :: Sleepiness s -> SleepinessLevel
toLevel = \case
  EnergeticT -> Energetic 
  SleepyT -> Sleepy 
  LethargicT -> Lethargic 
  InsomniacT -> Insomniac 

fromLevel :: SleepinessLevel -> Forget Sleepiness
fromLevel = \case
  Energetic -> MkForget EnergeticT 
  Sleepy -> MkForget SleepyT 
  Lethargic -> MkForget LethargicT 
  Insomniac -> MkForget InsomniacT 

sleepinessToC :: forall s. Sleepiness s -> CSleepiness s
sleepinessToC = MkCSleepiness . fromInteger . toEnum . fromEnum . toLevel

-- | Match the haskell enum represented by some C number (as enum?)
-- Inlines all the branches as nested if-then-else statements
matchSleepiness :: forall s eff. CSleepiness s -> (Sleepiness s -> Ivory eff ()) -> Ivory eff ()
matchSleepiness conId cont =
  matchEnum conId $ fromLevel >>> \(MkForget (sleepiness :: Sleepiness s0)) ->
    -- This relies completely on the correct pattern matching
    cont $ unsafeCoerce @(Sleepiness s0) @(Sleepiness s) sleepiness

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

getEnergized :: forall sleepiness (state :: SleepinessLevel -> Type) s.
  (forall s0. IvoryType (state s0)) =>
  (forall s0. IvoryStore (state s0)) =>
  IvoryStore (state sleepiness) =>
  (state ~ CSleepiness) =>
  (Equal sleepiness Insomniac -> Void) -> -- insomniacs can not
  (Equal sleepiness Lethargic -> Void) -> -- lethargics can not
  Def ('[Ref s (Stored (state sleepiness))] :-> Ref s (Stored (state Energetic)))
getEnergized nonInsomniac nonLethargic = proc "getEnergized" \state -> body do
  csleepiness <- deref state
  matchSleepiness csleepiness \sleepiness -> case sleepiness of
    EnergeticT ->
      ret state
    SleepyT ->
      ret =<< writeSleepiness state (sleepinessToC EnergeticT)
    LethargicT -> absurd $ nonLethargic EqRefl
    InsomniacT -> absurd $ nonInsomniac EqRefl

getLethargicSleep :: forall sleepiness state s.
  (forall s0. IvoryType (state s0)) =>
  (forall s0. IvoryStore (state s0)) =>
  IvoryStore (state sleepiness) =>
  (state ~ CSleepiness) =>
  (Equal sleepiness Insomniac -> Void) -> -- insomniacs can not
  Def ('[Ref s (Stored (state sleepiness))] :-> Ref s (Stored (state Lethargic)))
getLethargicSleep nonInsomniac = proc "getLethargicSleep" \state -> body do
  csleepiness <- deref state
  matchSleepiness csleepiness \sleepiness -> case sleepiness of
    EnergeticT ->
      ret =<< writeSleepiness state (sleepinessToC LethargicT)
    SleepyT ->
      ret =<< writeSleepiness state (sleepinessToC LethargicT)
    LethargicT ->
      ret state
    InsomniacT -> absurd $ nonInsomniac EqRefl

getInsomnia :: forall sleepiness state s.
  (forall s0. IvoryType (state s0)) =>
  (forall s0. IvoryStore (state s0)) =>
  IvoryStore (state sleepiness) =>
  (state ~ CSleepiness) =>
  (Equal sleepiness Lethargic -> Void) -> -- lethargic can not
  Def ('[Ref s (Stored (state sleepiness))] :-> Ref s (Stored (state Insomniac)))
getInsomnia nonLethargic = proc "getInsomnia" \state -> body do
  csleepiness <- deref state
  matchSleepiness csleepiness \sleepiness -> case sleepiness of
    EnergeticT ->
      ret =<< writeSleepiness state (sleepinessToC InsomniacT)
    SleepyT ->
      ret =<< writeSleepiness state (sleepinessToC InsomniacT)
    LethargicT -> absurd $ nonLethargic EqRefl
    InsomniacT -> ret state

getSleepy :: forall sleepiness state s.
  (forall s0. IvoryType (state s0)) =>
  (forall s0. IvoryStore (state s0)) =>
  IvoryStore (state sleepiness) =>
  (state ~ CSleepiness) =>
  (Equal sleepiness Insomniac -> Void) -> -- insomniacs can not
  Def ('[Ref s (Stored (state sleepiness))] :-> Ref s (Stored (state Sleepy)))
getSleepy nonInsomniac = proc "getSleepy" \state -> body do
  csleepiness <- deref state
  matchSleepiness csleepiness \sleepiness -> case sleepiness of
    EnergeticT ->
      ret =<< writeSleepiness state (sleepinessToC SleepyT)
    SleepyT -> ret state
    LethargicT -> 
      ret =<< writeSleepiness state (sleepinessToC SleepyT)
    InsomniacT -> absurd $ nonInsomniac EqRefl



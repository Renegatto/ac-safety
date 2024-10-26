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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
module StandaloneWaterSensor.Timeout where

import qualified StandaloneWaterSensor.Timer as Timer
import qualified StandaloneWaterSensor.Timer.State as Timer

import Prelude hiding (init)
import Ivory.Language as Ivory
import Enum (matchEnum)
import Ivory.Language.Pointer (Nullability(Nullable, Valid), Constancy (Mutable), unsafePointerCast)

data TimeoutParams s = MkTimeoutParams
  { timer :: Timer.CTimerState s
  }

type GlobalTimer = Timer.CTimerState 'Global

[ivory|
struct TimeoutState
  { timer :: Stored GlobalTimer
  ; status :: Stored CStatus
  }
|]

newtype CStatus = MkCStatus { unCStatus :: Uint8 }
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data Status
  = Ticking
  | Expired
  deriving (Enum, Eq, Ord, Bounded)

toCStatus :: Status -> CStatus
toCStatus = fromInteger . toEnum . fromEnum

newtype COutcome = MkCOutcome { unCOutcome :: Uint8 }
  deriving newtype (IvoryType, IvoryStore, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data Outcome
  = Done
  | TimedOut
  | NotYet
  deriving (Enum, Eq, Ord, Bounded)

toCOutcome :: Outcome -> COutcome
toCOutcome = fromInteger . toEnum . fromEnum

class Timeout t where
  checked :: forall s. Def ('[t s, IBool] :-> COutcome)
  reset :: forall s. Def ('[t s] :-> ())

newtype CTimeoutState s = MkCTimeoutState
  { ref :: Ref s (Struct "TimeoutState")
  }
  deriving newtype (IvoryType, IvoryVar)

data TimeoutState s = MkTimeoutState
  { timer :: Timer.TimerState 'Global
  , status :: Ref s (Stored CStatus)
  }

timeoutState :: CTimeoutState s -> Ivory aff (TimeoutState s)
timeoutState cstate = do
  ctimer <- deref $ cstate.ref ~> timer
  timerState <- Timer.timerState ctimer
  pure MkTimeoutState
    { timer = timerState
    , status = cstate.ref ~> status
    }

instance Timeout CTimeoutState where
  checked :: forall s. Def ([CTimeoutState s, IBool] :-> COutcome)
  checked = proc "checkedTimeoutState" \cstate outcome -> body do
    state <- timeoutState cstate
    currentStatus <- deref state.status
    matchEnum currentStatus \case
      Ticking -> do
        ifte_ outcome
          (ret $ toCOutcome Done)
          do
            tickOutcome <- call Timer.tryTick state.timer.repr
            ifte_ tickOutcome
              do
                store state.status $ toCStatus Expired
                ret $ toCOutcome TimedOut
              (ret $ toCOutcome NotYet)
      Expired -> ret $ toCOutcome TimedOut

  reset :: forall s. Def ('[CTimeoutState s] :-> ())
  reset = proc "reset" \cstate -> body do
    state <- timeoutState cstate
    store state.status $ toCStatus Ticking
    call_ Timer.start state.timer.repr

-- instantiation

newLocal :: forall eff (s :: RefScope).
  GetAlloc eff ~ Scope s =>
  TimeoutParams 'Global ->
  Ivory eff (CTimeoutState (Stack s))
newLocal params =
  fmap MkCTimeoutState $ local $ init params.timer

newGlobal :: forall eff (s :: RefScope).
  GetAlloc eff ~ Scope s =>
  TimeoutParams 'Global ->
  Ivory eff (CTimeoutState Global)
newGlobal params = do
  MkCTimeoutState localTimer <- newLocal @_ @s params
  let
    ptr = unsafePointerCast
      @'Nullable @'Valid
      @'Mutable @'Mutable
      nullPtr
  refCopy ptr localTimer
  pure $ MkCTimeoutState ptr

init :: Timer.CTimerState 'Global -> Init (Struct "TimeoutState")
init globalTimer = istruct @"TimeoutState"
    [ status .= ival (toCStatus Ticking)
    , timer .= Timer.initCTimerState globalTimer
    ]

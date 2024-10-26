{-# OPTIONS_GHC -XGHC2021 #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module StandaloneWaterSensor.Timer.State
  ( CTimerState
  , TimerState (MkTimerState, now, interval, previousTime, repr)
  , timerState
  , newLocal
  , newGlobal
  , initCTimerState
  , TimerParams (MkTimerParams, now, interval)
  ) where

import Ivory.Language as Ivory
import Data.Kind (Type)
import Ivory.Language.Pointer (unsafePointerCast, Nullability (Nullable, Valid), Constancy (Mutable, Const), Pointer, pointerCastToConst, pointerCastToNullable)
import Ivory.Language.Proc (ProcType)
import Prelude hiding (init)
import Ivory.Language.Init (Init(Init))
import Data.Coerce (Coercible)
import Enum (coerceInit)

type Now = ProcPtr ('[] :-> Uint64)
instance ProcType a => IvoryStore (ProcPtr a) where

[ivory|

struct TimerState
  { interval :: Stored Uint64
  ; previousTime :: Stored Uint64
  ; now :: Stored Now
  }

|]

type CTimerState :: RefScope -> Type
newtype CTimerState s = MkCTimerState
  { ref :: Ref s (Struct "TimerState")
  }
  deriving newtype (IvoryType, IvoryVar, IvoryStore)

-- IvoryType a => IvoryArea ('Stored a)
-- |- IvoryType (CTimerState s) => IvoryArea ('Stored (CTimerState s))

-- IvoryArea area => IvoryInit (Ptr Global area)
-- |- IvoryArea ('Stored (CTimerState s)) => IvoryInit (Ptr Global ('Stored (CTimerState s)))

-- deriving newtype instance IvoryArea (Struct "TimerState") =>
--   IvoryInit (CTimerState 'Global)

initCTimerState :: CTimerState 'Global -> Init ('Stored (CTimerState 'Global))
initCTimerState =
  coerceInit . ival . pointerCastToNullable . (.ref)

data TimerState s = MkTimerState
  { interval :: Uint64
  , previousTime :: Pointer 'Valid 'Mutable s (Stored Uint64)
  , now :: ProcPtr ('[] :-> Uint64)
  , repr :: CTimerState s
  }

timerState :: CTimerState s -> Ivory eff (TimerState s)
timerState timer = do
  interval <- deref $ pointerCastToConst $ timer.ref ~> interval
  now <- deref $ timer.ref ~> now
  pure MkTimerState
    { interval = interval
    , previousTime = timer.ref ~> previousTime
    , now = now
    , repr = timer
    }

-- instantiation

data TimerParams = MkTimerParams
  { now :: ProcPtr ('[] :-> Uint64)
  , interval :: Uint64
  }

newLocal :: forall eff (s :: RefScope).
  GetAlloc eff ~ Scope s =>
  TimerParams ->
  Ivory eff (CTimerState (Stack s))
newLocal params =
  fmap MkCTimerState $ local $ init params.interval

newGlobal :: forall eff (s :: RefScope).
  GetAlloc eff ~ Scope s =>
  TimerParams ->
  Ivory eff (CTimerState Global)
newGlobal params = do
  MkCTimerState localTimer <- newLocal @_ @s params
  let
    ptr = unsafePointerCast
      @'Nullable @'Valid
      @'Mutable @'Mutable
      nullPtr
  refCopy ptr localTimer
  pure $ MkCTimerState ptr

init :: Uint64 -> Init (Struct "TimerState")
init intervalMs = istruct @"TimerState"
    [ interval .= ival intervalMs
    , previousTime .= ival 0
    ]


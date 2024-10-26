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
{-# LANGUAGE FlexibleInstances #-}
module StandaloneWaterSensor.Timer (
  Timer (tryTick, start {-, newLocal, newGlobal-}),
) where

import Prelude hiding (init)
import Ivory.Language as Ivory
import Data.Kind (Type)
import Ivory.Language.Pointer (unsafePointerCast, Nullability (Nullable, Valid), Constancy (Mutable, Const), Pointer, pointerCastToConst)
import Ivory.Language.Proc (ProcType)

import StandaloneWaterSensor.Timer.State (CTimerState, TimerState(..), timerState)

-- data Timer t = MkTimer
--   { tryTick :: forall s. Def ('[t s] :-> IBool)
--   , start :: forall s. Def ('[t s] :-> ())
--   , newLocal :: forall eff (s :: RefScope).
--       GetAlloc eff ~ Scope s =>
--       Ivory eff (t (Stack s))
--   , newGlobal :: forall eff (s :: RefScope).
--       GetAlloc eff ~ Scope s =>
--       Ivory eff (t Global)
--   }
 
class Timer t where
  tryTick :: forall s. Def ('[t s] :-> IBool)
  start :: forall s. Def ('[t s] :-> ())

instance Timer CTimerState where
  tryTick = tryTickImpl
  start = startImpl

-- compileTimer :: Timer TimerState
-- compileTimer = MkTimer
--   { tryTick = tryTick
--   , start = start
--   , newLocal = newLocal
--   , newGlobal = newGlobal
--   } 
-- instance Timer TimerState where

tryTickImpl :: forall s. Def ('[CTimerState s] :-> IBool)
tryTickImpl = proc "tryTick" \ctimer -> body do
  timer <- timerState ctimer
  now <- indirect timer.now
  previous <- deref $ timer.previousTime
  let timePassed = now - previous
  ifte_ (timePassed >=? timer.interval)
    do
      store (timer.previousTime) now
      ret true
    (ret false)

startImpl :: forall s. Def ('[CTimerState s] :-> ())
startImpl = proc "start" \ctimer -> body do
  timer <- timerState ctimer
  now <- indirect timer.now
  store (timer.previousTime) now


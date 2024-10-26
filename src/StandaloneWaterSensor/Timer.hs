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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -XGHC2021 #-}
module StandaloneWaterSensor.Timer (
  Timer (tryTick, start),
  Ms(MkMs, unMs),
  newTimer,
  TimeNow (MkTimeNow, timeNow)
) where

import Prelude hiding (init)
import Ivory.Language as Ivory

import Control.Monad.State (MonadState)
import Ctx (newMemArea, Ctx, define', mkSym)
import Data.Monoid (Sum(Sum))

newtype Ms = MkMs { unMs :: Uint64 }
  deriving (Semigroup, Monoid) via (Sum Uint64)
  deriving newtype (Num, IvoryExpr, IvoryEq, IvoryOrd, IvoryType, IvoryVar, IvoryStore, IvoryInit, IvoryZeroVal)

data Timer = MkTimer
  { tryTick :: forall eff. Ivory eff IBool
  , start :: forall eff. Ivory eff ()
  }

newtype TimeNow t = MkTimeNow { timeNow :: forall eff. Ivory eff t }

newPrevTimestamp :: forall m. MonadState Ctx m => m (Ref 'Global (Stored Ms))
newPrevTimestamp = addrOf <$> newPrevTimestampArea
  where
    newPrevTimestampArea :: m (MemArea (Stored Ms))
    newPrevTimestampArea =
      newMemArea "timer_prevTimestamp" $ Just $ ival mempty

newTimer ::
  MonadState Ctx m =>
  TimeNow Ms ->
  Ms ->
  m (Ivory (ProcEffects s ()) Timer)
newTimer timeNow interval = do
  state <- newState
  tryTick <- define' incl \n -> proc (mkSym "tryTick" n)
    $ body
    $ tryTickImpl state interval timeNow
  start <- define' incl \n -> proc (mkSym "start" n)
    $ body
    $ startImpl timeNow state 
  pure $ pure MkTimer
    { tryTick = call tryTick 
    , start = call_ start
    }

newState :: (MonadState Ctx m) => m (State 'Global Ms)
newState = do
  stateRef <- newPrevTimestamp
  pure MkState
    { ref = stateRef
    , getState = deref stateRef
    , putState = store stateRef
    }

data State s t = MkState
  { ref :: Ref s (Stored t)
  , getState :: forall eff. Ivory eff t
  , putState :: forall eff. t -> Ivory eff ()
  }

tryTickImpl :: forall u (s :: RefScope).
  State s Ms ->
  Ms ->
  TimeNow Ms ->
  Ivory (ProcEffects u IBool) ()
tryTickImpl MkState {getState,putState} interval MkTimeNow {timeNow} = do
  now <- timeNow
  previous <- getState
  let timePassed = now - previous
  ifte_ (timePassed >=? interval)
    do
      putState now
      ret true
    (ret false)

startImpl :: forall s eff.
  (eff ~ ProcEffects s ()) =>
  TimeNow Ms ->
  State 'Global Ms ->
  Ivory eff ()
startImpl MkTimeNow {timeNow} MkState {putState} =
  putState =<< timeNow


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
{-# LANGUAGE NamedFieldPuns #-}
module StandaloneWaterSensor.Timeout
  ( toCOutcome
  , Outcome (Done, TimedOut, NotYet)
  , Timeout (MkTimeout, checked, reset)
  , newTimeout
  ) where

import qualified StandaloneWaterSensor.Timer as Timer

import Prelude hiding (init)
import Ivory.Language as Ivory
import Enum (matchEnum)
import Control.Monad.State (MonadState)
import Ctx (Ctx, newMemArea, define', mkSym)

newtype CStatus = MkCStatus { unCStatus :: Uint8 }
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

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

data Timeout = MkTimeout
  { checked :: forall eff. IBool -> Ivory eff COutcome
  , reset :: forall eff. Ivory eff ()
  }

newStatus :: forall m. MonadState Ctx m => m (Ref 'Global (Stored CStatus))
newStatus = addrOf <$> newPrevTimestampArea
  where
    newPrevTimestampArea :: m (MemArea (Stored CStatus))
    newPrevTimestampArea =
      newMemArea "timeout_status" $ Just $ ival 0

newState :: MonadState Ctx m => Timer.Timer -> m State
newState timer = do
  statusRef <- newStatus
  pure MkState
    { timer = timer
    , getStatus = deref statusRef
    , putStatus = store statusRef
    }

data State = MkState
  { timer :: Timer.Timer
  , getStatus :: forall eff. Ivory eff CStatus
  , putStatus :: forall eff. CStatus -> Ivory eff ()
  }

newTimeout ::
  MonadState Ctx m =>
  Timer.Timer ->
  m Timeout
newTimeout timer = do
  state <- newState timer
  checked <- define' incl \n ->
    proc (mkSym "checked" n) \outcome ->
    body $ checkedImpl state outcome
  reset <- define' incl \n ->
    proc (mkSym "reset" n) $ body $ resetImpl state
  pure MkTimeout
    { checked = \outcome -> call checked outcome
    , reset = call_ reset
    }

checkedImpl :: forall s. State -> IBool -> Ivory (ProcEffects s COutcome) ()
checkedImpl MkState {getStatus, putStatus, timer = Timer.MkTimer {tryTick}} outcome = do
  currentStatus <- getStatus
  matchEnum currentStatus \case
    Ticking -> do
      ifte_ outcome
        (ret $ toCOutcome Done)
        do
          tickOutcome <- tryTick
          ifte_ tickOutcome
            do
              putStatus $ toCStatus Expired
              ret $ toCOutcome TimedOut
            (ret $ toCOutcome NotYet)
    Expired -> ret $ toCOutcome TimedOut

resetImpl :: forall s. State -> Ivory (ProcEffects s (COutcome)) ()
resetImpl MkState {putStatus, timer = Timer.MkTimer {start}} = do
  putStatus $ toCStatus Ticking
  start

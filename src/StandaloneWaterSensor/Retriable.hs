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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module StandaloneWaterSensor.Retriable
  ( Result(HaveRetries, AllRetriesFailed, AlreadyFailedAllRetries)
  , Status(RetriesFailed, Retrying)
  , Retriable(failedAgain, reset)
  , newRetriable
  , Attempts (MkAttempts)
  , CResult
  , CStatus
  ) where

import Prelude hiding (init)
import Ivory.Language as Ivory
import Control.Monad.State (MonadState)
import Ctx (Ctx, newMemArea, define', mkSym)
import Enum (enum)

data Result
  = HaveRetries
  | AllRetriesFailed
  | AlreadyFailedAllRetries
  deriving (Bounded, Enum)

newtype CResult = MkCResult Uint8
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data Status
  = RetriesFailed
  | Retrying
  deriving (Bounded, Enum)

newtype CStatus = MkCStatus Uint8 
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data State = MkState
  { getStatus :: forall eff. Ivory eff CStatus
  , getAttempts :: forall eff. Ivory eff Attempts
  , putAttempts :: forall eff. Attempts -> Ivory eff ()
  , putStatus :: forall eff. CStatus -> Ivory eff ()
  }

newStatus :: forall m. MonadState Ctx m => m (Ref 'Global (Stored CStatus))
newStatus = fmap addrOf
  $ newMemArea "Retriable_status"
  $ Just
  $ ival
  $ enum Retrying

newAttempts :: forall m. MonadState Ctx m => m (Ref 'Global (Stored Attempts))
newAttempts = fmap addrOf
  $ newMemArea "Retriable_retriesPassed"
  $ Just
  $ ival 0

newState :: MonadState Ctx m => m State
newState = do
  statusRef <- newStatus
  attemptsRef <- newAttempts
  pure MkState
    { getStatus = deref statusRef
    , getAttempts = deref attemptsRef
    , putStatus = store statusRef
    , putAttempts = store attemptsRef
    }

data Retriable = MkRetriable
  { failedAgain :: forall eff. Ivory eff CResult
  , reset :: forall eff. Ivory eff ()
  }

newtype Attempts = MkAttempts { unAttempts :: Uint8 }
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

newRetriable :: MonadState Ctx m => Attempts -> m Retriable
newRetriable maxAttempts = do
  state <- newState
  reset <- define' incl \n -> proc (mkSym "Retriable_reset" n)
    $ body
    $ resetImpl state
  failedAgain <- define' incl \n -> proc (mkSym "Retriable_failedAgain" n)
    $ body
    $ failedAgainImpl maxAttempts state

  pure MkRetriable
    { reset = call_ reset
    , failedAgain = call failedAgain
    }

resetImpl :: State -> Ivory (ProcEffects s ()) ()
resetImpl MkState {putAttempts, putStatus} = do
  putAttempts 0
  putStatus $ enum Retrying

failedAgainImpl :: Attempts -> State -> Ivory (ProcEffects s CResult) ()
failedAgainImpl maxAttempts MkState {..} = do
  attempts <- getAttempts
  let attempts' = attempts + 1
  putAttempts attempts'
  ifte_
    (attempts'>=? maxAttempts)
    do
      putStatus $ enum RetriesFailed
      ret $ enum AllRetriesFailed
    (ret $ enum HaveRetries)

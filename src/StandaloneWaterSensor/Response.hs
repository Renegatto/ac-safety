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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FieldSelectors #-}
module StandaloneWaterSensor.Response
  ( Outcome(ResponseReceived, ResponseIsNotReady, ShouldRetry, NoResponse) 
  , COutcome
  , ReceiveMsg (MkReceiveMsg, receiveMsg)
  , newResponse
  ) 
  where

import Prelude hiding (init, fail)
import Ivory.Language as Ivory
import Control.Monad.State (MonadState)
import Ctx (Ctx, newMemArea, define', mkSym)
import Enum (enum, matchEnum)
import qualified StandaloneWaterSensor.Timeout as Timeout
import qualified StandaloneWaterSensor.Retriable as Retriable
import Data.Functor ((<&>))
import qualified StandaloneWaterSensor.Timer as Timer

data Outcome
  = ResponseReceived
  | ResponseIsNotReady
  | ShouldRetry
  | NoResponse
  deriving (Bounded, Enum)

newtype COutcome = MkCOutcome Uint8
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data Status
  = NoResponseExpected
  | ResponseAlreadyReceived
  | WaitingForResponse
  deriving (Bounded, Enum)

newtype CStatus = MkCStatus Uint8
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data Response = MkResponse
  { reset :: forall eff. Ivory eff ()
  , receive :: forall eff. Ivory eff COutcome
  }

data State = MkState
  { getStatus :: forall eff. Ivory eff CStatus
  , putStatus :: forall eff. CStatus -> Ivory eff ()
  , timeout :: Timeout.Timeout
  , retries :: Retriable.Retriable
  }

newtype ReceiveMsg = MkReceiveMsg { receiveMsg :: forall eff. Ivory eff IBool }

newStatus :: MonadState Ctx m => m (Ref 'Global (Stored CStatus))
newStatus = fmap addrOf
  $ newMemArea "response_status"
  $ Just
  $ ival
  $ enum WaitingForResponse

newState ::
  MonadState Ctx m =>
  Timeout.Timeout ->
  Retriable.Retriable ->
  m State
newState timeout retries = newStatus <&> \statusRef ->
  MkState
    { getStatus = deref statusRef
    , putStatus = store statusRef
    , timeout
    , retries      
    }

newResponse ::
  MonadState Ctx m =>
  Timer.Timer ->
  Retriable.Attempts ->
  ReceiveMsg ->
  m Response
newResponse timer maxAttempts recv = do
  timeout <- Timeout.newTimeout timer
  retriable <- Retriable.newRetriable maxAttempts
  state <- newState timeout retriable
  receive <- define' incl \n -> proc (mkSym "receive" n)
    $ body
    $ receiveImpl recv state
  reset <- define' incl \n -> proc (mkSym "reset" n)
    $ body
    $ resetImpl state
  pure MkResponse
    { receive = call receive
    , reset = call_ reset
    }

-- impl

succeed :: State -> Ivory eff ()
succeed MkState {putStatus} = putStatus $ enum ResponseAlreadyReceived

fail :: State -> Ivory eff ()
fail MkState {putStatus} = putStatus $ enum NoResponseExpected

tryReceive :: ReceiveMsg -> State -> Ivory (ProcEffects s COutcome) ()
tryReceive MkReceiveMsg {receiveMsg} state@MkState {timeout, retries} = do
--LIBDEBUG(DEBUGLN("WaitingForResponce::receive()"));
  recvOutcome <- receiveMsg
  timeoutOutcome <- Timeout.checked timeout recvOutcome
--LIBDEBUG(DEBUG("timeoutOutcome = "))
--LIBDEBUG(DEBUGSHOW(timeoutOutcome));
  matchEnum timeoutOutcome \case
    Timeout.Done -> do
      succeed state
      ret $ enum ResponseReceived
    Timeout.NotYet ->
      ret $ enum ResponseIsNotReady
    Timeout.TimedOut -> do
      retryOutcome <- Retriable.failedAgain retries
      matchEnum retryOutcome \case
        Retriable.HaveRetries -> do
          Timeout.reset timeout
          ret $ enum ShouldRetry
        Retriable.AllRetriesFailed -> do
--        LIBDEBUG(DEBUGLN("ALL RETRIES FAILED ==========="));
          fail state
          ret $ enum NoResponse;
        Retriable.AlreadyFailedAllRetries -> do
          fail state
          ret $ enum NoResponse
--        LIBDEBUG(DEBUGLN("WaitingForResponce::tryReceive returned "));
--        LIBDEBUG(DEBUGSHOW(outcome));

resetImpl :: State -> Ivory (ProcEffects s COutcome) ()
resetImpl MkState {putStatus, timeout, retries} = do
  putStatus $ enum WaitingForResponse
  Timeout.reset timeout
  Retriable.reset retries

receiveImpl :: ReceiveMsg -> State -> Ivory (ProcEffects s COutcome) ()
receiveImpl recv state@MkState {getStatus} = getStatus >>= flip matchEnum \case
  NoResponseExpected -> ret $ enum NoResponse
  ResponseAlreadyReceived -> ret $ enum ResponseReceived
  WaitingForResponse -> tryReceive recv state

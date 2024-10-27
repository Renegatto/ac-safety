
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
module StandaloneWaterSensor.Connection
  ( Connection
    ( MkConnection
    , send
    , sendRespondable
    , resume
    , receive
    )
  , ResponseOutcome
    ( Received
    , NotYet
    , NothingToReceive
    , NoResponse
    )
  , SendMessageResult
    ( Sent
    , CantSendWhileBusy
    , TakePreviousResponseFirst
    )
  , CSendMessageResult
  , CResponseOutcome
  , Send (MkSend, sendMsg)
  , newConnection
  )
  where

import Prelude hiding (init, fail)
import Ivory.Language as Ivory
import Control.Monad.State (MonadState)
import Ctx (Ctx, newMemArea, define', mkSym)
import Enum (enum, matchEnum)
import qualified StandaloneWaterSensor.Retriable as Retriable
import qualified StandaloneWaterSensor.Timer as Timer
import qualified StandaloneWaterSensor.Response as Response
import Ivory.Language.Cond (Cond)

data Connection msg = MkConnection
  { receive :: forall eff. Ivory eff CResponseOutcome
  , resume :: forall eff. Ivory eff ()
  , sendRespondable :: forall eff. msg -> Ivory eff CSendMessageResult
  , send :: forall eff. msg -> Ivory eff CSendMessageResult
  }

data ResponseOutcome
  = Received
  | NotYet
  | NothingToReceive
  | NoResponse
  deriving (Enum, Bounded)

newtype CResponseOutcome = MkCResponseOutcome Uint8
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data SendMessageResult
  = Sent
  | CantSendWhileBusy
  | TakePreviousResponseFirst
  deriving (Enum, Bounded)

newtype CSendMessageResult = MkCSendMessageResult Uint8
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data Status
  = Available
  | WaitingForResponse
  | HavingResponse
  deriving (Enum, Bounded)

newtype CStatus = MkCStatus Uint8
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

-- Witnesses for formal verification
data Witnesses msg = MkWitnesses
  { statusRef :: Ref 'Global (Stored CStatus)
  , messageRef :: Ref 'Global (Stored msg)
  , emptyMessage :: msg
  }

data State msg = MkState
  { getStatus :: forall eff. Ivory eff CStatus
  , putStatus :: forall eff. CStatus -> Ivory eff ()
  , getMessage :: forall eff. Ivory eff msg
  , putMessage :: forall eff. msg -> Ivory eff ()
  , response :: Response.Response
  }

-- * Instantiation

newtype Send msg = MkSend
  { sendMsg :: forall eff. msg -> Ivory eff ()
  }

newConnection :: forall msg m.
  MonadState Ctx m =>
  IvoryVar msg =>
  IvoryEq msg =>
  IvoryZeroVal msg =>
  IvoryStore msg =>
  Send msg ->
  Response.ReceiveMsg ->
  Timer.Timer ->
  Retriable.Attempts ->
  msg ->
  m (Connection msg)
newConnection sendMsg recv timer attempts noMessage = do
  response <- Response.newResponse timer attempts recv 
  (state,witnesses) <- newState response noMessage
  send <- define' incl \n -> proc (mkSym "Connection_send" n) 
    \msg -> sendImpl witnesses sendMsg state msg
  sendRespondable <- define' incl \n -> proc (mkSym "Connection_sendRespondable" n)
    \msg -> sendRespondableImpl witnesses state (call send) msg
  resume <- define' incl \n -> proc (mkSym "Connection_resume" n)
    $ body
    $ resumeImpl @_ @(ProcEffects _ ()) state
  receive <- define' incl \n -> proc (mkSym "Connection_receive" n)
    $ receiveImpl witnesses sendMsg state
  pure MkConnection
    { send = call send
    , sendRespondable = call sendRespondable
    , resume = call_ resume
    , receive = call receive
    }

newState :: forall msg m.
  MonadState Ctx m =>
  IvoryType msg =>
  IvoryZeroVal msg =>
  IvoryStore msg =>
  Response.Response ->
  msg ->
  m (State msg, Witnesses msg)
newState response emptyMessage = do
  statusRef <- newStatus
  msgRef <- newMessage
  let
    state = MkState
      { response
      , getStatus = deref statusRef
      , putStatus = store statusRef
      , getMessage = deref msgRef
      , putMessage = store msgRef
      }
    witnesses = MkWitnesses {messageRef = msgRef, statusRef, emptyMessage}
  pure (state,witnesses)

newStatus :: MonadState Ctx m => m (Ref 'Global (Stored CStatus))
newStatus = fmap addrOf
  $ newMemArea "Connection_status"
  $ Just
  $ ival
  $ enum Available

newMessage :: (MonadState Ctx m, IvoryType msg, IvoryZeroVal msg) => m (Ref 'Global (Stored msg))
newMessage = fmap addrOf
  $ newMemArea "Connection_message" Nothing

-- * Sending messages

sendImpl ::
  (IvoryEq msg, IvoryVar msg) =>
  Witnesses msg ->
  Send msg ->
  State msg ->
  msg ->
  Body CSendMessageResult
sendImpl witnesses MkSend {sendMsg} MkState {getStatus} msg = 
  send_conditions msg witnesses $ body do
    getStatus >>= flip matchEnum \case
      Available -> do
        sendMsg msg
        ret $ enum Sent
      WaitingForResponse -> ret $ enum CantSendWhileBusy
      HavingResponse -> ret $ enum TakePreviousResponseFirst

send_requires :: IvoryEq msg => msg -> Witnesses msg -> IBool
send_requires msg MkWitnesses {emptyMessage} = msg /=? emptyMessage

send_ensures :: IvoryEq msg => Witnesses msg -> Cond
send_ensures MkWitnesses {messageRef, emptyMessage} = 
  checkStored messageRef \storedMsg -> storedMsg /=? emptyMessage

send_conditions :: (IvoryEq msg, IvoryType a, IvoryVar a) => msg -> Witnesses msg -> Body a -> Body a
send_conditions msg witnesses = 
  requires (send_requires msg witnesses)
    . ensures (const $ send_ensures witnesses)

-- * Sending to get response

sendRespondableImpl ::
  (IvoryEq msg, IvoryVar msg) =>
  Witnesses msg ->
  State msg ->
  (forall eff. msg -> Ivory eff CSendMessageResult) ->
  msg ->
  Body CSendMessageResult
sendRespondableImpl witnesses MkState {putStatus,putMessage,response} send msg =
  send_conditions msg witnesses $ body do
    sent <- send msg
    matchEnum sent \case
      Sent -> do
        putStatus $ enum WaitingForResponse
        putMessage msg
        Response.reset response
      _ -> pure ()
    ret sent

-- * Receiving

receiveImpl ::
  (IvoryEq msg, IvoryVar msg) =>
  Witnesses msg ->
  Send msg ->
  State msg ->
  Body CResponseOutcome
receiveImpl witnesses send state@MkState {getStatus, response} = 
  requires (receive_requires witnesses) $ body do
    -- LIBDEBUG(DEBUGLN("IRConnection::receive()"));
    -- delay(800);
    status <- getStatus
    matchEnum status \case
      Available -> ret $ enum NothingToReceive
      HavingResponse -> ret $ enum Received
      WaitingForResponse -> do
          -- LIBDEBUG(DEBUGLN("IRConnection::receive | WaitingForResponse"));
          -- delay(800);
          attemptOutcome <- Response.receive response
          -- LIBDEBUG(DEBUGLN("IRConnection::receive | WaitingForResponse | got response"));
          handleResponse send state attemptOutcome $ ret . enum
          -- LIBDEBUG(DEBUGLN("IRConnection::receive | WaitingForResponse | handled response"));
    -- LIBDEBUG(DEBUGLN("IRConnection::receive returned: "));
    -- LIBDEBUG(DEBUGSHOW(outcome));

receive_requires :: (IvoryEq msg, IvoryVar msg) =>
  Witnesses msg -> Cond
receive_requires MkWitnesses {messageRef, statusRef, emptyMessage} =
  checkStored statusRef \status ->
  checkStored messageRef \msg ->
    messagePresentUnlessAvailable emptyMessage status msg

messagePresentUnlessAvailable :: IvoryEq msg => msg -> CStatus -> msg -> IBool
messagePresentUnlessAvailable emptyMsg status msg =
   (status /=? enum Available) `implies` (emptyMsg /=? msg)

handleResponse ::
  Send msg ->
  State msg ->
  Response.COutcome ->
  (ResponseOutcome -> Ivory eff ()) ->
  Ivory eff ()
handleResponse MkSend {sendMsg} MkState {putStatus, getMessage} outcome  cont = matchEnum outcome \case
  Response.ResponseReceived -> do
    putStatus $ enum HavingResponse
    cont Received
  Response.ResponseIsNotReady -> do
    putStatus $ enum WaitingForResponse
    cont NotYet
  Response.ShouldRetry -> do
    sendMsg =<< getMessage
    cont NotYet
  Response.NoResponse -> do
    putStatus $ enum Available
    cont NoResponse

-- * Resuming

resumeImpl :: forall msg eff. State msg -> Ivory eff ()
resumeImpl MkState {getStatus, putStatus} = getStatus >>= flip matchEnum \case
  HavingResponse -> putStatus $ enum Available
  _ -> pure ()

-- helpers

implies :: IBool -> IBool -> IBool
implies ante conseq = iNot ante .|| conseq
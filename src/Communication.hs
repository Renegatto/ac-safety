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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Werror=Wno-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns #-}
module Communication
  -- ( Outcome(ResponseReceived, ResponseIsNotReady, ShouldRetry, NoResponse) 
  -- , COutcome
  -- , ReceiveMsg (MkReceiveMsg, receiveMsg)
  -- , newResponse
  -- , Response (MkResponse, reset, receive)
  -- ) 
  where

import Prelude hiding (init, fail)
import Ivory.Language as Ivory
import qualified Message
import qualified Composite.Pack as Pack

-- | Enlarge number size, truncating with zeroes
truncateNumber :: forall t u. (IvoryBits t, SafeCast t u) => t -> u
truncateNumber n = 
  let
    byteSize = 8
    -- highest bit of 'u' is set to 1, others are zeroes
    highestBit = iShiftL @t
      1
      (sizeOf (Proxy @(Stored u)) * byteSize - 1)
    -- mask of 1s of size u, hanving type t
    mask = (iShiftL @t (highestBit - 1) 1) + 1
  in safeCast $ n .& mask

newtype Recipient = MkRecipient
  { unRecipient :: Uint8
  }
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data Message rec cmd pay = MkMessage
  { recipient :: rec
  , cmd     :: cmd
  , payload :: pay
  }
newtype RawMessage = MkRawMessage
  { unRawMessage :: Message Recipient Message.Uint4 Message.Uint4 }

newtype CMessage = MkCMessage Uint16
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

withMessage :: (RawMessage -> Ivory eff a) -> CMessage -> Ivory eff a 
withMessage cont (MkCMessage message)  =
  let
    (MkRecipient -> recipient,cmd,payload) =
      Message.unpackMsg Message.msgSchema message
  in cont $ MkRawMessage $ MkMessage {recipient,cmd,payload}

toCMessage :: RawMessage -> CMessage
toCMessage (MkRawMessage MkMessage {..}) = do
  MkCMessage
    $ Pack.pack
    $ Message.putMessage (unRecipient recipient,cmd,payload) Message.msgSchema

callToCMessage :: RawMessage -> Ivory eff CMessage
callToCMessage (MkRawMessage (MkMessage {recipient,cmd,payload})) =
  call toCMessageFn recipient cmd payload

toCMessageFn :: Def ('[Recipient, Message.Uint4, Message.Uint4] :-> CMessage) 
toCMessageFn = proc "toCMessage" \recipient cmd payload -> body
  $ ret $ toCMessage $ MkRawMessage MkMessage {recipient, cmd, payload}

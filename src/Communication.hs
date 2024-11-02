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
module Communication
  -- ( Outcome(ResponseReceived, ResponseIsNotReady, ShouldRetry, NoResponse) 
  -- , COutcome
  -- , ReceiveMsg (MkReceiveMsg, receiveMsg)
  -- , newResponse
  -- , Response (MkResponse, reset, receive)
  -- ) 
  where

import Prelude hiding (init, fail, append)
import Ivory.Language as Ivory
import Control.Monad.State (MonadState)
import Ctx (Ctx, newMemArea, define', mkSym)
import Enum (enum, matchEnum)
-- import qualified StandaloneWaterSensor.Timeout as Timeout
-- import qualified StandaloneWaterSensor.Retriable as Retriable
import qualified StandaloneWaterSensor.Timer as Timer
import Data.Functor ((<&>))
import qualified StandaloneWaterSensor.Connection as Connection
import GHC.TypeNats (KnownNat, natVal, type (-), type (+), type (<=))
import Data.Bits (shiftL, (.&.), shiftR)
import Data.Type.Ord (Compare, type (>=?))
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Type.Equality ((:~:) (Refl))

data WaterLevel
  = Low
  | Medium
  | High
  | Overflow
  deriving (Bounded, Enum)

data Cfg size msg = MkCfg
  { addressOffset :: size
  , addressMask :: msg

  , cmdOffset :: size
  , cmdMask :: msg

  , dataOffset :: size
  , dataMask :: msg
  }

defaultCfg :: Num n => Cfg n Uint16
defaultCfg = MkCfg
  { addressMask   = 0xff00
  , cmdMask       = 0x00f0
  , dataMask      = 0x000f
  , addressOffset = 8
  , cmdOffset     = 4
  , dataOffset    = 0
  }

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

truncateNumberFn :: forall t u.
  (IvoryBits t, SafeCast t u) => Def ('[t] :-> u)
truncateNumberFn =
  proc "truncate0" \n -> body $ ret $ truncateNumber n

byteToWord :: Def ('[Uint8] :-> Uint16)
byteToWord = truncateNumberFn



type Uint4 = Uint8

data Message = MkMessage
  { recipient :: Uint8
  , cmd     :: Uint4
  , payload :: Uint4
  }

newtype CMessage = MkCMessage Uint16
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

withMessage :: CMessage -> (Message -> Ivory eff a) -> Ivory eff a 
withMessage message cont =
  
  undefined

toCMessage :: Cfg Uint16 Uint16 -> Message -> Ivory (ProcEffects s CMessage) ()
toCMessage MkCfg {..} MkMessage {..} = do
  recipient' <- call byteToWord recipient
  cmd' <- call byteToWord cmd
  payload' <- call byteToWord payload
  ret $ MkCMessage $
    (recipient' `iShiftL` addressOffset)
    + (cmd' `iShiftL` cmdOffset)
    + (payload' `iShiftL` dataOffset)

part :: IvoryBits msg => msg -> msg -> msg -> msg
part offset mask msg = (msg .& mask) `iShiftR` offset

-- (byte offset, word mask, Msg msg) {
--   return (msg & mask) >> offset; 
-- };
address :: IvoryBits msg => Cfg msg msg -> msg -> msg
address MkCfg {addressOffset, addressMask} =
  part addressOffset addressMask

command :: IvoryBits msg => Cfg msg msg -> msg -> msg
command MkCfg {cmdOffset, cmdMask} =
  part cmdOffset cmdMask
--       uint4 command(Msg msg) {
--         return part(CMD_OFFSET,CMD_MASK,msg);
--       };
--       uint4 data(Msg msg) {
--         return part(DATA_OFFSET,DATA_MASK,msg);
--       };
--     }


--     namespace Client {
--       typedef Message::Msg Message;
--       // 4b
--       enum Request {
--         CheckHealth = 0xA,
--         GetWaterLevel = 0xC,
--       };
--       Request getMessageRequest(Message msg) {
--         return Message::command(msg);
--       };
--       Message message(byte recipient, enum Request request) {
--         return Message::message(recipient,request,0);
--       };
--     };

--     namespace Sensor {
--       typedef Message::Msg Message;
--       // 4b
--       enum Response {
--         Alive = 0xB,
--         WaterLevel = 0x3,
--       };
--       typedef Message WaterLevelResponse;
--       typedef Message AliveResponse;

--       WaterLevelResponse makeAliveMessage(byte recipient) {
--         return Message::message(recipient, Response::Alive, 0);
--       };
--       WaterLevelResponse makeWaterLevelMessage(byte recipient, enum WaterLevel level) {
--         return Message::message(recipient, Response::WaterLevel, level);
--       };
--       enum WaterLevel waterLevelFromMessage(WaterLevelResponse resp) {
--         return Message::data(resp);
--       };
--       Response getMessageResponse(Message msg) {
--         return Message::command(msg);
--       };
--     };
--   };
-- };
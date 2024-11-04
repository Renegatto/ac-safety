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
{-# LANGUAGE MultiParamTypeClasses #-}
module Communication.Sensor
  ( WaterLevel
    ( Overflow
    , High
    , Medium
    , Low
    )
  , CWaterLevel(unCWaterLevel)
  , CResponse (MkCResponse, unCResponse)
  , SensorMessage
  -- * Dealing with sensor response
  , Response
    ( ItIsAlive
    , ItsWaterLevel
    )
  , withResponse
  -- * Converting between reprs
  , toWaterLevelMessage
  , toAliveMessage
  -- * Constructing messages
  , iamAlive
  , waterLevelIs
  )
  where

import Prelude hiding (init, fail)
import Ivory.Language as Ivory
import Ivory.Language.Uint (Uint8(Uint8))
import qualified Message
import Communication (Message (MkMessage, recipient, cmd, payload), CMessage, Recipient, RawMessage (MkRawMessage), withMessage)
import Enum (enum, matchEnum)
import Control.Arrow ((>>>))
import Data.Function ((&))

data WaterLevel
  = Low
  | Medium
  | High
  | Overflow
  deriving (Bounded, Enum)

newtype CWaterLevel = MkCWaterLevel
  { unCWaterLevel :: Message.Uint4
  }
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data ResponseTag
  = Alive
  | WaterLevel
  deriving (Enum, Bounded)

newtype CResponse = MkCResponse
  { unCResponse :: Message.Uint4
  }
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

-- * Converting between message representations

newtype SensorMessage a = MkSensorMessage
  { unSensorMessage :: Message Recipient CResponse a
  }

toMessage :: (a -> Message.Uint4) -> SensorMessage a -> RawMessage
toMessage unPayload (MkSensorMessage MkMessage {..}) =
  MkRawMessage
  $ MkMessage
  { recipient
  , cmd = unCResponse cmd
  , payload = unPayload payload
  }

toAliveMessage :: SensorMessage Message.Uint4 -> RawMessage
toAliveMessage = toMessage id

toWaterLevelMessage :: SensorMessage CWaterLevel -> RawMessage
toWaterLevelMessage = toMessage unCWaterLevel

fromMessage :: (Message.Uint4 -> a) -> RawMessage -> SensorMessage a
fromMessage withPayload (MkRawMessage (MkMessage {..})) =
  MkSensorMessage
   $ MkMessage
   { recipient
   , cmd = MkCResponse cmd
   , payload = withPayload payload
   }

-- * Constructing messages

iamAlive :: Recipient -> SensorMessage Uint8
iamAlive recipient =
  MkSensorMessage $ MkMessage { recipient, cmd = enum Alive, payload = 0 }

waterLevelIs :: Recipient -> WaterLevel -> SensorMessage CWaterLevel
waterLevelIs recipient level = MkSensorMessage $ MkMessage
  { recipient
  , cmd = enum WaterLevel
  , payload = enum level
  }

-- * Recovering response

data Response
  = ItIsAlive
  | ItsWaterLevel WaterLevel

withResponse :: CMessage -> ((Recipient, Response) -> Ivory eff ()) -> Ivory eff ()
withResponse msg cont = flip withMessage msg $ fromMessage (&) >>> unSensorMessage >>> \MkMessage {..} ->
  matchEnum cmd \case
    Alive -> cont (recipient,ItIsAlive)
    WaterLevel -> matchEnum (payload MkCWaterLevel) \level ->
      cont (recipient,ItsWaterLevel level)

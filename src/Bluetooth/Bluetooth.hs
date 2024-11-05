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
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bluetooth.Bluetooth
  -- ( BluetoothConnection
  --   ( MkBluetoothConnection
  --   , touch
  --   , take
  --   , withMessage
  --   , send
  --   , peek
  --   , available
  --   )
  -- , Payload (unPayload)
  -- , newState
  -- , newBluetoothConnection
  -- )
  where

import Prelude hiding (init, fail, read, take)
import Ivory.Language as Ivory
import Data.Bits (Bits((.|.), shiftL))
import Communication (truncateNumber)
import Debug (Debug (debugPrintLn, debugPrint), debugNumLn)
import Composite.Pack (takePart')
import Control.Monad (void)
import SoftwareSerial ( SoftwareSerial, Tx (unTx), Rx (unRx), SS, begin )
import Control.Monad.State (MonadState)
import Ctx (Ctx, define', mkSym, newMemArea)
import qualified Bluetooth.Connection  (BluetoothConnection (touch))
import qualified Arduino 
import Arduino (Pin(unPin))

data Config = MkConfig
  { rxPin :: Tx -- = 2
  , txPin :: Rx -- = 3
  , baudRate :: Arduino.BaudRate
  }

data RuntimeDeps = MkRuntimeDeps
  { lib :: SoftwareSerial
  -- , ss :: SoftwareSerial.SS
  , conn :: Bluetooth.Connection.BluetoothConnection
  }

data Bluetooth = MkBluetooth
  { setup :: forall eff. Ivory eff ()
  , loop :: forall eff. Ivory eff ()
  , connection :: Bluetooth.Connection.BluetoothConnection
  }

setupImpl :: forall eff . Config -> RuntimeDeps -> Ivory eff ()
setupImpl MkConfig {baudRate,rxPin,txPin} MkRuntimeDeps {lib} = do
  Arduino.pinMode (unRx txPin) Arduino.iNPUT
  Arduino.pinMode (unTx rxPin) Arduino.oUTPUT
  SoftwareSerial.begin lib baudRate

loopImpl :: forall eff. RuntimeDeps -> Ivory eff ()
loopImpl MkRuntimeDeps {conn} = do
  call_ $ Bluetooth.Connection.touch conn

--     Bluetooth(
--       uint8_t rxPin,
--       uint8_t txPin
--     ): rxPin(rxPin), txPin(txPin) {
--       bt = new SoftwareSerial(txPin,rxPin);
--       conn = new BluetoothConnection(bt);
--     };
-- };

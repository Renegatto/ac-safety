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
module WaterSensor.ControlModule.Client
  -- ( Outcome(ResponseReceived, ResponseIsNotReady, ShouldRetry, NoResponse) 
  -- , COutcome
  -- , ReceiveMsg (MkReceiveMsg, receiveMsg)
  -- , newResponse
  -- , Response (MkResponse, reset, receive)
  -- ) 
  where

import Prelude hiding (init, fail)
import Ivory.Language as Ivory
import Control.Monad.State (MonadState)
import Ctx (Ctx, newMemArea, define', mkSym)
import Enum (enum, matchEnum)
-- import qualified StandaloneWaterSensor.Timeout as Timeout
-- import qualified StandaloneWaterSensor.Retriable as Retriable
import qualified StandaloneWaterSensor.Timer as Timer
import Data.Functor ((<&>))
import qualified StandaloneWaterSensor.Connection as Connection
import qualified Communication.Client
import qualified Communication
import qualified Communication.Sensor
import Debug (DebugMode, Debug (debugPrint, debugln, debugPrintLn))
import qualified Bluetooth.Connection as Bluetooth

newtype Pin = MkPin Uint8
newtype BaudRate = MkBaudRate Uint64
data Bluetooth

data ClientConfig = MkClientConfig
  { btBaudRate :: BaudRate
  , sensorAddress :: Communication.Recipient
  , irPin :: Pin
  , isAliveRequestsInterval :: Timer.Ms
  , waterLevelRequestInterval :: Timer.Ms
  }

clientCfg :: ClientConfig
clientCfg = MkClientConfig
  { btBaudRate = MkBaudRate 9600
  , sensorAddress = Communication.MkRecipient 0x69
  , irPin = MkPin 4
  , isAliveRequestsInterval = Timer.MkMs 10000
  , waterLevelRequestInterval = Timer.MkMs 2000
  }

data Runtime = MkRuntime
  { askingHealthTimer :: Timer.Timer
  , askingWaterLevelTimer :: Timer.Timer
  , bluetooth :: Bluetooth.BluetoothConnection
  , connection :: Connection.Connection Communication.CMessage 
  , handleWaterLevel :: HandleWaterLevel
  , handleSensorDeath :: HandleSensorDeath
  }

makeRequests
  :: forall (dbg :: DebugMode) eff
  . Debug dbg
  => ClientConfig
  -> Runtime
  -> Ivory eff ()
makeRequests MkClientConfig {sensorAddress} MkRuntime {connection, askingHealthTimer, askingWaterLevelTimer} = do
  askWaterLevel <- Timer.tryTick askingWaterLevelTimer
  ifte_ askWaterLevel
    do
      sent <- Connection.sendRespondable connection
        =<< Communication.Client.message
          sensorAddress
          Communication.Client.CheckHealth
      debugPrint @dbg "Sending result:"
      debugln @dbg sent
    do 
      askHealth <- Timer.tryTick askingHealthTimer
      ifte_ askHealth
        do
          sent <- Connection.sendRespondable connection
            =<< Communication.Client.message
              sensorAddress
              Communication.Client.GetWaterLevel
          debugPrint @dbg "Sending result:"
          debugln @dbg sent
        do
          debugPrintLn @dbg "Not going to ask anything yet"

newtype HandleWaterLevel = MkHandleWaterLevel
  { unHandleWaterLevel
    :: forall eff
    . Communication.Sensor.WaterLevel
    -> Ivory eff ()
  }
newtype HandleSensorDeath = MkHandleSensorDeath
  { unHandleSensorDeath :: forall eff. Ivory eff ()
  }

processResponses :: forall (dbg :: DebugMode) eff.
  Debug dbg =>
  Runtime ->
  Ivory eff ()
processResponses
  MkRuntime
    { connection
    , handleWaterLevel
    , handleSensorDeath
    , bluetooth
    } = do

  debugPrintLn @dbg "ControlModule.Client.processResponses";
  outcome <- Connection.receive connection
  matchEnum outcome \case
    Connection.Received -> do
      debugPrintLn @dbg "RECEIVED IR RESPONSE"
      sensorResponse :: Communication.CMessage <-
        fmap (Communication.MkCMessage . Bluetooth.unPayload)
        $ call
        $ Bluetooth.take bluetooth
      Connection.resume connection
      Communication.Sensor.withResponse sensorResponse
        \(_,response) -> case response of
          Communication.Sensor.ItIsAlive ->
            debugPrintLn @dbg "Sensor is alive"
          Communication.Sensor.ItsWaterLevel level ->
            unHandleWaterLevel handleWaterLevel level
    Connection.NoResponse -> do
      debugPrintLn @dbg "SENSOR DIEEEEEED"
      unHandleSensorDeath handleSensorDeath
    Connection.NotYet -> pure ()
    Connection.NothingToReceive -> pure ()
--   default:
--     DEBUG("Unknown response: ");
--     DEBUGSHOW(outcome);
--     break;

loop :: forall dbg eff. Debug dbg => ClientConfig -> Runtime -> Ivory eff ()
loop cfg runtime@MkRuntime {bluetooth} = do
  debugPrintLn @dbg "loop"
  Bluetooth.loop bluetooth
  processResponses runtime
  makeRequests cfg runtime
  debugPrintLn @dbg "endloop"

{-
class Client {
  public:
    ActiveConnection<WaterSensor::Communication::Sensor::Message>* conn;
    virtual void setup();
    virtual void loop();
    virtual ~Client(){};
};

template <
    typename HandleWaterLevel,
    typename HandleSensorDeath,
    typename Recv,
    typename Send
  > class GClient: public Client {
  private:

    Recv receiveIR;
    Send sendIR;

    HandleWaterLevel handleWaterLevel;
    HandleSensorDeath handleSensorDeath;
    -- Timer* askingHealthTimer;
    -- Timer* askingWaterLevelTimer;
    -- Bluetooth* bluetooth;
    void processSensorResponses()
    void makePeriodicRequests()
  public:
    // client needs to await some responses
    -- ActiveConnection<WaterSensor::Communication::Sensor::Message>* conn;

    GClient(
      HandleWaterLevel handleWaterLevel,
      HandleSensorDeath handleSensorDeath,
      Recv receiveIR,
      Send sendIR,
      Bluetooth* bluetooth
    ): bluetooth(bluetooth), receiveIR(receiveIR), sendIR(sendIR), handleSensorDeath(handleSensorDeath), handleWaterLevel(handleWaterLevel) {
      conn =
          new GActiveConnection<WaterSensor::Communication::Sensor::Message,typeof receiveIR, typeof sendIR>(
          10,
          5000,
          sendIR,
          receiveIR
        );
      askingHealthTimer = new Timer(IS_ALIVE_REQUESTS_INTERVAL_MS, millis);
      askingWaterLevelTimer = new Timer(WATER_LEVEL_REQUESTS_INTERVAL_MS, millis);
    };
    void setup() {
      Serial.begin(9600);
      pinMode(IR_PIN, OUTPUT);
      IrSender.begin(IR_PIN);

      bluetooth->setup(BT_BAUD_RATE);
      askingHealthTimer->start();
      askingWaterLevelTimer->start();
      // resetAwait();
    };

};
-}
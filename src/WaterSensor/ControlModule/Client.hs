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

newtype Pin = MkPin Uint8
newtype BaudRate = MkBaudRate Uint64
data Bluetooth
data Message

data DebugMode = DebugEnabled | DebugDisabled

class Debug (mode :: DebugMode) where
  debug :: forall a eff. IvoryType a => a -> Ivory eff ()
  debugln :: forall a eff. IvoryType a => a -> Ivory eff ()
  debugPrint :: forall eff. String -> Ivory eff ()
  debugPrintLn :: forall eff. String -> Ivory eff ()

instance Debug 'DebugEnabled where
  debug :: forall a eff. IvoryType a => a -> Ivory eff ()
  debug = undefined
  debugln :: forall a eff. IvoryType a => a -> Ivory eff ()
  debugln = undefined
  debugPrint :: forall eff. String -> Ivory eff ()
  debugPrint = undefined
  debugPrintLn :: forall eff. String -> Ivory eff ()
  debugPrintLn = undefined

instance Debug 'DebugDisabled where
  debug :: forall a eff. IvoryType a => a -> Ivory eff ()
  debug _ = pure ()
  debugln :: forall a eff. IvoryType a => a -> Ivory eff ()
  debugln _ = pure ()
  debugPrint :: forall eff. String -> Ivory eff ()
  debugPrint _ = pure ()
  debugPrintLn :: forall eff. String -> Ivory eff ()
  debugPrintLn _ = pure ()

data ClientConfig = MkClientConfig
  { btBaudRate :: BaudRate
  , sensorAddress :: Uint16
  , irPin :: Pin
  , isAliveRequestsInterval :: Timer.Ms
  , waterLevelRequestInterval :: Timer.Ms
  }

clientCfg :: ClientConfig
clientCfg = MkClientConfig
  { btBaudRate = MkBaudRate 9600
  , sensorAddress = undefined
  , irPin = MkPin 4
  , isAliveRequestsInterval = Timer.MkMs 10000
  , waterLevelRequestInterval = Timer.MkMs 2000
  }

data Runtime = MkRuntime
  { askingHealthTimer :: Timer.Timer
  , askingWaterLevelTimer :: Timer.Timer
  , bluetooth :: Bluetooth
  , connection :: Connection.Connection Message 
  }

makeRequests :: forall (dbg :: DebugMode) eff.
  Debug dbg =>
  ClientConfig ->
  Runtime ->
  Ivory eff ()
makeRequests MkClientConfig {sensorAddress} MkRuntime {connection, askingHealthTimer, askingWaterLevelTimer} = do
  askWaterLevel <- Timer.tryTick askingWaterLevelTimer
  ifte_ askWaterLevel
    do
      undefined
      -- Connection.sendRespondable
      --   connection
      --   $ Communication.Client.message
      --     sensorAddress
      --     Communication.Client.Request.CheckHealth
    do 
      askHealth <- Timer.tryTick askingHealthTimer
      ifte_ askHealth
        do
          -- Connection.sendRespondable
          --   connection
          --   $ Communication.Client.message
          --     sensorAddress
          --     Communication.Client.Request.GetWaterLevel
          debugPrint @dbg "Sending result:"
        --  debugln @dbg result
        do
          undefined
--     void makePeriodicRequests()
--  {
--       if (askingHealthTimer->tryTick()) {
--         DEBUGLN("Time to ask health")
--         int result = conn->sendRespondable(WaterSensor::Communication::Client::message(SENSOR_ADDRESS, WaterSensor::Communication::Client::Request::CheckHealth));
--         DEBUG("Sending result: ")
--         DEBUGSHOW(result);
--       } else if (askingWaterLevelTimer->tryTick()) {
--         DEBUGLN("Time to ask water level");
--         int result = conn->sendRespondable(WaterSensor::Communication::Client::message(SENSOR_ADDRESS, WaterSensor::Communication::Client::Request::GetWaterLevel));
--         DEBUG("Sending result: ")
--         DEBUGSHOW(result);
--       } else {
--      //   DEBUGLN("Not going to ask anything yet");
--       };
--     };

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
    void processSensorResponses() {
   //   DEBUGLN("Client::processSensorResponses");
      enum Connection::ResponseOutcome outcome = conn->receive();
      switch (outcome) {
        case Connection::ResponseOutcome::Received:
          {
            DEBUGLN("RECEIVED IR RESPONSE");
            WaterSensor::Communication::Sensor::Message sensorResponse = bluetooth->conn->take();
            conn->resume();
            switch (WaterSensor::Communication::Sensor::getMessageResponse(sensorResponse)) {
              case WaterSensor::Communication::Sensor::Response::Alive:
                DEBUGLN("Sensor is alive");
                break;
              case WaterSensor::Communication::Sensor::Response::WaterLevel:
                handleWaterLevel(WaterSensor::Communication::Sensor::waterLevelFromMessage(sensorResponse));
                break;
            };
          };
          break;
        case Connection::ResponseOutcome::NoResponse:
          DEBUGLN("SENSOR DIEEEEEED");
          handleSensorDeath();
          break;
        case Connection::ResponseOutcome::NotYet:
          break;
        case Connection::ResponseOutcome::NothingToReceive:
          break;
        default:
          DEBUG("Unknown response: ");
          DEBUGSHOW(outcome);
          break;
      };
      //Serial.println("Client::processedSensorResponses");
    };
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
    void loop() {
     // DEBUGLN("loop");
      bluetooth->loop();
      processSensorResponses();
      makePeriodicRequests();
     // DEBUGLN("endloop");
    };
};
-}
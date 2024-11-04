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
module Bluetooth.Connection
  ( BluetoothConnection
    ( MkBluetoothConnection
    , touch
    , withMessage
    , send
    , peek
    , available
    )
  , newState
  , newBluetoothConnection
  )
  where

import Prelude hiding (init, fail, read, take)
import Ivory.Language as Ivory
import Data.Bits (Bits((.|.), shiftL))
import Communication (truncateNumber)
import Debug (Debug (debugPrintLn, debugPrint), debugNumLn)
import Composite.Pack (takePart')
import Control.Monad (void)
import SoftwareSerial
import Control.Monad.State (MonadState)
import Ctx (Ctx, define', mkSym, newMemArea)

byteSize :: Int
byteSize = 8

mESSAGE_START :: Integer
mESSAGE_START = 0xA9

mESSAGE_END :: Integer
mESSAGE_END   = 0xAA

mESSAGE_MASK :: Integer
mESSAGE_MASK  = 0xFF0000FF

eMPTY_MESSAGE :: Integer
eMPTY_MESSAGE =
  mESSAGE_END `shiftL` (byteSize * 3 :: Int)
    -- a word of a message payload in between
  .|. mESSAGE_START

long :: Integer -> Uint64
long = fromInteger

longInt :: Int -> Uint64
longInt = long . toEnum

isMessageImpl :: Uint64 -> IBool
isMessageImpl msg =
  (long mESSAGE_MASK .& msg) ==? long eMPTY_MESSAGE

insert :: Uint8 -> Uint64 -> Uint64
insert x xs =
  let xs' = xs `iShiftR` longInt byteSize
  in (truncateNumber x `iShiftR` (longInt byteSize * 3)) + xs'

touchImpl
  :: forall dbg s
  . Debug dbg
  => State
  -> Ivory (ProcEffects s IBool) ()
touchImpl MkState {putBuf, getBuf, bluetooth}= do
  tryRead bluetooth \case
    Nothing -> ret false
    Just btByte -> do
      debugPrintLn @dbg "Wrote byte"
      putBuf
        . MkBuffer
        . insert btByte
        . unBuffer
        =<< getBuf
      buf' <- getBuf
      debugPrint @dbg "[bluetooth] Buf: "
      debugNumLn @dbg buf' 16
      ret true

newtype Buffer = MkBuffer
  { unBuffer :: Uint64
  }
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

newtype Payload = MkPayload Uint16
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

data State = MkState
  { getBuf :: forall eff. Ivory eff Buffer
  , putBuf :: forall eff. Buffer -> Ivory eff ()
  , bluetooth :: SoftwareSerial
  }

newBuf :: MonadState Ctx m => m (Ref 'Global (Stored Buffer))
newBuf = fmap addrOf
  $ newMemArea "BluetoothConnection_buffer"
  $ Just
  $ ival 0

newState :: forall m.
  MonadState Ctx m =>
  SoftwareSerial ->
  m State
newState bluetooth = do
  buf <- newBuf
  pure MkState
    { getBuf = deref buf
    , putBuf = store buf
    , bluetooth
    }

newBluetoothConnection
  :: forall m dbg
  . MonadState Ctx m
  => Debug dbg
  => State
  -> m BluetoothConnection
newBluetoothConnection state@MkState {bluetooth} = do
  isMessage <- define' incl \n ->
    proc (mkSym "BluetoothConnection_isMessage" n) \msg ->
      body $ ret $ isMessageImpl msg
  send <- define' incl \n ->
    proc (mkSym "BluetoothConnection_send" n) \payload ->
      body @() $ sendImpl @dbg bluetooth payload
  peek <- define' incl \n ->
    proc (mkSym "BluetoothConnection_peek" n)
      $ body $ ret =<< peekImpl state
  touch <- define' incl \n ->
    proc (mkSym "BluetoothConnection_touch" n)
      $ body $ touchImpl @dbg state
  available <- define' incl \n ->
    proc (mkSym "BluetoothConnection_available" n)
      $ body $ ret =<< availableImpl (call isMessage) state
  pure MkBluetoothConnection
    { withMessage =
        withMessageImpl
          (call isMessage)
          (call peek)
          state
    , send
    , available
    , peek
    , touch
    } 

data BluetoothConnection = MkBluetoothConnection
  { send :: Def ('[Payload] :-> ())
  , peek :: Def ('[] :-> Payload)
  , touch :: Def ('[] :-> IBool) -- true if received
  , withMessage
    :: forall eff
    . (Maybe Payload -> Ivory eff ())
    -> Ivory eff ()
  , available :: Def ('[] :-> IBool)
  }

availableImpl ::
  (forall eff'. Uint64 -> Ivory eff' IBool) ->
  State ->
  Ivory eff IBool
availableImpl isMessage MkState { getBuf } =
  isMessage . unBuffer =<< getBuf

withMessageImpl
  :: (forall eff'. Uint64 -> Ivory eff' IBool)
  -> (forall eff'. Ivory eff' Payload)
  -> State
  -> (Maybe Payload -> Ivory eff ())
  -> Ivory eff ()
withMessageImpl isMessage peek state cont = do
  available <- availableImpl isMessage state
  ifte_ available
    do
      payload <- takePayload peek state
      cont $ Just payload
    $ cont Nothing

sendImpl :: forall dbg eff. Debug dbg => SoftwareSerial -> Payload -> Ivory eff ()
sendImpl bluetooth payload@(MkPayload payloadByte) = do
  debugPrintLn @dbg ("[bluetooth] Send ");
  debugNumLn @dbg payload 16

  void $ write bluetooth $ fromInteger @Uint8 mESSAGE_START
  void $ write bluetooth . bitCast @Uint16 @Uint8
    $ takePart' @8 @1 payloadByte
  void $ write bluetooth . bitCast @Uint16 @Uint8
    $ takePart' @8 @5 payloadByte
  void $ write bluetooth $ fromInteger @Uint8 mESSAGE_END

payloadOf :: Buffer -> Payload
payloadOf buf = MkPayload
  $ bitCast
  $ (unBuffer buf .& iComplement (long mESSAGE_MASK))
    `iShiftR` longInt byteSize 

-- Returns the message content, if any, not changing the state
peekImpl :: State -> Ivory eff Payload
peekImpl MkState {getBuf} = payloadOf <$> getBuf

-- Takes the data part of the message, flushing the buffer;
takePayload
  :: (forall eff. Ivory eff Payload)
  -> State
  -> Ivory eff' Payload
takePayload peek MkState {putBuf} = do
  payload <- peek
  putBuf 0x0
  pure payload


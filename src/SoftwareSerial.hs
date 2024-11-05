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
{-# OPTIONS_GHC -Wall -Werror=Wno-incomplete-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SoftwareSerial
  ( SoftwareSerial
    ( MkSoftwareSerial
    , write
    , tryRead
    , begin
    )
  , handleError
  , BaudRate
  , Rx (MkRx, unRx)
  , Tx (MkTx, unTx)
  , SS
  )
  where

import Prelude hiding (init, fail, read, take)
import Ivory.Language as Ivory
import Ivory.Language.Uint (Uint8(Uint8))
import Enum (enum)
import Data.Bits (Bits((.|.), shiftL))
import Communication (truncateNumber)
import Debug (Debug (debugPrintLn, debugNum, debugPrint), debugNumLn)
import Ivory.Stdlib (when)
import Composite.Pack (takePart')
import Control.Monad (void)
import Data.Bifunctor (Bifunctor(second))
import Ivory.Language.Proc (WrapIvory(wrap))
import Arduino (Pin, Size_t, BaudRate)

-- #include <SoftwareSerial.h>

data SS

-- newFn :: Def ('[Uint8,Uint8] :-> SS)
-- newFn =
--   importProc
--     @('[Uint8,Uint8] :-> SS)
--     "SSBridge.h"
--     "newSS"

-- writeFn :: Def ('[SS, Uint8] :-> Size_t)
-- writeFn =
--   importProc
--     @('[SS, Uint8] :-> Size_t)
--     "SSBridge.h"
--     "writeSS"

-- readFn :: Def ('[SS] :-> Sint32)
-- readFn =
--   importProc
--     @('[SS] :-> Sint32)
--     "SSBridge.h"
--     "readSS"

-- softwareSerialLib :: SoftwareSerialLib
-- softwareSerialLib = MkSoftwareSerialLib
--   { lib_write = call writeFn
--   , lib_read = call readFn
--   }


newtype Rx = MkRx { unRx :: Pin }
newtype Tx = MkTx { unTx :: Pin }


data SoftwareSerialLib = MkSoftwareSerialLib
  { lib_write
    :: forall eff
    . SS
    -> Uint8
    -> Ivory eff Size_t
  , lib_read
    :: forall eff
    . SS
    -> Ivory eff Sint32 -- either -1 or Uint8
  , lib_begin
    :: forall eff
    . SS
    -> BaudRate
    -> Ivory eff ()
  , new
    :: forall eff
    . (Rx,Tx)
    -> Ivory eff SS
    -- note that it's library order;
    -- OUR tx and rx pins are in REVERSE order
    -- new SoftwareSerial(txPin,rxPin)
  }

data SoftwareSerial = MkSoftwareSerial
  { write
    :: forall eff
    . Uint8
    -> Ivory eff Size_t
  , tryRead
    :: forall eff
    . (Maybe Uint8 -> Ivory eff ())
    -> Ivory eff ()
  , begin
    :: forall eff
    . BaudRate
    -> Ivory eff ()
  }

tryReadImpl
  :: SoftwareSerialLib
  -> SS
  -> (Maybe Uint8 -> Ivory eff ())
  -> Ivory eff ()
tryReadImpl ssLib ss cont = do
  btByteOrErr <- lib_read ssLib ss
  handleError btByteOrErr
    $ fmap cont
    $ either (const Nothing)
    $ Just . bitCast @Uint32 @Uint8

handleError
  :: Sint32
  -> (Either Uint32 Uint32 -> Ivory eff ())
  -> Ivory eff ()
handleError valueOrErrorCode cont =
  let unsigned = signCast $ abs valueOrErrorCode
  in ifte_ (valueOrErrorCode <? 0)
    (cont $ Left unsigned)
    (cont $ Right unsigned)
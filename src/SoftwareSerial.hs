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
    )
  , handleError
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

-- #include <SoftwareSerial.h>

type Size_t = Uint16 -- size_t = at least 16 bit

data SoftwareSerialLib = MkSoftwareSerialLib
  { lib_write
    :: forall eff
    . Uint8
    -> Ivory eff Size_t
  , lib_read
    :: forall eff
    . Ivory eff Sint32 -- either -1 or Uint8
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
  }

tryReadImpl
  :: SoftwareSerialLib
  -> (Maybe Uint8 -> Ivory eff ())
  -> Ivory eff ()
tryReadImpl ss cont = do
  btByteOrErr <- lib_read ss
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
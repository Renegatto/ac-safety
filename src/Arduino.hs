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
module Arduino where

import Prelude hiding (init, fail)
import Ivory.Language as Ivory



type Size_t = Uint16 -- size_t = at least 16 bit

newtype BaudRate = MkBaudRate
  { unBaudRate :: Uint64
  }

newtype Pin = MkPin
  { unPin :: Uint8
  }

newtype PinMode = UnsafePinMode Uint8

iNPUT :: PinMode
iNPUT = undefined

oUTPUT :: PinMode
oUTPUT = undefined

iNPUT_PULLUP :: PinMode
iNPUT_PULLUP = undefined

pinMode :: Pin -> PinMode -> Ivory eff ()
pinMode = undefined
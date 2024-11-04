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
module Communication.Client
  ( Request
    ( CheckHealth
    , GetWaterLevel
    )
  , CRequest (MkCRequest, unCRequest)
  , ClientMessage
  , getMessageRequest
  , message
  )
  where

import Prelude hiding (init, fail)
import Ivory.Language as Ivory
import Ivory.Language.Uint (Uint8(Uint8))
import qualified Message
import Communication (Message (MkMessage, recipient, cmd, payload), CMessage, Recipient, RawMessage (MkRawMessage, unRawMessage), callToCMessage, withMessage)
import Enum (enum)

data Request
  = CheckHealth
  | GetWaterLevel
  deriving (Enum, Bounded)

newtype CRequest = MkCRequest
  { unCRequest :: Message.Uint4
  }
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

newtype ClientMessage = MkClientMessage
  { unClientMessage :: Message Recipient CRequest Uint8
  }

getMessageRequest :: CMessage -> Ivory eff CRequest
getMessageRequest = withMessage
  $ pure . MkCRequest . cmd . unRawMessage

message :: Recipient -> Request -> Ivory eff CMessage
message recipient req = callToCMessage
  $ MkRawMessage
  $ MkMessage
  { recipient, cmd = enum req, payload = 0 }
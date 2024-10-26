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
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
module Ctx where

import Prelude hiding (init)
import Ivory.Language as Ivory
import Enum (matchEnum)
import Ivory.Language.Pointer (Nullability(Nullable, Valid), Constancy (Mutable), unsafePointerCast)
import Control.Monad.State (MonadState (get, state), modify, gets)
import Ivory.Language.Syntax.Names (Sym)

data Ctx = MkCtx
  { definitions :: ModuleDef
  , defCounter :: Int
  }

define :: forall m. MonadState Ctx m => (Int -> ModuleDef) -> m Int
define def = do
  modify \s -> s
    { definitions = s.definitions <> def s.defCounter
    , defCounter = s.defCounter + 1
    }
  gets (.defCounter)

define' :: forall a m.
  MonadState Ctx m =>
  (a -> ModuleDef) ->
  (Int -> a) ->
  m a
define' def instantiate = do
  state \s ->
    let
      a = instantiate s.defCounter
    in
    ( a
    , s
      { definitions = s.definitions <> def a
      , defCounter = s.defCounter + 1
      }
    )

data Timer = MkTimer
  { tryTick :: forall eff. Ivory eff IBool
  , start :: forall eff. Ivory eff ()
  -- , newLocal :: forall eff (s :: RefScope).
  --     GetAlloc eff ~ Scope s =>
  --     Ivory eff (t (Stack s))
  -- , newGlobal :: forall eff (s :: RefScope).
  --     GetAlloc eff ~ Scope s =>
  --     Ivory eff (t Global)
  }

newMemArea ::
  forall area m.
  MonadState Ctx m =>
  IvoryArea area =>
  IvoryZero area =>
  Sym ->
  Maybe (Init area) ->
  m (MemArea area)
newMemArea name initializer =
  define' defMemArea \n -> area (mkSym name n) initializer

mkSym :: Sym -> Int -> Sym
mkSym prefix idx = prefix <> "_" <> show idx
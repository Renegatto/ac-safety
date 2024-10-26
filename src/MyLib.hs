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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
module MyLib (someFunc) where

import Ivory.Language as Ivory
import Ivory.Language.Pointer (Pointer, KnownConstancy, Nullability(Valid))
import qualified Ivory.Language.Proc as Proc
import Data.Void (Void, absurd)
import Data.Kind (Type)
import Control.Arrow ((>>>))
import Unsafe.Coerce (unsafeCoerce)
import qualified Ivory.Language.Pointer as Pointer
import Sleepiness (CSleepiness, Sleepiness (Sleepy, Energetic, Insomniac, Lethargic))
import qualified StandaloneWaterSensor.Timer as Uuuh
import Enum (matchEnum)
import qualified StandaloneWaterSensor.Timer as Timer
import qualified StandaloneWaterSensor.Timeout as Timeout
import qualified Ctx
import Control.Monad.State (State, runState)
import Ctx (define)
import qualified Ivory.Language.Syntax.AST as AST

newtype Ms = MkMs { unMs :: Uint64 }
  deriving newtype (IvoryType, IvoryVar)

touch :: forall s.
  Ms ->
  ProcPtr ('[Ms] :-> ()) ->
  ProcPtr ('[Ref s (Stored (CSleepiness))] :-> ()) ->
  Def ('[Ref s (Stored CSleepiness)] :-> ())
touch sleepDuration sleepFor toTouch = proc "touch" \state -> body do
  indirect_ toTouch state
  state' <- deref state
  matchEnum state' \case
    Energetic -> retVoid
    Insomniac -> retVoid
    Sleepy -> indirect_ sleepFor sleepDuration
    Lethargic -> indirect_ sleepFor (MkMs (-1))


serialPrint :: forall a. ProcPtr ('[a] :-> ())
serialPrint = Proc.ProcPtr $ AST.NameSym "Serial.println"

makeTimeout :: State Ctx.Ctx ()
makeTimeout = do
  timer <- Timer.newTimer
    (Timer.MkTimeNow undefined)
    (Timer.MkMs 2_000)
  Timeout.MkTimeout {Timeout.checked,Timeout.reset}
    <- Timeout.newTimeout timer
  let
    fn = proc "main" $ body do
      checked true >>= flip matchEnum \case
        Timeout.Done -> indirect_ serialPrint (1 :: Uint8)
        Timeout.NotYet -> indirect_ serialPrint (0 :: Uint8)
        Timeout.TimedOut -> do
          indirect_ serialPrint (-1 :: Uint8)
          reset
      retVoid
  _ <- define $ const $ incl fn
  pure ()

someFunc :: IO ()
someFunc = do
  let (_,Ctx.MkCtx {Ctx.definitions}) = runState makeTimeout Ctx.initial
   --   text = showModule $ compileModule $ package "MyModule" definitions
  --writeFile "code.c" text
  putStrLn "someFunc"

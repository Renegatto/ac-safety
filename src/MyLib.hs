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
module MyLib (someFunc) where

import {-qualified-} Ivory.Language as Ivory
import Ivory.Language.Pointer (Pointer, KnownConstancy, Nullability(Valid))
import qualified Ivory.Language.Proc as Proc
import Data.Void (Void, absurd)
import Data.Kind (Type)
import Control.Arrow ((>>>))
import Unsafe.Coerce (unsafeCoerce)
import qualified Ivory.Language.Pointer as Pointer
import Sleepiness (CSleepiness, matchSleepiness, Sleepiness (SleepyT, EnergeticT, InsomniacT, LethargicT), SleepinessLevel)

newtype Ms = MkMs { unMs :: Uint64 }
  deriving newtype (IvoryType, IvoryVar)

touch :: forall (sleepiness :: SleepinessLevel) s.
  Ms ->
  ProcPtr ('[Ms] :-> ()) ->
  (forall (sl :: SleepinessLevel).
    ProcPtr ('[Ref s (Stored (CSleepiness sl))] :-> ())
  ) ->
  Def ('[Ref s (Stored (CSleepiness sleepiness))] :-> ())
touch sleepDuration sleepFor toTouch = proc "touch" \state -> body do
  indirect_ toTouch state
  state' <- deref state
  matchSleepiness @sleepiness state' \case
    EnergeticT -> retVoid
    InsomniacT -> retVoid
    SleepyT -> indirect_ sleepFor sleepDuration
    LethargicT -> indirect_ sleepFor (MkMs (-1))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

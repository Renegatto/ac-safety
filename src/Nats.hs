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

module Nats where
import GHC.TypeNats (type (+), type (-))

data N = S N | Z

type family NToNat (n :: N) = s where
  NToNat Z = 0
  NToNat (S n) = 1 + NToNat n

data Gt a b where
  GtBase :: Gt (S a) Z
  GtStep :: Gt a b -> Gt (S a) (S b) 
  
data Nat (n :: N) where
  Zero :: Nat Z
  Succ :: Nat n -> Nat (S n)

type family a :+: b where
  S a :+: b = S (a :+: b)
  Z   :+: b = b

type family N2S n where
  N2S 0 = Z
  N2S n = S (N2S (n - 1))  

type family S2N n where
  S2N Z = 0
  S2N (S n) = 1 + (S2N n)

stoInt :: Nat n -> Int
stoInt Zero = 0
stoInt (Succ n) = 1 + stoInt n

class Materialize n where
  materialize :: Nat n
instance Materialize Z where
  materialize = Zero
instance Materialize n => Materialize (S n) where
  materialize = Succ (materialize @n)

natToInt :: Nat n -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

add :: Nat a -> Nat b -> Nat (a :+: b)
add Zero b = b
add (Succ a) b = Succ (add a b)

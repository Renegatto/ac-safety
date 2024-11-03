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
module Composite.Type
  ( Composite (Single, Prepend)
  , begin
  , append
  -- * Transformations
  , apply
  , compositeNatTrans
  -- * Instances
  , CompositeF (MkCompositeF, unCompositeF)
  , withComposite
  )
  where

import Prelude hiding (init, fail)
import Ivory.Language as Ivory
import Data.Bits (shiftL, (.&.), shiftR, Bits ((.|.)))
import Data.Functor.Const (Const (Const))
import Control.Arrow ((>>>))
import Control.Monad.Identity (Identity(Identity))
import Communication (Uint4)
import Nats

data Composite hi lo f xs where
  Single :: forall size plo a f
    . Nat size
    -> Nat (S plo)
    -> f a
    -> Composite (plo :+: size) (S plo) f '[ '(S plo,a)]
  Prepend
    :: forall hi1 hi0 lo0 f {x} {y} xs
    .  Composite hi1 (S hi0) f '[x]
    -> Composite hi0 lo0 f (y:xs)
    -> Composite hi1 lo0 f (x:y:xs)

-- * Construction

begin
  :: forall size n f {psize}
  . ( size ~ S psize
    , Materialize size
    )
  => f n
  -> Composite size (N2S 1) f '[ '(N2S 1,n) ]
begin = Single (materialize @size) materialize

append
  :: forall size n f hi0 lo0 ns y {psize}
  . ( size ~ S psize
    , Materialize hi0
    , Materialize size
    )
  => f n
  -> Composite hi0 lo0 f (y:ns)
  -> Composite (hi0 :+: size) lo0 f ( '(S hi0,n):y:ns)
append = Prepend . Single size (Succ hi0)
  where
    size = materialize @size
    hi0 = materialize @hi0

-- * Transformations

apply :: forall f g h hi lo ns.
  (forall a. f a -> g a -> h a) ->
  Composite hi lo f ns ->
  Composite hi lo g ns ->
  Composite hi lo h ns
apply f (Single size lo x) (Single _ _ y) = Single size lo (f x y)
apply f
  (Prepend x0@(Single _ _ _) x1)
  (Prepend y0@(Single _ _ _) y1) =
    Prepend (apply f x0 y0) (apply f x1 y1)

compositeNatTrans ::
  (forall a. f a -> g a) ->
  Composite hi lo f ns ->
  Composite hi lo g ns
compositeNatTrans f (Single size lo x) = Single size lo (f x)
compositeNatTrans f (Prepend x xs) =
  Prepend (compositeNatTrans f x) (compositeNatTrans f xs)

-- * Instances

newtype CompositeF hi lo ns a = MkCompositeF
  { unCompositeF :: Composite hi lo (Const a) ns
  }

withComposite
  :: (CompositeF hi lo ns a -> CompositeF hi' lo' ns' b)
  -> Composite hi lo (Const a) ns
  -> Composite hi' lo' (Const b) ns'
withComposite f = unCompositeF . f . MkCompositeF 

instance Functor (CompositeF hi lo ns) where
  fmap f (MkCompositeF c) = MkCompositeF case c of
    Single size lo (Const x) -> Single size lo (Const $ f x)
    Prepend a b -> Prepend
      (withComposite (fmap f) a)
      (withComposite (fmap f) b)

instance Foldable (CompositeF hi lo xs) where
  foldMap f = unCompositeF >>> \case
    Single _ _ (Const n) -> f n
    Prepend a b -> foldMap f (MkCompositeF a) <> foldMap f (MkCompositeF b)

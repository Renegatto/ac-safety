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
module Packing
  ( Composite (Single, Prepend)
  , begin
  , append
  -- * Packing/unpacking
  , pack
  , unpack
  -- * Instances
  , CompositeF (MkCompositeF, unCompositeF)
  , withComposite
  -- * Part
  , Part (MkPart, sizeBits, offset, mask)
  , part
  , makePart
  , takePart
  )
  where

import Prelude hiding (init, fail, append)
import Ivory.Language as Ivory
import Control.Monad.State (MonadState)
import Ctx (Ctx, newMemArea, define', mkSym)
import Enum (enum, matchEnum)
-- import qualified StandaloneWaterSensor.Timeout as Timeout
-- import qualified StandaloneWaterSensor.Retriable as Retriable
import qualified StandaloneWaterSensor.Timer as Timer
import Data.Functor ((<&>))
import qualified StandaloneWaterSensor.Connection as Connection
import GHC.TypeNats (KnownNat, natVal, type (-), type (+), type (<=))
import Data.Bits (shiftL, (.&.), shiftR)
import Data.Type.Ord (Compare, type (>=?))
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Type.Equality ((:~:) (Refl))
import Control.Arrow ((>>>))
import Data.Foldable (Foldable(fold))


data Composite hi lo f xs where
  Single :: forall hi lo a f
    . ( KnownNat hi
      , KnownNat lo
      , (hi >=? lo) ~ 'True 
      , Compare lo 0 ~ 'GT
      )
    => f a
    -> Composite hi lo f '[a]
  Prepend
    :: forall hi1 hi0 lo0 f x xs
    . Composite hi1 (hi0 + 1) f '[x]
    -> Composite hi0 lo0 f xs
    -> Composite hi1 lo0 f (x:xs)

pack
  :: forall hi lo ns
  . Composite hi lo (Const Integer) ns
  -> Integer
pack (Prepend x xs) = pack x + pack xs
pack (Single n) = makePart (part @hi @lo) $ toInteger n

unpack
  :: forall hi lo ns f g
  . (forall a. f a -> Integer -> g a)
  -> Composite hi lo f ns
  -> Integer
  -> Composite hi lo g ns
unpack apply = flip \packed -> \case
  Single @hi' @lo' n ->
    Single $ apply n $ takePart (part @hi' @lo') packed
  Prepend x xs ->
    Prepend (unpack apply x packed) (unpack apply xs packed)

begin
  :: forall size n f
  . ( Compare size 1 ~ 'GT
    , KnownNat size
    )
  => f n
  -> Composite size 1 f '[n]
begin = Single 

append
  :: forall size n f hi lo ns {lo'} {hi'}
  . ( lo' ~ hi + 1
    , hi' ~ hi + size
    , KnownNat hi'
    , KnownNat lo'
    , Compare hi' lo' ~ 'GT
    , Compare lo' 0 ~ 'GT
    )
  => f n
  -> Composite hi lo f ns
  -> Composite hi' lo f (n:ns)
append n = Prepend (Single @hi' @lo' n)   

-- * Part

data Part hi lo = MkPart
  { offset :: Int
  , sizeBits :: Int
  , mask :: Integer
  }

takePart :: Part hi lo -> Integer -> Integer
takePart MkPart {offset, mask} src = (src .&. mask) `shiftR` offset  

makePart :: Part hi lo -> Integer -> Integer
makePart MkPart {offset, mask} n =
  (n `shiftL` offset) .&. mask

part :: forall hi lo.
  ( KnownNat hi
  , KnownNat lo
  , Compare lo 0 ~ 'GT 
  ) =>
  Part hi lo
part = MkPart {offset, sizeBits, mask}
  where
    mask = shiftL template offset
    template :: Integer
    template = 2^sizeBits - 1
    sizeBits = hi - offset
    hi = fromEnum $ natVal @hi Proxy
    offset = fromEnum (natVal @lo Proxy) - 1

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
    Single @u @b (Const x) -> Single @u @b (Const $ f x)
    Prepend a b -> Prepend
      (withComposite (fmap f) a)
      (withComposite (fmap f) b)

instance Foldable (CompositeF hi lo xs) where
  foldMap f = unCompositeF >>> \case
    Single (Const n) -> f n
    Prepend a b -> foldMap f (MkCompositeF a) <> foldMap f (MkCompositeF b)

-- * Existentials

-- data SomeComposite lo f xs = forall hi. MkSomeComposite
--   (Composite hi lo f xs)

-- pop ::
--   Composite hi lo Identity (n : ns) ->
--   ( n
--   , Either
--     (SomeComposite lo Identity ns)
--     (ns :~: '[])
--   )
-- pop (Prepend x xs) =
--   case x of
--   Prepend _ ys -> case ys of {}
--   Single n -> (runIdentity n, Left $ MkSomeComposite xs)

-- * Examples

partJ  = part @8 @5

collectedJJ = fold
  $ pure @[] <$> MkCompositeF unpackedJJ

unpackedJJ = unpack (const Const) jj (pack jj)

jj = append @8 @Uint8 (n 251)
  $ append @4 @Bool (n 13)
  $ begin @4 @Int (n 7)
  where
    n = Const @Integer

k =
  append @8 @Uint8 cu
  $ append @4 @Bool cu
  $ begin @4 @String cu
  where
    cu = Const ()

j = Single @19 @17 $ Const @_ @String ()

--msgStructure :: Composite 16 0 (Const ())'[Uint16, Uint4, Bool]
msgStructure = Prepend (Single @19 @17 $ Const @_ @String ())
  $ Prepend (Single @16 @9 $ Const ())
  $ Prepend (Single @8 @5 @String $ Const ()) -- 0x11110000
  $ Single @4 @1 @Bool $ Const ()

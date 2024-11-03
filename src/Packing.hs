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

import Prelude hiding (init, fail)
import Ivory.Language as Ivory
import GHC.TypeNats (KnownNat, natVal, type (-), type (+), type (<=))
import Data.Bits (shiftL, (.&.), shiftR)
import Data.Type.Ord (Compare)
import Data.Functor.Const (Const (Const))
import Control.Arrow ((>>>))
import Data.Foldable (Foldable(fold))

data Composite hi lo f xs where
  Single :: forall size a hi lo f
    . ( KnownNat hi
      , KnownNat lo
      , KnownNat size
      , hi - lo + 1 ~ size
      , Compare hi lo ~ 'GT
      , Compare lo  0 ~ 'GT
      )
    => f a
    -> Composite hi lo f '[ '(size,a)]
  Prepend
    :: forall hi1 hi0 lo0 f {x} {y}  xs
    .  Composite hi1 (hi0 + 1) f '[x]
    -> Composite hi0 lo0 f (y:xs)
    -> Composite hi1 lo0 f (x:y:xs)

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
  Single @_ @_ @hi' @lo' n ->
    Single $ apply n $ takePart (part @hi' @lo') packed
  Prepend x xs ->
    Prepend (unpack apply x packed) (unpack apply xs packed)

begin
  :: forall size n f
  . ( KnownNat size
    , size - 1 + 1 ~ size
    , Compare size 1 ~ 'GT
    )
  => f n
  -> Composite size 1 f '[ '(size,n) ]
begin = Single

append
  :: forall size n f hi0 lo0 ns y {lo1} {hi1}
  . ( lo1 ~ hi0 + 1
    , hi1 - lo1 + 1 ~ size

    , KnownNat hi1
    , KnownNat lo1
    , KnownNat size

    , Compare hi1 lo1 ~ 'GT
    , Compare lo1 0 ~ 'GT
    )
  => f n
  -> Composite hi0 lo0 f (y:ns)
  -> Composite hi1 lo0 f ( '(size,n):y:ns)
append = Prepend . Single

-- -- * Part

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

unpackMsg :: forall a
  . Composite 16 1 (Const a) -- 16 bits size in total
   '[ '(8,Integer) -- 8 bits size
    , '(4,Integer) -- 4 bits size
    , '(4,Integer) -- 4 bits size
    ]
  -> Integer -- integer to extract these pieces from
  -> (Integer,Integer,Integer)
unpackMsg schema n =
  case unpack (const Const) schema n of
    Prepend (Single (Const n3))
      (Prepend
        (Single (Const n2))
        (Single (Const n1))) -> (n3,n2,n1)

-- -- * Instances

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

-- -- * Existentials

-- -- data SomeComposite lo f xs = forall hi. MkSomeComposite
-- --   (Composite hi lo f xs)

-- -- pop ::
-- --   Composite hi lo Identity (n : ns) ->
-- --   ( n
-- --   , Either
-- --     (SomeComposite lo Identity ns)
-- --     (ns :~: '[])
-- --   )
-- -- pop (Prepend x xs) =
-- --   case x of
-- --   Prepend _ ys -> case ys of {}
-- --   Single n -> (runIdentity n, Left $ MkSomeComposite xs)

-- -- * Examples

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

j = Single @_ @String @19 @17 $ Const @_  ()

--msgStructure :: Composite 16 0 (Const ())'[Uint16, Uint4, Bool]
msgStructure = Prepend (Single @_ @String @19 @17 $ Const @_ @String ())
  $ Prepend (Single @_ @String @16 @9 $ Const ())
  $ Prepend (Single @_ @String @8 @5  $ Const ()) -- 0x11110000
  $ Single @_ @Bool @4 @1 $ Const ()

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
import GHC.TypeNats (KnownNat, natVal, type (-), type (+), type (<=), Nat)
import Data.Bits (shiftL, (.&.), shiftR, Bits ((.|.)))
import Data.Type.Ord (Compare)
import Data.Functor.Const (Const (Const))
import Control.Arrow ((>>>))
import Data.Foldable (Foldable(fold))
import Control.Monad.Identity (Identity(Identity))
import Communication (Uint4)

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
  :: forall hi lo ns n
  . (BitLike n, Num n, Num n)
  => Composite hi lo (Const n) ns
  -> n
pack (Prepend x xs) = pack x + pack xs
pack (Single (Const n)) = makePart (part @hi @lo) n

unpack
  :: forall hi lo ns f g n
  . (BitLike n, Num n, Num n) 
  => (forall a. f a -> n -> g a)
  -> Composite hi lo f ns
  -> n
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

takePart :: (BitLike a, Num a) => Part hi lo -> a -> a
takePart MkPart {offset, mask} src =
  (src `conj` fromInteger mask) `shiftR'` fromInteger (toEnum offset)  

class Sized size a
instance Sized 16 Uint16
instance Sized 8 Uint8
instance Sized 4 Uint8

class BitLike a where
  shiftL' :: a -> a -> a
  shiftR' :: a -> a -> a
  conj :: a -> a -> a
  disj :: a -> a -> a

instance BitLike Integer where
  shiftL' a = shiftL a . fromInteger
  shiftR' a = shiftR a . fromInteger
  conj = (.&.)
  disj = (.|.)

newtype AsBit a = MkAsBit a
  deriving newtype (IvoryType,IvoryVar,IvoryExpr,Num,IvoryBits)

instance IvoryBits (AsBit a) => BitLike (AsBit a) where
  shiftL' = iShiftL
  shiftR' = iShiftR
  conj = (.&)
  disj = (.|)

deriving via AsBit Uint8 instance BitLike Uint8
deriving via AsBit Uint16 instance BitLike Uint16

makePart :: (BitLike a, Num a) => Part hi lo -> a -> a
makePart MkPart {offset, mask} n =
  (n `shiftL'` fromInteger (toEnum offset))
    `conj` fromInteger mask

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

newtype Iso a b = MkIso (a -> b, b -> a)

-- newtype S f g a = MkS (f a (g a))

type Msg f = Composite 16 1 f
   '[ '(8,Uint8) -- 8 bits size
    , '(4,Uint4) -- 4 bits size
    , '(4,Uint4) -- 4 bits size
    ]

packMsg
  :: Msg (Const Uint16)
  -> Uint16
packMsg = pack

compositeNatTrans ::
  (forall a. f a -> g a) ->
  Composite hi lo f ns ->
  Composite hi lo g ns
compositeNatTrans f (Single x) = Single (f x)
compositeNatTrans f (Prepend x xs) =
  Prepend (compositeNatTrans f x) (compositeNatTrans f xs)

msgSchema :: Msg (Iso Uint16)
msgSchema = undefined

data N = S N | Z

type family NToNat (n :: N) where
  NToNat Z = 0
  NToNat (S n) = 1 + NToNat n

apply :: forall f g h hi lo ns.
  (forall a. f a -> g a -> h a) ->
  Composite (NToNat hi) (NToNat lo) f ns ->
  Composite (NToNat hi) (NToNat lo) g ns ->
  Composite (NToNat hi) (NToNat lo) h ns
apply f (Single x) (Single y) = Single (f x y)
apply f (Prepend @hi1 @hi0 x0 x1) (Prepend @hi1' @hi0' y0 y1) =
  q'
  where
  q' :: Composite hi lo h ns
  q' = q
  q :: hi1 ~ hi1' => hi0 ~ hi0' => Composite hi lo h ns
  q = Prepend @hi1 @hi0
    (apply f x0 y0)
    (apply f x1 undefined)



putMessage ::
  (Uint8,Uint4,Uint4) ->
  Msg (Iso Uint16) ->
  Msg (Const Uint16)
putMessage (a,b,c) = \case
  Prepend (Single fa) x -> undefined

unpackMsg' :: forall
  . Msg (Iso Uint16)
  -> Uint16 -- integer to extract these pieces from
  -> (Uint8,Uint4,Uint4)
unpackMsg' schema n =
  case unpack (\(MkIso (to,_)) n -> Identity $ to n) schema n of
    Prepend (Single (Identity n3))
      (Prepend
        (Single (Identity n2))
        (Single (Identity n1))) -> (n3,n2,n1)


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

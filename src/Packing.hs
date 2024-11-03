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
  -- ( Composite (Single, Prepend)
  -- , begin
  -- , append
  -- -- * Packing/unpacking
  -- , pack
  -- , unpack
  -- -- * Instances
  -- , CompositeF (MkCompositeF, unCompositeF)
  -- , withComposite
  -- -- * Part
  -- , Part (MkPart, sizeBits, offset, mask)
  -- , part
  -- , makePart
  -- , takePart
  -- )
  where

import Prelude hiding (init, fail)
import Ivory.Language as Ivory
import GHC.TypeNats (KnownNat, natVal, type (-), type (+), type (<=))
import Data.Bits (shiftL, (.&.), shiftR, Bits ((.|.)))
import Data.Type.Ord (Compare)
import Data.Functor.Const (Const (Const))
import Control.Arrow ((>>>))
import Data.Foldable (Foldable(fold))
import Control.Monad.Identity (Identity(Identity))
import Communication (Uint4)

type family a :+: b where
  S a :+: b = S (a :+: b)
  Z :+: b = b

type family a :-: b where
  S a :-: S b = (a :-: b)
  a :-: Z = a

type family N2S n where
  N2S 0 = Z
  N2S n = S (N2S (n - 1))  

type family S2N n where
  S2N Z = 0
  S2N (S n) = 1 + (S2N n)

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

begin
  :: forall size n f {psize}
  . ( size ~ S psize
    , Materialize size
    )
  => f n
  -> Composite size (N2S 1) f '[ '(N2S 1,n) ]
begin = Single (materialize @size) materialize

pack
  :: forall hi lo ns n
  . (BitLike n, Num n, Num n)
  => Composite hi lo (Const n) ns
  -> n
pack (Prepend x xs) = pack x + pack xs
pack (Single @psize size lo (Const n)) =
  makePart (part (add lo size) lo) n

unpack
  :: forall hi lo ns f g n
  . (BitLike n, Num n, Num n) 
  => (forall a. f a -> n -> g a)
  -> Composite hi lo f ns
  -> n
  -> Composite hi lo g ns
unpack f = flip \packed -> \case
  Single size lo n ->
    Single size lo $ f n $ takePart (part size lo) packed
  Prepend x xs ->
    Prepend
       (unpack f x packed)
       (unpack f xs packed)

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

-- * Part

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

part :: forall size {plo} {psize}.
  Nat size ->
  Nat (S plo) ->
  Part (S plo :+: psize) (S plo)
part size lo = MkPart {offset, sizeBits, mask}
  where
    mask = shiftL template offset
    template :: Integer
    template = 2^sizeBits - 1
    sizeBits = hi - offset
    hi = natToInt (lo `add` size)
    offset = natToInt lo - 1

newtype Iso a b = MkIso (a -> b, b -> a)

-- newtype S f g a = MkS (f a (g a))

type Msg f = Composite (N2S 16) (N2S 1) f
   '[ '(N2S 9, Uint8) -- 8 bits size
    , '(N2S 5, Uint4) -- 4 bits size
    , '(N2S 1, Uint4) -- 4 bits size
    ]

compositeNatTrans ::
  (forall a. f a -> g a) ->
  Composite hi lo f ns ->
  Composite hi lo g ns
compositeNatTrans f (Single size lo x) = Single size lo (f x)
compositeNatTrans f (Prepend x xs) =
  Prepend (compositeNatTrans f x) (compositeNatTrans f xs)

msgSchema :: Msg (Iso Uint16)
msgSchema = append (MkIso (castDefault,safeCast))
  $ append (MkIso (castDefault,safeCast))
  $ begin (MkIso (castDefault,safeCast))

data N = S N | Z

type family NToNat (n :: N) where
  NToNat Z = 0
  NToNat (S n) = 1 + NToNat n

data Gt a b where
  GtBase :: Gt (S a) Z
  GtStep :: Gt a b -> Gt (S a) (S b) 
  
data Nat (n :: N) where
  Zero :: Nat Z
  Succ :: Nat n -> Nat (S n)

putMessage ::
  (Uint8,Uint4,Uint4) ->
  Msg (Iso Uint16) ->
  Msg (Const Uint16)
putMessage (a,b,c) = apply f
    $ append (Identity c)
    $ append (Identity b)
    $ begin (Identity a)
  where
    f (Identity x) (MkIso (_,from)) = Const $ from x

_ = pack $ putMessage (10,20,30) msgSchema

unpackMsg' :: forall
  . Msg (Iso Uint16)
  -> Uint16 -- integer to extract these pieces from
  -> (Uint8,Uint4,Uint4)
unpackMsg' schema n =
  case unpack (\(MkIso (to,_)) x -> Identity $ to x) schema n of
    Prepend (Single _ _ (Identity n3))
      (Prepend
        (Single _ _ (Identity n2))
        (Single _ _ (Identity n1))) -> (n3,n2,n1)


unpackMsg :: forall a
  . Composite (N2S 16) (N2S 1) (Const a) -- 16 bits size in total
   '[ '(N2S 9,Integer) -- 8 bits size
    , '(N2S 5,Integer) -- 4 bits size
    , '(N2S 1,Integer) -- 4 bits size
    ]
  -> Integer -- integer to extract these pieces from
  -> (Integer,Integer,Integer)
unpackMsg schema n =
  case unpack (const Const) schema n of
    Prepend (Single _ _ (Const n3))
      (Prepend
        (Single _ _ (Const n2))
        (Single _ _ (Const n1))) -> (n3,n2,n1)

-- -- -- * Instances

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

-- partJ  = part @8 @5

-- collectedJJ = fold
--   $ pure @[] <$> MkCompositeF unpackedJJ

-- unpackedJJ = unpack (const Const) jj (pack jj)

-- jj = append @8 @Uint8 (n 251)
--   $ append @4 @Bool (n 13)
--   $ begin @4 @Int (n 7)
--   where
--     n = Const @Integer

-- k =
--   append @8 @Uint8 cu
--   $ append @4 @Bool cu
--   $ begin @4 @String cu
--   where
--     cu = Const ()

-- j = Single @_ @String @19 @17 $ Const @_  ()

-- --msgStructure :: Composite 16 0 (Const ())'[Uint16, Uint4, Bool]
-- msgStructure = Prepend (Single @_ @String @19 @17 $ Const @_ @String ())
--   $ Prepend (Single @_ @String @16 @9 $ Const ())
--   $ Prepend (Single @_ @String @8 @5  $ Const ()) -- 0x11110000
--   $ Single @_ @Bool @4 @1 $ Const ()

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

module Composite.Pack
  ( -- * Packing/unpacking
    pack
  , unpack
  -- * Part
  , Part (MkPart, sizeBits, offset, mask)
  , part
  , makePart
  , takePart
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
import Composite.Type


-- * Packing/unpacking

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

-- * Part

data Part hi lo = MkPart
  { offset :: Int
  , sizeBits :: Int
  , mask :: Integer
  }

takePart :: (BitLike a, Num a) => Part hi lo -> a -> a
takePart MkPart {offset, mask} src =
  (src `conj` fromInteger mask) `shiftR'` fromInteger (toEnum offset)  

-- class Sized size a
-- instance Sized 16 Uint16
-- instance Sized 8 Uint8
-- instance Sized 4 Uint8

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

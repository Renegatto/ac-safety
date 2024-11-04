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
module Message
  ( Iso(MkIso, unIso)
  , Msg
  , msgSchema
  , putMessage
  , unpackMsg
  , unpackHsMsg
  , Uint4(MkUint4, unUint4)
  )
  where

import Prelude hiding (init, fail)
import Ivory.Language as Ivory
import Data.Bits (shiftL, (.&.), shiftR, Bits ((.|.)))
import Data.Functor.Const (Const (Const))
import Control.Arrow ((>>>))
import Control.Monad.Identity (Identity(Identity))
import Nats
import Composite.Type
import Composite.Pack (part, unpack, pack)
import Data.Foldable (Foldable(fold))
import Ivory.Language.Uint (Uint8(Uint8))

newtype Uint4 = MkUint4 { unUint4 :: Uint8 }
  deriving newtype (Default, Bounded)
  deriving newtype (IvoryType, IvoryInit, IvoryStore, IvoryZeroVal, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd, Num)

newtype Iso a b = MkIso { unIso :: (a -> b, b -> a) }

type Msg f = Composite (N2S 16) (N2S 1) f
   '[ '(N2S 9, Uint8) -- 8 bits size
    , '(N2S 5, Uint4) -- 4 bits size
    , '(N2S 1, Uint4) -- 4 bits size
    ]

msgSchema :: Msg (Iso Uint16)
msgSchema = append (MkIso (castDefault,safeCast))
  $ append (MkIso (MkUint4 . castDefault,safeCast . unUint4))
  $ begin (MkIso (MkUint4 . castDefault,safeCast . unUint4))

putMessage ::
  (Uint8,Uint4,Uint4) ->
  Msg (Iso Uint16) ->
  Msg (Const Uint16)
putMessage (x2,x1,x0) = apply f
    $ append (Identity x2)
    $ append (Identity x1)
    $ begin (Identity x0)
  where
    f (Identity x) (MkIso (_,from)) = Const $ from x

_ = pack $ putMessage (10,20,30) msgSchema

unpackMsg :: forall
  . Msg (Iso Uint16)
  -> Uint16 -- integer to extract these pieces from
  -> (Uint8,Uint4,Uint4)
unpackMsg schema n =
  case unpack (\(MkIso (to,_)) x -> Identity $ to x) schema n of
    Prepend (Single _ _ (Identity n3))
      (Prepend
        (Single _ _ (Identity n2))
        (Single _ _ (Identity n1))) -> (n3,n2,n1)


unpackHsMsg :: forall a
  . Composite (N2S 16) (N2S 1) (Const a) -- 16 bits size in total
   '[ '(N2S 9,Integer) -- 8 bits size
    , '(N2S 5,Integer) -- 4 bits size
    , '(N2S 1,Integer) -- 4 bits size
    ]
  -> Integer -- integer to extract these pieces from
  -> (Integer,Integer,Integer)
unpackHsMsg schema n =
  case unpack (const Const) schema n of
    Prepend (Single _ _ (Const n3))
      (Prepend
        (Single _ _ (Const n2))
        (Single _ _ (Const n1))) -> (n3,n2,n1)

-- * Existentials

-- * Examples

partJ = part
  (materialize @(N2S 8))
  (materialize @(N2S 5))

collectedJJ = fold
  $ pure @[] <$> MkCompositeF unpackedJJ

unpackedJJ = unpack (const Const) jj (pack jj)

jj = append @(N2S 8) @Uint8 (n 251)
  $ append @(N2S 4) @Bool (n 13)
  $ begin @(N2S 4) @Int (n 7)
  where
    n = Const @Integer

k =
  append @(N2S 8) @Uint8 cu
  $ append @(N2S 4) @Bool cu
  $ begin @(N2S 4) @String cu
  where
    cu = Const ()

-- j = Single @_ @String @19 @17 $ Const @_  ()

newtype RComposite hi lo f ns = MkRComposite
  { unRComposite :: (Composite hi lo f ns, Proxy '( S2N hi,S2N lo ))  }

msgStructure' ::
  Composite (N2S 47) (N2S 1) (Const ())
  '[ '(N2S 29,Uint16)
  , '(N2S 13,Uint4)
  , '(N2S 5,Bool)
  , '(N2S 1,Bool)
  ]
msgStructure' = msgStructure

msgStructure =
  append @(N2S 19) @Uint16 @_ @(N2S 28) @(N2S 1) (Const ())
  $ append @(N2S 16) @Uint4 @_ @(N2S 12) (Const ())
  $ append @(N2S 8) @Bool @_ @(N2S 4) @_ (Const ())
  $ begin @(N2S 4) @Bool @_ (Const ())
  -- where
  --   _ = s2n @u
_ = s2n @((S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))))))))))))))))
{-
'( 'S ('S ('S ('S ('S ('S ('S ('S ('S ('S ('S ('S ('S 'Z))))))))))))

-}
s2n :: forall s. Materialize s => Proxy (S2N s) 
s2n = Proxy @(S2N s)

MkRComposite (_,ll) = MkRComposite (msgStructure, Proxy) 
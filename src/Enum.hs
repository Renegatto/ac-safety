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
module Enum (matchEnum, coerceInit, unEnum) where

import Ivory.Language as Ivory
import Data.Coerce (Coercible)
import Ivory.Language.Init (Init(Init))
import Ivory.Stdlib (cond, (==>))
import Data.Kind (Type)


coerceInit :: Coercible a b => Init a -> Init b
coerceInit (Init x) = Init x 

-- | Match the haskell enum represented by some C number (as enum?)
-- Inlines all the branches as nested if-then-else statements
matchEnum :: forall eff b conId.
  (IvoryEq conId, Num conId) =>
  (Bounded b, Enum b) =>
  conId -> (b -> Ivory eff ()) -> Ivory eff ()
matchEnum conId cont = do
  foldr
    (\con else_ -> ifte_ (conId ==? toConId con) (cont con) else_)
    (assert false)
  $ enumFrom (minBound @b)
  where
    toConId :: b -> conId
    toConId = fromInteger . toEnum . fromEnum

-- | Match the haskell enum represented by some C number (as enum?)
-- Inlines all the branches as nested if-then-else statements
unEnum :: forall eff b (s :: Type) conId.
  (IvoryOrd conId, Num conId, IvoryStore b, IvoryZeroVal b) =>
  (GetAlloc eff ~ 'Scope s) =>
  (Bounded b, Enum b) =>
  conId ->
  Ivory eff b
unEnum conId =do
  assert (conId <=? toConId (maxBound @b))
  cond
    $ fmap (\con -> (conId ==? toConId con) ==> pure con)
    $ enumFrom (minBound @b)
  where
    toConId :: b -> conId
    toConId = fromInteger . toEnum . fromEnum
{-# OPTIONS_GHC -XGHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Things where
import Prelude hiding (flip)

data Equal a b where
  EqRefl :: Equal a b

data SKI a where
  I :: forall a.
    SKI (a -> a)
  K :: forall a b.
    SKI (a -> b -> a)
  S :: forall a b c.
    SKI ((a -> b -> c) -> (a -> b) -> (a -> c))
  Apply :: forall a b.
    SKI (a -> b) -> SKI a -> SKI b

-- now it's # instead of just space
infixl 4 #
a # b = Apply a b

flip :: SKI ((a -> b -> c) -> b -> a -> c)
flip = S # (S # (K # S) # (S # (K # K) # S)) # (K # K)

flip' :: (a -> b -> c) -> b -> a -> c
flip' = \f x y -> f y x

data Lambda a where
  Var :: forall a.
    Lambda a
  Abstraction :: forall a b.
    (Lambda a -> Lambda b) -> Lambda (a -> b)
  Application :: forall a b.
    Lambda (a -> b) -> Lambda a -> Lambda b

infixl 4 ##
a ## b = Application a b

flip'' =
  Abstraction \f ->
    Abstraction \x ->
      Abstraction \y ->
        Application (Application f y) x

assignment :: forall a c d.
  Lambda _ ->
  Lambda a ->
  Lambda (a -> c -> d) ->
  Lambda c ->
  Lambda d
assignment g a b c = g ## (Abstraction \f -> f ## a ## b) ## c
  where
    bac :: Lambda d
    bac = b ## a ## c

assignment_answer :: forall a c d.
  Lambda a ->
  Lambda (a -> c -> d) ->
  Lambda c ->
  Lambda d
assignment_answer = assignment $
  Abstraction \f ->
    Abstraction \c0 ->
      f ## Abstraction \a0 ->
        Abstraction \b0 ->
          b0 ## a0 ## c0

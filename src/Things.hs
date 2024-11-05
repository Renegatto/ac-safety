{-# OPTIONS_GHC -XGHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}
module Things where
-- import Prelude hiding (flip)
import Prelude.Linear hiding (IO)
import System.IO.Linear (IO, fromSystemIO) 
import Control.Functor.Linear qualified as LMonad
import Control.Monad qualified as M
import Prelude qualified as P (IO)
import Control.Applicative (pure)
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

newMem :: P.IO Int
newMem = undefined

freeMemAt :: Int -> IO ()
freeMemAt = undefined

newtype Ptr a = UnsafePtr (Ur Int)

malloc :: a -> IO (Ptr a)
malloc x = fromSystemIO M.do
  area <- newMem
  pure $ UnsafePtr $ Ur area

-- withNewDynMemPtr :: (DynMemPtr a %1 -> IO r) -> IO r
-- withNewDynMemPtr f = undefined -- do
--   -- ptr <- newDynMemPtr
--   -- f ptr

write :: Ptr a %1-> a -> IO (Ptr a)
write = undefined

deref :: Ptr a %1 -> IO (Ptr a, Ur a)
deref = undefined

free :: Ptr a %1 -> IO ()
free (UnsafePtr (Ur memArea)) = freeMemAt memArea

good :: IO ()
good = LMonad.do
  ptr <- malloc (222 :: Int)
  (ptr1, Ur val) <- deref ptr
  fromSystemIO $ print val
  fromSystemIO $ print val
  ptr2 <- write ptr1 2 
  ptr3 <- write ptr2 3 
  free ptr3

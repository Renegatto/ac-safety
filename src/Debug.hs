{-# OPTIONS_GHC -XGHC2024 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Debug where
import Ivory.Language

data DebugMode = DebugEnabled | DebugDisabled

class Debug (mode :: DebugMode) where
  debug :: forall a eff. IvoryType a => a -> Ivory eff ()
  debugln :: forall a eff. IvoryType a => a -> Ivory eff ()
  debugPrint :: forall eff. String -> Ivory eff ()
  debugPrintLn :: forall eff. String -> Ivory eff ()
  debugNum :: forall eff n. (Num n, IvoryType n) => n -> Int -> Ivory eff () 


instance Debug 'DebugEnabled where
  debug :: forall a eff. IvoryType a => a -> Ivory eff ()
  debug = undefined
  debugln :: forall a eff. IvoryType a => a -> Ivory eff ()
  debugln = undefined
  debugPrint :: forall eff. String -> Ivory eff ()
  debugPrint = undefined
  debugPrintLn :: forall eff. String -> Ivory eff ()
  debugPrintLn = undefined
  debugNum :: forall eff n. (Num n, IvoryType n) => n -> Int -> Ivory eff () 
  debugNum _ base = undefined

debugNumLn :: forall dbg eff n. Debug dbg => (Num n, IvoryType n) => n -> Int -> Ivory eff ()
debugNumLn n base = do
  debugNum @dbg n base
  debugPrintLn @dbg ""

instance Debug 'DebugDisabled where
  debug :: forall a eff. IvoryType a => a -> Ivory eff ()
  debug _ = pure ()
  debugln :: forall a eff. IvoryType a => a -> Ivory eff ()
  debugln _ = pure ()
  debugPrint :: forall eff. String -> Ivory eff ()
  debugPrint _ = pure ()
  debugPrintLn :: forall eff. String -> Ivory eff ()
  debugPrintLn _ = pure ()
  debugNum :: forall eff n. (Num n, IvoryType n) => n -> Int -> Ivory eff () 
  debugNum _ _ = pure ()
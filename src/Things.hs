{-# OPTIONS_GHC -XGHC2021 #-}
module Things where

data Equal a b where
  EqRefl :: Equal a b
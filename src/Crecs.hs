module Crecs () where

data Some c where
  Some :: c a => a -> Some c

data World = World {}
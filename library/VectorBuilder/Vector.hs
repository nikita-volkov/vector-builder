-- |
-- Extensions to the standard immutable Vector API.
module VectorBuilder.Vector
where

import VectorBuilder.Prelude
import Data.Vector
import qualified VectorBuilder.Builder as A
import qualified VectorBuilder.MVector as B


{-# INLINE build #-}
build :: A.Builder element -> Vector element
build builder =
  runST (B.build builder >>= unsafeFreeze)

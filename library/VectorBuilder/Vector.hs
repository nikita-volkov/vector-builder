-- |
-- Extensions to the standard immutable Vector API.
module VectorBuilder.Vector
where

import VectorBuilder.Private.Prelude
import Data.Vector.Generic
import qualified VectorBuilder.Private.Builder as A
import qualified VectorBuilder.MVector as B


-- |
-- Construct a vector from a builder.
{-# INLINE build #-}
build :: Vector vector element => A.Builder element -> vector element
build builder =
  runST (B.build builder >>= unsafeFreeze)

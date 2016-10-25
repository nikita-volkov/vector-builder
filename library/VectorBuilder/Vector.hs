-- |
-- Extensions to the standard immutable Vector API.
module VectorBuilder.Vector
where

import VectorBuilder.Private.Prelude
import Data.Vector
import qualified VectorBuilder.Private.Builder as A
import qualified VectorBuilder.MVector as B


-- |
-- Construct a vector from a builder.
{-# INLINE build #-}
build :: A.Builder element -> Vector element
build builder =
  runST (B.build builder >>= unsafeFreeze)

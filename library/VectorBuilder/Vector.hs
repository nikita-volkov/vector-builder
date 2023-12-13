-- |
-- Extensions to the standard immutable Vector API.
module VectorBuilder.Vector where

import Data.Vector.Generic
import qualified VectorBuilder.Core.Builder as A
import qualified VectorBuilder.MVector as B
import VectorBuilder.Prelude

-- |
-- Construct an immutable vector from a builder.
--
-- Supports all kinds of vectors: boxed, unboxed, primitive, storable.
{-# INLINE build #-}
build :: (Vector vector element) => A.Builder element -> vector element
build builder =
  runST (B.build builder >>= unsafeFreeze)

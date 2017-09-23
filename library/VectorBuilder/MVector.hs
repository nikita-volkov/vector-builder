-- |
-- Extensions to the standard mutable Vector API.
module VectorBuilder.MVector
where

import VectorBuilder.Prelude
import Data.Vector.Generic.Mutable
import qualified VectorBuilder.Core.Builder as A
import qualified VectorBuilder.Core.Update as C


-- |
-- Construct a mutable vector from a builder.
-- 
-- Supports all kinds of vectors: boxed, unboxed, primitive, storable.
{-# INLINABLE build #-}
build :: MVector vector element => A.Builder element -> ST s (vector s element)
build (A.Builder size (C.Update update)) =
  do
    vector <- unsafeNew size
    update vector 0
    return vector

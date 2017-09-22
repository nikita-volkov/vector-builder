-- |
-- Extensions to the standard mutable Vector API.
module VectorBuilder.MVector
where

import VectorBuilder.Private.Prelude
import Data.Vector.Generic.Mutable
import qualified VectorBuilder.Private.Builder as A
import qualified VectorBuilder.Private.UpdateWithOffset as B
import qualified VectorBuilder.Private.Update as C


-- |
-- Construct a mutable vector from a builder.
-- 
-- Supports all kinds of vectors: boxed, unboxed, primitive, storable.
{-# INLINABLE build #-}
build :: MVector vector element => A.Builder element -> ST s (vector s element)
build (A.Builder (B.UpdateWithOffset offsetTrackingUpdateFn)) =
  case offsetTrackingUpdateFn 0 of
    (C.Update actionFn, offset) ->
      do
        vector <- unsafeNew offset
        actionFn vector
        return vector

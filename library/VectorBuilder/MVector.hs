-- |
-- Extensions to the standard mutable Vector API.
module VectorBuilder.MVector
where

import VectorBuilder.Private.Prelude
import Data.Vector.Generic.Mutable
import qualified VectorBuilder.Private.Builder as A
import qualified VectorBuilder.Private.SizeTrackingAction as B
import qualified VectorBuilder.Private.Action as C


-- |
-- Construct a mutable vector from a builder.
{-# INLINABLE build #-}
build :: MVector vector element => A.Builder element -> ST s (vector s element)
build (A.Builder (B.SizeTrackingAction sizeTrackingActionFn)) =
  case sizeTrackingActionFn 0 of
    (C.Action actionFn, size) ->
      do
        vector <- unsafeNew size
        actionFn vector
        return vector

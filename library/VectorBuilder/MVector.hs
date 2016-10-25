-- |
-- Extensions to the standard mutable Vector API.
module VectorBuilder.MVector
where

import VectorBuilder.Private.Prelude
import Data.Vector.Mutable
import qualified VectorBuilder.Private.Builder as A
import qualified VectorBuilder.Private.Action as B


{-# INLINABLE build #-}
build :: A.Builder element -> ST s (MVector s element)
build (A.Builder (B.Action actionFn)) =
  case actionFn 0 of
    (updateFn, size) ->
      do
        vector <- unsafeNew size
        updateFn vector
        return vector

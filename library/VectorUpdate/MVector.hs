-- |
-- Extensions to the standard mutable Vector API.
module VectorUpdate.MVector
where

import VectorUpdate.Prelude
import Data.Vector.Mutable
import qualified VectorUpdate.Builder as A
import qualified VectorUpdate.Action as B


{-# INLINABLE build #-}
build :: A.Builder element -> ST s (MVector s element)
build (A.Builder (B.Action actionFn)) =
  case actionFn 0 of
    (updateFn, size) ->
      do
        vector <- unsafeNew size
        updateFn vector
        return vector

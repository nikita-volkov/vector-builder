-- |
-- Extensions to the standard immutable Vector API.
module VectorUpdate.Vector
where

import VectorUpdate.Prelude
import Data.Vector
import qualified VectorUpdate.Builder as A
import qualified VectorUpdate.MVector as B


{-# INLINE build #-}
build :: A.Builder element -> Vector element
build builder =
  runST (B.build builder >>= unsafeFreeze)

module VectorBuilder.Private.Prelude
(
  module Exports,
  strict,
)
where

import BasePrelude as Exports hiding ((<>))
import Data.Semigroup as Exports (Semigroup(..))

{-# INLINE strict #-}
strict :: a -> a
strict a =
  seq a a

module VectorBuilder.Private.UpdateWithOffset
where

import VectorBuilder.Private.Prelude
import qualified Data.Vector.Generic as B
import qualified VectorBuilder.Private.Update as C


newtype UpdateWithOffset element =
  UpdateWithOffset (Int -> (C.Update element, Int))

instance Semigroup (UpdateWithOffset element) where
  {-# INLINE (<>) #-}
  (<>) (UpdateWithOffset leftOffsetFn) (UpdateWithOffset rightOffsetFn) =
    UpdateWithOffset (\leftOffset -> case leftOffsetFn leftOffset of
      (leftUpdate, rightOffset) -> case rightOffsetFn rightOffset of
        (rightUpdate, !offset) -> (leftUpdate <> rightUpdate, offset))

instance Monoid (UpdateWithOffset element) where
  {-# INLINE mempty #-}
  mempty =
    UpdateWithOffset (\offset -> (mempty, offset))
  mappend =
    (<>)

{-# INLINE snoc #-}
snoc :: element -> UpdateWithOffset element
snoc element =
  UpdateWithOffset (\size -> (C.unsafeWrite size element, strict (succ size)))

{-# INLINE append #-}
append :: B.Vector vector element => vector element -> UpdateWithOffset element
append appendedVector =
  UpdateWithOffset (\size -> (C.unsafeWriteMany size appendedVector, strict (size + B.length appendedVector)))

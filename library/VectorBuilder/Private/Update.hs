module VectorBuilder.Private.Update
where

import VectorBuilder.Private.Prelude
import qualified Data.Vector.Generic.Mutable as A
import qualified Data.Vector.Generic as B


newtype Update element =
  Update (forall s vector. A.MVector vector element => vector s element -> ST s ())

instance Semigroup (Update element) where
  {-# INLINE (<>) #-}
  (<>) (Update leftIO) (Update rightIO) =
    Update (\v -> leftIO v >> rightIO v)

instance Monoid (Update element) where
  {-# INLINE mempty #-}
  mempty =
    Update (const (pure ()))
  {-# INLINE mappend #-}
  mappend =
    (<>)

{-# INLINE unsafeWrite #-}
unsafeWrite :: Int -> element -> Update element
unsafeWrite index element =
  Update (\mVector -> A.unsafeWrite mVector index element)

{-# INLINE unsafeWriteMany #-}
unsafeWriteMany :: B.Vector vector element => Int -> vector element -> Update element
unsafeWriteMany startingIndex appendedVector =
  Update (\mVector -> B.ifoldM' (\_ index element -> A.unsafeWrite mVector (strict (startingIndex + index)) element) () appendedVector)

module VectorBuilder.Core.Update
where

import VectorBuilder.Prelude
import qualified Data.Vector.Generic.Mutable as A
import qualified Data.Vector.Generic as B


newtype Update element =
  Update (forall s vector. A.MVector vector element => vector s element -> Int -> ST s ())

{-# INLINE write #-}
write :: element -> Update element
write element =
  Update (\mVector offset -> A.unsafeWrite mVector offset element)

{-# INLINE writeMany #-}
writeMany :: B.Vector vector element => vector element -> Update element
writeMany appendedVector =
  Update (\mVector offset -> B.ifoldM' (\_ index element -> A.unsafeWrite mVector (strict (offset + index)) element) () appendedVector)

{-# INLINE prepend #-}
prepend :: Int -> Update element -> Update element -> Update element
prepend size (Update leftST) (Update rightST) =
  Update (\mVector offset -> leftST mVector offset >> rightST mVector (strict (size + offset)))

{-# INLINE empty #-}
empty :: Update element
empty =
  Update (\_ _ -> pure ())

{-# INLINE writeFoldable #-}
writeFoldable :: Foldable foldable => foldable element -> Update element
writeFoldable foldable =
  Update (\mVector offset -> foldM_ (\ index element -> A.unsafeWrite mVector index element $> succ index) offset foldable)

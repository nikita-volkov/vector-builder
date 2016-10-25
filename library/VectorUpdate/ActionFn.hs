module VectorUpdate.ActionFn
where

import VectorUpdate.Prelude
import qualified Data.Vector as A
import qualified Data.Vector.Mutable as B


-- |
-- An operation on vector in a context of its presumed size.
type ActionFn s element result =
  B.MVector s element -> STRef s Int -> ST s result

snoc :: element -> ActionFn s element ()
snoc element mVector sizeRef =
  do
    !size <- readSTRef sizeRef
    !newSize <- return (succ size)
    writeSTRef sizeRef newSize
    B.unsafeWrite mVector size element

mapElement :: (element1 -> element2) -> ActionFn s element1 result -> ActionFn s element2 result
mapElement fn actionFn1 mVector sizeRef =
  error "unimplementable"

mapResult :: (result1 -> result2) -> ActionFn s element result1 -> ActionFn s element result2
mapResult fn actionFn1 mVector sizeRef =
  fmap fn (actionFn1 mVector sizeRef)

ap :: ActionFn s element (result1 -> result2) -> ActionFn s element result1 -> ActionFn s element result2
ap actionFn1 actionFn2 mVector sizeRef =
  actionFn1 mVector sizeRef <*> actionFn2 mVector sizeRef

point :: result -> ActionFn s element result
point result _ _ =
  pure result

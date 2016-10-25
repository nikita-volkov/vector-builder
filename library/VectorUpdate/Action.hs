module VectorUpdate.Action
where

import VectorUpdate.Prelude
import qualified Data.Vector.Mutable as A
import qualified Data.Vector as B


newtype Action s element result =
  Action (Int -> (A.MVector s element -> ST s result, Int))

instance Functor (Action s element) where
  fmap fn (Action actionFn) =
    Action $ \size ->
    case actionFn size of
      (updateFn, newSize) ->
        (fmap fn . updateFn, newSize)

instance Applicative (Action s element) where
  pure result =
    Action (\size -> (const (pure result), size))
  (<*>) (Action actionFn1) (Action actionFn2) =
    Action (\size -> combineActionResults size (actionFn1 size) (actionFn2 size))
    where
      combineActionResults size (vectorFn1, size1) (vectorFn2, size2) =
        (vectorFn3, size3)
        where
          vectorFn3 =
            (<*>) <$> vectorFn1 <*> vectorFn2
          size3 =
            size1 + size2 - size


snoc :: element -> Action s element ()
snoc element =
  Action (\size -> (\mVector -> A.unsafeWrite mVector size element, succ size))

append :: B.Vector element -> Action s element ()
append appendedVector =
  Action ((,) <$> vectorFn <*> size)
  where
    vectorFn currentSize mVector =
      B.ifoldM' (\_ index element -> A.unsafeWrite mVector (currentSize + index) element) () appendedVector
    size currentSize =
      B.length appendedVector + currentSize

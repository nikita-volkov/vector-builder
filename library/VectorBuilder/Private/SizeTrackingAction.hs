module VectorBuilder.Private.SizeTrackingAction
where

import VectorBuilder.Private.Prelude
import qualified Data.Vector.Generic as B
import qualified VectorBuilder.Private.Action as C


newtype SizeTrackingAction element result =
  SizeTrackingAction (Int -> (C.Action element result, Int))
  deriving (Functor)

instance Applicative (SizeTrackingAction element) where
  {-# INLINE pure #-}
  pure result =
    SizeTrackingAction (\size -> (pure result, size))
  {-# INLINE (<*>) #-}
  (<*>) (SizeTrackingAction sizeTrackingActionFn1) (SizeTrackingAction sizeTrackingActionFn2) =
    SizeTrackingAction sizeTrackingActionFn
    where
      sizeTrackingActionFn size =
        case sizeTrackingActionFn1 size of
          (action1, !size1) ->
            case sizeTrackingActionFn2 size1 of
              (action2, !size2) ->
                (action1 <*> action2, size2)

{-# INLINE snoc #-}
snoc :: element -> SizeTrackingAction element ()
snoc element =
  SizeTrackingAction (\size -> (C.unsafeWrite size element, succ size))

{-# INLINE append #-}
append :: B.Vector vector element => vector element -> SizeTrackingAction element ()
append appendedVector =
  SizeTrackingAction (\size -> (C.unsafeWriteMany size appendedVector, size + B.length appendedVector))

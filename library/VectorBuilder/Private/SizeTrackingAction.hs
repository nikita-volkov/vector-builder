module VectorBuilder.Private.SizeTrackingAction
where

import VectorBuilder.Private.Prelude
import qualified Data.Vector.Mutable as A
import qualified Data.Vector as B
import qualified VectorBuilder.Private.Action as C


newtype SizeTrackingAction element result =
  SizeTrackingAction (Int -> (C.Action element result, Int))
  deriving (Functor)

instance Applicative (SizeTrackingAction element) where
  pure result =
    SizeTrackingAction (\size -> (pure result, size))
  (<*>) (SizeTrackingAction sizeTrackingActionFn1) (SizeTrackingAction sizeTrackingActionFn2) =
    SizeTrackingAction sizeTrackingActionFn
    where
      sizeTrackingActionFn size =
        case sizeTrackingActionFn1 size of
          (action1, size1) ->
            case sizeTrackingActionFn2 size1 of
              (action2, size2) ->
                (action1 <*> action2, size2)

snoc :: element -> SizeTrackingAction element ()
snoc element =
  SizeTrackingAction (\size -> (C.unsafeWrite size element, succ size))

append :: B.Vector element -> SizeTrackingAction element ()
append appendedVector =
  SizeTrackingAction ((,) <$> action <*> size)
  where
    action currentSize =
      C.unsafeWriteMany currentSize appendedVector
    size currentSize =
      B.length appendedVector + currentSize
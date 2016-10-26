module VectorBuilder.Private.Action
where

import VectorBuilder.Private.Prelude
import qualified Data.Vector.Mutable as A
import qualified Data.Vector as B


newtype Action element result =
  Action (forall s. A.MVector s element -> ST s result)
  deriving (Functor)

instance Applicative (Action element) where
  pure result =
    Action (const (pure result))
  (<*>) (Action actionFn1) (Action actionFn2) =
    Action ((<*>) <$> actionFn1 <*> actionFn2)

unsafeWrite :: Int -> element -> Action element ()
unsafeWrite index element =
  Action (\mVector -> A.unsafeWrite mVector index element)

unsafeWriteMany :: Int -> B.Vector element -> Action element ()
unsafeWriteMany startingIndex appendedVector =
  Action (\mVector -> B.ifoldM' (\_ index element -> A.unsafeWrite mVector (startingIndex + index) element) () appendedVector)

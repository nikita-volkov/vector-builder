module VectorBuilder.Private.Action
where

import VectorBuilder.Private.Prelude
import qualified Data.Vector.Generic.Mutable as A
import qualified Data.Vector.Generic as B


newtype Action element result =
  Action (forall s vector. A.MVector vector element => vector s element -> ST s result)
  deriving (Functor)

instance Applicative (Action element) where
  pure result =
    Action (const (pure result))
  (<*>) (Action actionFn1) (Action actionFn2) =
    Action ((<*>) <$> actionFn1 <*> actionFn2)

unsafeWrite :: Int -> element -> Action element ()
unsafeWrite index element =
  Action (\mVector -> A.unsafeWrite mVector index element)

unsafeWriteMany :: B.Vector vector element => Int -> vector element -> Action element ()
unsafeWriteMany startingIndex appendedVector =
  Action (\mVector -> B.ifoldM' (\_ index element -> A.unsafeWrite mVector (startingIndex + index) element) () appendedVector)

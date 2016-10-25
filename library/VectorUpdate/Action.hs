module VectorUpdate.Action
where

import VectorUpdate.Prelude
import qualified VectorUpdate.ActionFn as A


newtype Action s element result =
  Action (A.ActionFn s element result)
  deriving (Functor)

instance Applicative (Action s element) where
  pure =
    Action . A.point
  (<*>) (Action actionFn1) (Action actionFn2) =
    Action (A.ap actionFn1 actionFn2)

instance Monad (Action s element) where

snoc :: element -> Action s element ()
snoc element =
  Action (A.snoc element)


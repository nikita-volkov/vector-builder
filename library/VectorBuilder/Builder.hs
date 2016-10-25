module VectorBuilder.Builder
where

import VectorBuilder.Prelude
import qualified VectorBuilder.Action as A
import qualified Data.Vector as B


newtype Builder element =
  Builder (forall s. A.Action s element ())

instance Monoid (Builder element) where
  mempty =
    VectorBuilder.Builder.empty
  mappend =
    prepend


-- * Initialisation

empty :: Builder element
empty =
  Builder (pure ())

singleton :: element -> Builder element
singleton element =
  Builder (A.snoc element)

vector :: B.Vector element -> Builder element
vector vector =
  Builder (A.append vector)


-- * Updates

snoc :: element -> Builder element -> Builder element
snoc element (Builder action) =
  Builder (action *> A.snoc element)

cons :: element -> Builder element -> Builder element
cons element (Builder action) =
  Builder (A.snoc element *> action)

prepend :: Builder element -> Builder element -> Builder element
prepend (Builder action1) (Builder action2) =
  Builder (action1 *> action2)

append :: Builder element -> Builder element -> Builder element
append (Builder action1) (Builder action2) =
  Builder (action1 <* action2)

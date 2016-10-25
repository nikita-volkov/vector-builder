module VectorUpdate.Builder
where

import VectorUpdate.Prelude
import qualified VectorUpdate.Action as A


newtype Builder element =
  Builder (forall s. A.Action s element ())


-- * Initialisation

empty :: Builder element
empty =
  Builder (pure ())

singleton :: element -> Builder element
singleton element =
  Builder (A.snoc element)


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

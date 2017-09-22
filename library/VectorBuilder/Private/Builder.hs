module VectorBuilder.Private.Builder
where

import VectorBuilder.Private.Prelude hiding (empty)
import qualified VectorBuilder.Private.Update as A
import qualified Data.Vector.Generic as B


-- |
-- An abstraction over the size of a vector for the process of its construction.
-- 
-- It postpones the actual construction of a vector until the execution of the builder.
data Builder element =
  Builder !Int !(A.Update element)


-- * Initialisation

-- |
-- Empty builder.
{-# INLINE empty #-}
empty :: Builder element
empty =
  Builder 0 A.empty

-- |
-- Builder of a single element.
{-# INLINE singleton #-}
singleton :: element -> Builder element
singleton element =
  Builder 1 (A.write element)

-- |
-- Builder from an immutable vector of elements.
-- 
-- Supports all kinds of vectors: boxed, unboxed, primitive, storable.
{-# INLINE vector #-}
vector :: B.Vector vector element => vector element -> Builder element
vector vector =
  Builder (B.length vector) (A.writeMany vector)


-- * Updates

{-# INLINE snoc #-}
snoc :: element -> Builder element -> Builder element
snoc element (Builder size update) =
  Builder (succ size) (A.prepend size update (A.write element))

{-# INLINE cons #-}
cons :: element -> Builder element -> Builder element
cons element (Builder size update) =
  Builder (succ size) (A.prepend 1 (A.write element) update)

{-# INLINE prepend #-}
prepend :: Builder element -> Builder element -> Builder element
prepend (Builder leftSize leftUpdate) (Builder rightSize rightUpdate) =
  Builder (leftSize + rightSize) (A.prepend leftSize leftUpdate rightUpdate)

{-# INLINE append #-}
append :: Builder element -> Builder element -> Builder element
append =
  flip prepend


-- * Instances

-- |
-- Provides support for /O(1)/ concatenation.
instance Semigroup (Builder element) where
  {-# INLINE (<>) #-}
  (<>) =
    prepend

-- |
-- Provides support for /O(1)/ concatenation.
instance Monoid (Builder element) where
  {-# INLINE mempty #-}
  mempty =
    empty
  {-# INLINE mappend #-}
  mappend =
    (<>)



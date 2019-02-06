{-|
MonadPlus utilities.
For instance, they can be applied with parsing libraries.
-}
module VectorBuilder.MonadPlus
where

import VectorBuilder.Prelude
import Data.Vector (Vector)
import qualified VectorBuilder.Builder as A
import qualified VectorBuilder.Vector as B
import qualified Data.Vector.Generic as C


{-# INLINABLE many #-}
many :: (MonadPlus m, C.Vector vector element) => m element -> m (vector element)
many m =
  liftM B.build (manyBuilder m)

{-# INLINABLE manyBuilder #-}
manyBuilder :: (MonadPlus m) => m element -> m (A.Builder element)
manyBuilder m =
  loop mempty
  where
    loop !builder =
      mplus
        (do
          !element <- m
          loop (builder <> A.singleton element))
        (return builder)

{-# INLINABLE many1 #-}
many1 :: (MonadPlus m, C.Vector vector element) => m element -> m (vector element)
many1 m =
  do
    firstElement <- m
    builder <- manyBuilder m
    return (B.build (A.singleton firstElement <> builder))

{-# INLINABLE sepBy #-}
sepBy :: (MonadPlus m, C.Vector vector element) => m element -> m separator -> m (vector element)
sepBy elementM separatorM =
  mplus (sepBy1 elementM separatorM) (return C.empty)

{-# INLINABLE sepBy1 #-}
sepBy1 :: (MonadPlus m, C.Vector vector element) => m element -> m separator -> m (vector element)
sepBy1 elementM separatorM =
  do
    firstElement <- elementM
    builder <- loop (A.singleton firstElement)
    return (B.build builder)
  where
    loop builder =
      mplus
        (do
          separatorM
          !element <- elementM
          loop (builder <> A.singleton element))
        (return builder)

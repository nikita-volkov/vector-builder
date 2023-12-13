-- |
-- MonadPlus utilities.
-- For instance, they can be applied with parsing libraries.
module VectorBuilder.MonadPlus where

import qualified Data.Vector.Generic as C
import qualified VectorBuilder.Builder as A
import VectorBuilder.Prelude
import qualified VectorBuilder.Vector as B

{-# INLINEABLE many #-}
many :: (MonadPlus m, C.Vector vector element) => m element -> m (vector element)
many m =
  liftM B.build (manyBuilder m)

{-# INLINEABLE manyBuilder #-}
manyBuilder :: (MonadPlus m) => m element -> m (A.Builder element)
manyBuilder m =
  loop mempty
  where
    loop !builder =
      mplus
        ( do
            !element <- m
            loop (builder <> A.singleton element)
        )
        (return builder)

{-# INLINEABLE many1 #-}
many1 :: (MonadPlus m, C.Vector vector element) => m element -> m (vector element)
many1 m =
  do
    firstElement <- m
    builder <- manyBuilder m
    return (B.build (A.singleton firstElement <> builder))

{-# INLINEABLE sepBy #-}
sepBy :: (MonadPlus m, C.Vector vector element) => m element -> m separator -> m (vector element)
sepBy elementM separatorM =
  mplus (sepBy1 elementM separatorM) (return C.empty)

{-# INLINEABLE sepBy1 #-}
sepBy1 :: (MonadPlus m, C.Vector vector element) => m element -> m separator -> m (vector element)
sepBy1 elementM separatorM =
  do
    firstElement <- elementM
    builder <- loop (A.singleton firstElement)
    return (B.build builder)
  where
    loop builder =
      mplus
        ( do
            separatorM
            !element <- elementM
            loop (builder <> A.singleton element)
        )
        (return builder)

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
  liftM B.build loop
  where
    loop =
      mplus
        (do
          !element <- m
          remainders <- loop
          return (A.singleton element <> remainders))
        (return mempty)

{-# INLINABLE sepBy #-}
sepBy :: (MonadPlus m, C.Vector vector element) => m element -> m separator -> m (vector element)
sepBy elementM separatorM =
  mplus (sepBy1 elementM separatorM) (return C.empty)

{-# INLINABLE sepBy1 #-}
sepBy1 :: (MonadPlus m, C.Vector vector element) => m element -> m separator -> m (vector element)
sepBy1 elementM separatorM =
  liftM B.build loop
  where
    loop =
      do
        !element <- elementM
        remainders <- mplus (separatorM >> loop) (return mempty)
        return (A.singleton element <> remainders)

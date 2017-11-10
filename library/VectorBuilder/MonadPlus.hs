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


{-# INLINABLE many #-}
many :: MonadPlus m => m a -> m (Vector a)
many m =
  fmap B.build loop
  where
    loop =
      mplus
        (do
          !element <- m
          remainders <- loop
          return (A.singleton element <> remainders))
        (return mempty)

{-# INLINABLE sepBy #-}
sepBy :: MonadPlus m => m element -> m separator -> m (Vector element)
sepBy elementM separatorM =
  mplus (sepBy1 elementM separatorM) (return mempty)

{-# INLINABLE sepBy1 #-}
sepBy1 :: MonadPlus m => m element -> m separator -> m (Vector element)
sepBy1 elementM separatorM =
  fmap B.build loop
  where
    loop =
      do
        !element <- elementM
        remainders <- mplus (separatorM >> loop) (return mempty)
        return (A.singleton element <> remainders)

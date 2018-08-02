{-|
Alternative utilities.
For instance, they can be applied with parsing libraries.
-}
module VectorBuilder.Alternative
where

import VectorBuilder.Prelude
import Data.Vector (Vector)
import qualified VectorBuilder.Builder as A
import qualified VectorBuilder.Vector as B


{-# INLINABLE many #-}
many :: Alternative m => m a -> m (Vector a)
many m =
  fmap B.build loop
  where
    loop = ((<>) <$> A.singleton <$> m <*> loop) <|> pure mempty

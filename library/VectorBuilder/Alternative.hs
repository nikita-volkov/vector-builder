-- |
-- Alternative utilities.
-- For instance, they can be applied with parsing libraries.
module VectorBuilder.Alternative where

import Data.Vector (Vector)
import qualified Data.Vector.Generic as C
import qualified VectorBuilder.Builder as A
import VectorBuilder.Prelude hiding (many, some)
import qualified VectorBuilder.Vector as B

{-# INLINEABLE many #-}
many :: (Alternative m, C.Vector vector a) => m a -> m (vector a)
many = fmap B.build . manyBuilder

{-# INLINEABLE manyBuilder #-}
manyBuilder :: Alternative m => m a -> m (A.Builder a)
manyBuilder m =
  let loop = ((<>) <$> A.singleton <$> m <*> loop) <|> pure mempty
   in loop

{-# INLINEABLE some #-}
some :: (Alternative m, C.Vector vector a) => m a -> m (vector a)
some m = B.build <$> someBuilder m

{-# INLINEABLE someBuilder #-}
someBuilder :: Alternative m => m a -> m (A.Builder a)
someBuilder m = (<>) <$> A.singleton <$> m <*> manyBuilder m

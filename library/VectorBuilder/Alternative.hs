{-|
Alternative utilities.
For instance, they can be applied with parsing libraries.
-}
module VectorBuilder.Alternative
where

import VectorBuilder.Prelude hiding (many, some)
import Data.Vector (Vector)
import qualified VectorBuilder.Builder as A
import qualified VectorBuilder.Vector as B
import qualified Data.Vector.Generic as C


{-# INLINABLE many #-}
many :: (Alternative m, C.Vector vector a) => m a -> m (vector a)
many = fmap B.build . manyBuilder

{-# INLINABLE manyBuilder #-}
manyBuilder :: Alternative m => m a -> m (A.Builder a)
manyBuilder m = let
  loop = ((<>) <$> A.singleton <$> m <*> loop) <|> pure mempty
  in loop

{-# INLINABLE some #-}
some :: (Alternative m, C.Vector vector a) => m a -> m (vector a)
some m = B.build <$> someBuilder m

{-# INLINABLE someBuilder #-}
someBuilder :: Alternative m => m a -> m (A.Builder a)
someBuilder m = (<>) <$> A.singleton <$> m <*> manyBuilder m

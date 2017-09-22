module Main where

import Prelude
import Criterion
import Criterion.Main
import qualified Control.Foldl as A
import qualified Data.Vector as F
import qualified Data.Vector.Unboxed as G
import qualified VectorBuilder.Builder as N
import qualified VectorBuilder.Vector as O


main =
  defaultMain [boxed, unboxed]
  where
    sizes =
      [1000, 100000, 10000000]
    boxed =
      bgroup "boxed" (map group sizes)
      where
        group size =
          bgroup (show size)
          [
            bench "vector-builder" (nf foldWithBuilder input),
            bench "default" (nf foldDefault input)
          ]
          where
            input =
              [0..size]
            foldWithBuilder input =
              A.fold (A.foldMap N.singleton O.build) input :: Vector Int
            foldDefault input =
              runST (A.foldM A.vector input) :: Vector Int
    unboxed =
      bgroup "unboxed" (map group sizes)
      where
        group size =
          bgroup (show size)
          [
            bench "vector-builder" (nf foldWithBuilder input),
            bench "default" (nf foldDefault input)
          ]
          where
            input =
              [0..size]
            foldWithBuilder input =
              A.fold (A.foldMap N.singleton O.build) input :: G.Vector Int
            foldDefault input =
              runST (A.foldM A.vector input) :: G.Vector Int

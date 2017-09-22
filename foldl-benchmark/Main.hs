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
    comparisons name builderSubject growingSubject =
      bgroup name (map comparison sizes)
      where
        sizes =
          [1000, 10000, 100000, 1000000, 10000000]
        comparison size =
          bgroup (show size)
          [
            bench "builder" (nf builderSubject input),
            bench "growing" (nf growingSubject input)
          ]
          where
            input =
              [0..size]
    boxed =
      comparisons "boxed" builderSubject growingSubject
      where
        builderSubject input =
          A.fold (A.foldMap N.singleton O.build) input :: Vector Int
        growingSubject input =
          runST (A.foldM A.vector input) :: Vector Int
    unboxed =
      comparisons "unboxed" builderSubject growingSubject
      where
        builderSubject input =
          A.fold (A.foldMap N.singleton O.build) input :: G.Vector Int
        growingSubject input =
          runST (A.foldM A.vector input) :: G.Vector Int

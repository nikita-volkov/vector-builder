module Main where

import Prelude
import Criterion
import Criterion.Main
import qualified Control.Foldl as A
import qualified Data.Vector as F
import qualified VectorBuilder.Builder as N
import qualified VectorBuilder.Vector as O


main =
  defaultMain [group 1000, group 10000, group 100000, group 1000000, group 10000000, group 100000000]
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

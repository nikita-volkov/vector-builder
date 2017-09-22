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
    comparison builder growing size =
      bgroup (show size)
      [
        bench "builder" (nf builder input),
        bench "growing" (nf growing input)
      ]
      where
        input =
          [0..size]
    sizes =
      [1000, 100000, 10000000]
    boxed =
      bgroup "boxed" (map (comparison builder growing) sizes)
      where
        builder input =
          A.fold (A.foldMap N.singleton O.build) input :: Vector Int
        growing input =
          runST (A.foldM A.vector input) :: Vector Int
    unboxed =
      bgroup "unboxed" (map (comparison builder growing) sizes)
      where
        builder input =
          A.fold (A.foldMap N.singleton O.build) input :: G.Vector Int
        growing input =
          runST (A.foldM A.vector input) :: G.Vector Int

module Main where

import Prelude
import Bug
import Criterion
import Criterion.Main
import qualified Control.Foldl as A
import qualified Data.Vector as F
import qualified VectorBuilder.Builder as N
import qualified VectorBuilder.Vector as O


main =
  defaultMain
  [
    bench "vector-builder" (nf foldWithBuilder input),
    bench "default" (nf foldDefault input)
  ]
  where
    input =
      [0..1000]
    foldWithBuilder input =
      A.fold (A.foldMap N.singleton O.build) input :: Vector Int
    foldDefault input =
      runST (A.foldM A.vector input) :: Vector Int

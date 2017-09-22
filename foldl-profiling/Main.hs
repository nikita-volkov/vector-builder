module Main where

import Prelude
import qualified Data.Vector.Unboxed as V
import qualified Control.Foldl as L
import qualified VectorBuilder.Builder as N
import qualified VectorBuilder.Vector as O

main :: IO ()
main = (print . V.maximum) (L.fold fold input) where
  input = [1..1000000] :: [Int]
  fold = L.Fold step begin done where
    begin = N.empty
    step x a = x <> N.singleton a
    done = O.build

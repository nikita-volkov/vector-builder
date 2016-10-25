module Main where

import Rebase.Prelude
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified VectorBuilder.Builder as A
import qualified VectorBuilder.Vector as B
import qualified Main.Sample as C


main =
  defaultMain $
  testGroup "All tests"
  [
    testProperty "" $ \(samples :: [C.Sample Int]) ->
      foldMap C.toVector samples ===
      B.build (foldMap C.toBuilder samples)
  ]


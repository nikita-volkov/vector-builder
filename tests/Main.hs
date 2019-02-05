module Main where

import Prelude
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified VectorBuilder.Builder as A
import qualified VectorBuilder.Vector as B
import qualified VectorBuilder.Alternative as F
import qualified Main.Sample as C
import qualified Data.Attoparsec.Text as D
import qualified Data.Vector as E


main =
  defaultMain $
  testGroup "All tests"
  [
    testProperty "samples" $ \(samples :: [C.Sample Int]) ->
      foldMap C.toVector samples ===
      B.build (foldMap C.toBuilder samples)
    ,
    testCase "Alternative.some" $ assertEqual ""
      (Right (E.fromList "1234"))
      (D.parseOnly (F.some D.anyChar) "1234")
    ,
    testCase "Alternative.some on empty" $ assertEqual ""
      (Left "not enough input")
      (D.parseOnly (F.some D.anyChar :: D.Parser (Vector Char)) "")
    ,
    testProperty "mconcat" $ \(samples :: [C.Sample Int]) ->
      foldMap C.toVector samples ===
      B.build (mconcat (map C.toBuilder samples))
    ,
    testProperty "foldable" $ \(elements :: [Int]) ->
      E.fromList elements ===
      B.build (A.foldable elements)
  ]


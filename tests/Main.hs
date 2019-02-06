module Main where

import Prelude
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified VectorBuilder.Builder as A
import qualified VectorBuilder.Vector as B
import qualified VectorBuilder.Alternative as F
import qualified VectorBuilder.MonadPlus as H
import qualified Main.Sample as C
import qualified Data.Attoparsec.Text as D
import qualified Data.Vector as E
import qualified Data.Text as G


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
    ,
    testGroup "MonadPlus"
    [
      testProperty "many" $ \(elements :: [Char]) ->
        Right (E.fromList elements) ===
        D.parseOnly (H.many D.anyChar) (fromString elements)
      ,
      testProperty "many1" $ \(elements :: [Char]) ->
        (if null elements
          then Left "not enough input"
          else Right (E.fromList elements)) ===
        D.parseOnly (H.many1 D.anyChar) (fromString elements)
      ,
      testProperty "sepBy1" $ \(elements :: [Char]) ->
        (if null elements
          then Left "not enough input"
          else Right (E.fromList elements)) ===
        D.parseOnly (H.sepBy1 D.anyChar (D.char ',')) (G.intersperse ',' (fromString elements))
    ]
  ]


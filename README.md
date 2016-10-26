# vector-builder

An API for efficient and convenient construction of vectors. It provides the composable `Builder` abstraction, which has instances of the `Monoid` and `Semigroup` classes. 


## Usage

First you use the `Builder` abstraction to specify the structure of the vector. Then you execute the builder to produce the output vector.

## Example

The following code shows how you can efficiently concatenate different datastructures into a single immutable vector:

```haskell

import qualified Data.Vector as A
import qualified VectorBuilder.Builder as B
import qualified VectorBuilder.Vector as C


myVector :: A.Vector a -> [a] -> a -> A.Vector a
myVector vector list element =
  C.build builder
  where
    builder =
      B.vector vector <>
      foldMap B.singleton list <>
      B.singleton element

```

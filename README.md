# vector-builder

An API for efficient construction of vectors with abstraction over their size.

## Usage

Vectors are built using the `Monoid` and `Semigroup` instances for `Builder`.

```
import qualified VectorBuilder.Builder as Builder (singleton)
import qualified VectorBuilder.Vector as Builder (build)
import Data.Vector (Vector)

list2vector :: [a] -> Vector a
list2vector l = Builder.build builder
  where
    builder = foldMap Builder.singleton l

```

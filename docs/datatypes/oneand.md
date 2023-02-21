# OneAnd

API Documentation: @:api(cats.data.OneAnd)

The `OneAnd[F[_],A]` data type represents a single element of type `A`
that is guaranteed to be present (`head`) and in addition to this a
second part that is wrapped inside an higher kinded type constructor
`F[_]`.  By choosing the `F` parameter, you can model for example
non-empty lists by choosing `List` for `F`, giving:

```scala mdoc:silent
import cats.data.OneAnd

type NonEmptyList[A] = OneAnd[List, A]
```

which used to be the implementation of non-empty lists in Cats but has
been replaced by the `cats.data.NonEmptyList` data type. By
having the higher kinded type parameter `F[_]`, `OneAnd` is also able
to represent other "non-empty" data structures e.g.

```scala mdoc:silent
type NonEmptyStream[A] = OneAnd[Stream, A]
```
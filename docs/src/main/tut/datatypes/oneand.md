---
layout: docs
title:  "OneAnd"
section: "data"
source: "core/src/main/scala/cats/data/OneAnd.scala"
scaladoc: "#cats.data.OneAnd"
---
# OneAnd

The `OneAnd[F[_],A]` data type represents a single element of type `A`
that is guaranteed to be present (`head`) and in addition to this a
second part that is wrapped inside an higher kinded type constructor
`F[_]`.  By choosing the `F` parameter, you can model for example
non-empty lists by choosing `List` for `F`, giving:

```tut:silent
import cats.data.OneAnd

type NonEmptyList[A] = OneAnd[List, A]
```

which used to be the implementation of non-empty lists in cats but has
been replaced by the `cats.data.NonEmptyList` data type. By
having the higher kinded type parameter `F[_]`, `OneAnd` is also able
to represent other "non-empty" data structures e.g.

```tut:silent
type NonEmptyStream[A] = OneAnd[Stream, A]
```
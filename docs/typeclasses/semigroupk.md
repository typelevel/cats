# SemigroupK

API Documentation: @:api(cats.SemigroupK)

`SemigroupK` has a very similar structure to [`Semigroup`](semigroup.md), the difference
is that `SemigroupK` operates on type constructors of one argument. So, for
example, whereas you can find a `Semigroup` for types which are fully
specified like `Int` or `List[Int]` or `Option[Int]`, you will find
`SemigroupK` for type constructors like `List` and `Option`. These types
are type constructors in that you can think of them as "functions" in
the type space. You can think of the `List` type as a function which
takes a concrete type, like `Int`, and returns a concrete type:
`List[Int]`. This pattern would also be referred to having `kind: * ->
*`, whereas `Int` would have kind `*` and `Map` would have kind `*,* -> *`,
and, in fact, the `K` in `SemigroupK` stands for `Kind`.

First some imports.

```scala mdoc:silent
import cats._
import cats.syntax.all._
```

For `List`, the `Semigroup` instance's `combine` operation and the `SemigroupK`
instance's `combineK` operation are both list concatenation:

```scala mdoc
SemigroupK[List].combineK(List(1,2,3), List(4,5,6)) == Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6))
```

However for `Option`, the `Semigroup`'s `combine` and the `SemigroupK`'s
`combineK` operation differ. Since `Semigroup` operates on fully specified
types, a `Semigroup[Option[A]]` knows the concrete type of `A` and will use
`Semigroup[A].combine` to combine the inner `A`s. Consequently,
`Semigroup[Option[A]].combine` requires an implicit `Semigroup[A]`.

In contrast, `SemigroupK[Option]` operates on `Option` where
the inner type is not fully specified and can be anything (i.e. is
"universally quantified"). Thus, we cannot know how to `combine`
two of them. Therefore, in the case of `Option` the
`SemigroupK[Option].combineK` method has no choice but to use the
`orElse` method of Option:

```scala mdoc
Semigroup[Option[Int]].combine(Some(1), Some(2))
SemigroupK[Option].combineK(Some(1), Some(2))
SemigroupK[Option].combineK(Some(1), None)
SemigroupK[Option].combineK(None, Some(2))
```

There is inline syntax available for both `Semigroup` and
`SemigroupK`. Here we are following the convention from scalaz, that
`|+|` is the operator from semigroup and that `<+>` is the operator
from `SemigroupK` (called `Plus` in scalaz).

```scala mdoc:silent
import cats.syntax.all._

val one = Option(1)
val two = Option(2)
val n: Option[Int] = None
```

Thus.

```scala mdoc
one |+| two
one <+> two
n |+| two
n <+> two
n |+| n
n <+> n
two |+| n
two <+> n
```

You'll notice that instead of declaring `one` as `Some(1)`, we chose
`Option(1)`, and we added an explicit type declaration for `n`. This is
because the `SemigroupK` type class instances is defined for `Option`,
not `Some` or `None`. If we try to use `Some` or `None`, we'll get errors:

```scala mdoc:fail
Some(1) <+> None
None <+> Some(1)
```

---
layout: default
title:  "SemigroupK"
section: "typeclasses"
source: "core/src/main/scala/cats/SemigroupK.scala"
scaladoc: "#cats.SemigroupK"
---
# SemigroupK

Before introducing a `SemigroupK`, it makes sense to talk about what a
`Semigroup` is. A semigroup for some given type `A` has a single operation
(which we will call `combine`), which takes two values of type `A`, and
returns a value of type `A`. This operation must be guaranteed to be
associative. That is to say that:

```scala
((a combine b) combine c)
```

must be the same as

```scala
(a combine (b combine c))
```

for all possible values of `a`, `b`, `c`.

Cats does not define a `Semigroup` type class itself. Instead, we use the
[`Semigroup`
trait](https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Semigroup.scala)
which is defined in the [algebra
project](https://github.com/non/algebra). The [`cats` package
object](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/package.scala)
defines type aliases to the `Semigroup` from algebra, so that you can
`import cats.semigroup`.

There are instances of `Semigroup` defined for many types found in the
scala common library:

```tut:silent
import cats._
import cats.std.all._
```

Examples.

```tut:book
Semigroup[Int].combine(1, 2)
Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6))
Semigroup[Option[Int]].combine(Option(1), Option(2))
Semigroup[Option[Int]].combine(Option(1), None)
Semigroup[Int => Int].combine({(x: Int) => x + 1},{(x: Int) => x * 10}).apply(6)
```

`SemigroupK` has a very similar structure to `Semigroup`, the difference
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

For `List`, the `Semigroup` instance's `combine` operation and the `SemigroupK`
instance's `combineK` operation are both list concatenation:

```tut:book
SemigroupK[List].combineK(List(1,2,3), List(4,5,6)) == Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6))
```

However for `Option`, the `Semigroup`'s `combine` and the `SemigroupK`'s
`combineK` operation differ. Since `Semigroup` operates on fully specified
types, a `Semigroup[Option[A]]` knows the concrete type of `A` and will use
`Semigroup[A].combine` to combine the inner `A`s. Consequently,
`Semigroup[Option[A]].combine` requires an implicit `Semigroup[A]`.

In contrast, since `SemigroupK[Option]` operates on `Option` where
the inner type is not fully specified and can be anything (i.e. is
"universally quantified"). Thus, we cannot know how to `combine`
two of them. Therefore, in the case of `Option` the
`SemigroupK[Option].combineK` method has no choice but to use the
`orElse` method of Option:

```tut:book
Semigroup[Option[Int]].combine(Some(1), Some(2))
SemigroupK[Option].combineK(Some(1), Some(2))
SemigroupK[Option].combineK(Some(1), None)
SemigroupK[Option].combineK(None, Some(2))
```

There is inline syntax available for both `Semigroup` and
`SemigroupK`. Here we are following the convention from scalaz, that
`|+|` is the operator from semigroup and that `<+>` is the operator
from `SemigroupK` (called `Plus` in scalaz).

```tut:silent
import cats.syntax.all._
import cats.implicits._
import cats.std._

val one = Option(1)
val two = Option(2)
val n: Option[Int] = None
```

Thus.

```tut:book
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

```tut:nofail
Some(1) <+> None
None <+> Some(1)
```

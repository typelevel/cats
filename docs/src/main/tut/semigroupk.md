---
layout: default
title:  "SemigroupK"
section: "typeclasses"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/SemigroupK.scala"
scaladoc: "#cats.SemigroupK"
---
# SemigroupK

Before introducing a SemigroupK, it makes sense to talk about what a
Semigroup is. A semigroup for some given type A has a single operation
(which we will call `combine`), which takes two values of type A, and
returns a value of type A. This operation must be guaranteed to be
associative. That is to say that:

    ((a combine b) combine c)

must be the same as
     
    (a combine (b combine c))

for all possible values of a,b,c.

Cats does not define a Semigroup type class itself, we use the
[Semigroup
trait](https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Semigroup.scala)
which is defined in the [algebra
project](https://github.com/non/algebra) on which we depend. The
[`cats` package
object](https://github.com/non/cats/blob/master/core/src/main/scala/cats/package.scala)
defines type aliases to the Semigroup from algebra, so that you can
`import cats.semigroup`.

There are instances of Semigroup defined for many types found in the
scala common library:

```tut
import cats._
import cats.std.all._

Semigroup[Int].combine(1, 2)
Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6))
Semigroup[Option[Int]].combine(Option(1), Option(2))
Semigroup[Option[Int]].combine(Option(1), None)
Semigroup[Int => Int].combine({(x: Int) => x + 1},{(x: Int) => x * 10}).apply(6)
```

SemigroupK has a very similar structure to Semigroup, the difference
is that it operates on type constructors of one argument. So, for
example, whereas you can find a Semigroup for types which are fully
specified like `Int` or `List[Int]` or `Option[Int]`, you will find
SemigroupK for type constructors like `List` and `Option`. These types
are type constructors in that you can think of them as "functions" in
the type space. You can think of the list type as a function which
takes a concrete type, like `Int` and returns a concrete type:
`List[Int]`. This pattern would also be referred to having `kind: * ->
*`, whereas Int would have kind `*` and Map would have kind `*,* -> *`,
and, in fact, the `K` in `SemigroupK` stands for `Kind`.

For list, the `SemigroupK` instance behaves the same, it is still just
appending lists:

```tut
SemigroupK[List].combine(List(1,2,3), List(4,5,6)) == Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6))
```

However for `Option`, our `Semigroup` instance was predicated on
knowing that the type the Option was (possibly) containing itself had
a semigroup instance available, so that in the case that we were
combining a `Some` with a `Some`, we would know how to combine the two
values. With the SemigroupK instance, we are "universally quantified"
on the type parameter to Option, so we cannot know anything about
it. We cannot know, for instance, how to combine two of them, so in
the case of Option, the `combine` method has no choice but to use the
`orElse` method of Option:

```tut
SemigroupK[Option].combine(Some(1), Some(2))
SemigroupK[Option].combine(Some(1), None)
SemigroupK[Option].combine(None, Some(2))
```

There is inline syntax available for both Semigroup and
SemigroupK. Here we are following the convention from scalaz, that
`|+|` is the operator from semigroup and that `<+>` is the operator
from SemigroupK (called Plus in scalaz).

```tut
import cats.syntax.all._
import cats.implicits._
import cats.std._

val one = Option(1)
val two = Option(2)
val n: Option[Int] = None

one |+| two
one <+> two
n |+| two
n <+> two
n |+| n
n <+> n
two |+| n
two <+> n
```

You'll notice that instead of declaring `one` as `Some(1)`, I chose
`Option(1)`, and I added an explicit type declaration for `n`. This is
because there aren't type class instances for Some or None, but for
Option. If we try to use Some and None, we'll get errors:

```tut:nofail
Some(1) <+> None
None <+> Some(1)
```
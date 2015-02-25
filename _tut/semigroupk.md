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

Cats does not define a Semigroup typeclass itself, we use the
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

```scala
scala> import cats._
import cats._

scala> import cats.std.all._
import cats.std.all._

scala> Semigroup[Int].combine(1, 2)
res0: Int = 3

scala> Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6))
res1: List[Int] = List(1, 2, 3, 4, 5, 6)

scala> Semigroup[Option[Int]].combine(Option(1), Option(2))
res2: Option[Int] = Some(3)

scala> Semigroup[Option[Int]].combine(Option(1), None)
res3: Option[Int] = Some(1)

scala> Semigroup[Int => Int].combine({(x: Int) => x + 1},{(x: Int) => x * 10}).apply(6)
res4: Int = 67
```

SemigroupK has a very similar structure to Semigroup, the difference
is that it operates on type constructors of one arguement. So, for
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

```scala
scala> SemigroupK[List].combine(List(1,2,3), List(4,5,6)) == Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6))
res5: Boolean = true
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

```scala
scala> SemigroupK[Option].combine(Some(1), Some(2))
res6: Option[Int] = Some(1)

scala> SemigroupK[Option].combine(Some(1), None)
res7: Option[Int] = Some(1)

scala> SemigroupK[Option].combine(None, Some(2))
res8: Option[Int] = Some(2)
```

---
layout: default
title:  "Semigroup"
section: "typeclasses"
source: "kernel/src/main/scala/cats/kernel/Semigroup.scala"
scaladoc: "#cats.kernel.Semigroup"
---
# Semigroup

A semigroup for some given type A has a single operation
(which we will call `combine`), which takes two values of type A, and
returns a value of type A. This operation must be guaranteed to be
associative. That is to say that:

```scala
((a combine b) combine c)
```

must be the same as

```scala
(a combine (b combine c))
```

for all possible values of a,b,c. 

There are instances of `Semigroup` defined for many types found in the
scala common library:

First some imports.

```scala
import cats._
import cats.implicits._
```

Examples.

```scala
Semigroup[Int].combine(1, 2)
// res0: Int = 3

Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6))
// res1: List[Int] = List(1, 2, 3, 4, 5, 6)

Semigroup[Option[Int]].combine(Option(1), Option(2))
// res2: Option[Int] = Some(3)

Semigroup[Option[Int]].combine(Option(1), None)
// res3: Option[Int] = Some(1)

Semigroup[Int => Int].combine({(x: Int) => x + 1},{(x: Int) => x * 10}).apply(6)
// res4: Int = 67
```

Many of these types have methods defined directly on them,
which allow for such combining, e.g. `++` on List, but the
value of having a `Semigroup` type class available is that these
compose, so for instance, we can say

```scala
Map("foo" -> Map("bar" -> 5)).combine(Map("foo" -> Map("bar" -> 6), "baz" -> Map()))
// res5: Map[String,scala.collection.immutable.Map[String,Int]] = Map(foo -> Map(bar -> 11), baz -> Map())

Map("foo" -> List(1, 2)).combine(Map("foo" -> List(3,4), "bar" -> List(42)))
// res6: Map[String,List[Int]] = Map(foo -> List(1, 2, 3, 4), bar -> List(42))
```

which is far more likely to be useful than

```scala
Map("foo" -> Map("bar" -> 5)) ++  Map("foo" -> Map("bar" -> 6), "baz" -> Map())
// res7: scala.collection.immutable.Map[String,scala.collection.immutable.Map[_ <: String, Int]] = Map(foo -> Map(bar -> 6), baz -> Map())

Map("foo" -> List(1, 2)) ++ Map("foo" -> List(3,4), "bar" -> List(42))
// res8: scala.collection.immutable.Map[String,List[Int]] = Map(foo -> List(3, 4), bar -> List(42))
```

There is inline syntax available for `Semigroup`. Here we are 
following the convention from scalaz, that `|+|` is the 
operator from `Semigroup`.

```scala
import cats.implicits._

val one = Option(1)
val two = Option(2)
val n: Option[Int] = None
```

Thus.

```scala
one |+| two
// res10: Option[Int] = Some(3)

n |+| two
// res11: Option[Int] = Some(2)

n |+| n
// res12: Option[Int] = None

two |+| n
// res13: Option[Int] = Some(2)
```

You'll notice that instead of declaring `one` as `Some(1)`, I chose
`Option(1)`, and I added an explicit type declaration for `n`. This is
because there aren't type class instances for Some or None, but for
Option. If we try to use Some and None, we'll get errors:

```scala
scala> Some(1) |+| None
<console>:22: error: value |+| is not a member of Some[Int]
       Some(1) |+| None
               ^

scala> None |+| Some(1)
<console>:22: error: value |+| is not a member of object None
       None |+| Some(1)
            ^
```

N.B.
Cats defines the `Semigroup` type class in cats-kernel. The [`cats` package object](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/package.scala)
defines type aliases to the `Semigroup` from cats-kernel, so that you can
`import cats.Semigroup`.

---
layout: default
title:  "Semigroup"
section: "typeclasses"
source: "https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Semigroup.scala"

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

```tut:silent
import cats._
import cats.std.all._
import cats.implicits._
```

Examples.

```tut:book
Semigroup[Int].combine(1, 2)
Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6))
Semigroup[Option[Int]].combine(Option(1), Option(2))
Semigroup[Option[Int]].combine(Option(1), None)
Semigroup[Int => Int].combine({(x: Int) => x + 1},{(x: Int) => x * 10}).apply(6)
```

Many of these types have methods defined directly on them,
which allow for such combining, e.g. `++` on List, but the
value of having a `Semigroup` type class available is that these
compose, so for instance, we can say

```tut:book
Map("foo" -> Map("bar" -> 5)).combine(Map("foo" -> Map("bar" -> 6), "baz" -> Map()))
Map("foo" -> List(1, 2)).combine(Map("foo" -> List(3,4), "bar" -> List(42)))
```

which is far more likely to be useful than

```tut:book
Map("foo" -> Map("bar" -> 5)) ++  Map("foo" -> Map("bar" -> 6), "baz" -> Map())
Map("foo" -> List(1, 2)) ++ Map("foo" -> List(3,4), "bar" -> List(42))
```

There is inline syntax available for `Semigroup`. Here we are 
following the convention from scalaz, that `|+|` is the 
operator from `Semigroup`.

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
n |+| two
n |+| n
two |+| n
```

You'll notice that instead of declaring `one` as `Some(1)`, I chose
`Option(1)`, and I added an explicit type declaration for `n`. This is
because there aren't type class instances for Some or None, but for
Option. If we try to use Some and None, we'll get errors:

```tut:nofail
Some(1) |+| None
None |+| Some(1)
```

N.B.
Cats does not define a `Semigroup` type class itself, it uses the [`Semigroup`
trait](https://github.com/non/algebra/blob/master/core/src/main/scala/algebra/Semigroup.scala)
which is defined in the [algebra project](https://github.com/non/algebra) on 
which it depends. The [`cats` package object](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/package.scala)
defines type aliases to the `Semigroup` from algebra, so that you can
`import cats.Semigroup`.

---
layout: docs
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

```tut:silent
import cats._
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

There is inline combinator syntax available for `Semigroup`. Here Cats
follows the convention set by Scalaz, `|+|`, used to combine two semigroups. Consider
three `Option`s:

```tut:silent
import cats.implicits._

val one = Option(1)
val two = Option(2)
val n: Option[Int] = None
```

Since `Option` is a semigroup, these can be added or combined with `|+|`:

```tut:book
one |+| two
n |+| two
n |+| n
two |+| n
```

You'll notice that instead of declaring `one` as `Some(1)`, I chose
`Option(1)`, and I added an explicit type declaration for `n`. This is because
the type class instance for `Semigroup` is defined for `Option`. There is no `Semigroup`
instance defined for `None` or `Some`--you will get errors if you attempt to combine a
`Some` or `None` instance:

```tut:book
import cats.implicits._

val someInstance = Some(1)
val noneInstance = None
```
```tut:fail
val result = someInstance |+| noneInstance
```

whereby telling the compiler your vals are `Option` instances succeeds:


```tut:book
import cats.implicits._

val optionInstanceSome : Option[Int] = Some(1)
val optionInstanceNone : Option[Int] = None
val result = optionInstanceSome |+| optionInstanceNone
```

N.B.
Cats defines the `Semigroup` type class in cats-kernel. The [`cats` package object](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/package.scala)
defines type aliases to the `Semigroup` from cats-kernel, so that you can
`import cats.Semigroup`.

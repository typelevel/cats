---
layout: docs
title:  "Semigroup"
section: "typeclasses"
source: "kernel/src/main/scala/cats/kernel/Semigroup.scala"
scaladoc: "#cats.kernel.Semigroup"
---
# Semigroup

If a type `A` can form a `Semigroup` it has an **associative** binary operation.

```tut:book:silent
trait Semigroup[A] {
  def combine(x: A, y: A): A
}
```

Associativity means the following equality must hold for any choice of `x`, `y`, and
`z`.

```
combine(x, combine(y, z)) = combine(combine(x, y), z)
```

A common example of a semigroup is the type `Int` with the operation `+`.

```tut:reset:book:silent
import cats.Semigroup

implicit val intAdditionSemigroup: Semigroup[Int] = new Semigroup[Int] {
  def combine(x: Int, y: Int): Int = x + y
}

val x = 1
val y = 2
val z = 3
```

```tut:book
Semigroup[Int].combine(x, y)

Semigroup[Int].combine(x, Semigroup[Int].combine(y, z))

Semigroup[Int].combine(Semigroup[Int].combine(x, y), z)
```

Infix syntax is also available for types that have a `Semigroup` instance.

```tut:book
import cats.implicits._

1 |+| 2
```

A more compelling example which we'll see later in this tutorial is the `Semigroup`
for `Map`s.

```tut:book:silent
import cats.implicits._

val map1 = Map("hello" -> 0, "world" -> 1)
val map2 = Map("hello" -> 2, "cats"  -> 3)
```

```tut:book
Semigroup[Map[String, Int]].combine(map1, map2)

map1 |+| map2
```

# Example instances

Cats provides many `Semigroup` instances out of the box such as `Int` (`+`) and `String` (`++`)...

```tut:reset:book:silent
import cats.Semigroup
import cats.implicits._
```

```tut:book
Semigroup[Int]
Semigroup[String]
```

Instances for type constructors regardless of their type parameter such as `List` (`++`)
and `Set` (`union`)...

```tut:book
Semigroup[List[Byte]]
Semigroup[Set[Int]]

trait Foo
Semigroup[List[Foo]]
```

And instances for type constructors that depend on (one of) their type parameters having instances such
as tuples (pointwise `combine`).

```tut:book
Semigroup[(List[Foo], Int)]
```

# Example usage: Merging maps

Consider a function that merges two `Map`s that combines values if they share
the same key. It is straightforward to write these for `Map`s with values of
type say, `Int` or `List[String]`, but we can write it once and for all for
any type with a `Semigroup` instance.

```tut:book:silent
import cats.implicits._

def optionCombine[A: Semigroup](a: A, opt: Option[A]): A =
  opt.map(a |+| _).getOrElse(a)

def mergeMap[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] =
  lhs.foldLeft(rhs) {
    case (acc, (k, v)) => acc.updated(k, optionCombine(v, acc.get(k)))
  }
```

```tut:book
val xm1 = Map('a' -> 1, 'b' -> 2)
val xm2 = Map('b' -> 3, 'c' -> 4)

val x = mergeMap(xm1, xm2)

val ym1 = Map(1 -> List("hello"))
val ym2 = Map(2 -> List("cats"), 1 -> List("world"))

val y = mergeMap(ym1, ym2)
```

It is interesting to note that the type of `mergeMap` satisfies the type of `Semigroup`
specialized to `Map[K, ?]` and is associative - indeed the `Semigroup` instance for `Map`
uses the same function for its `combine`.

```tut:book
Semigroup[Map[Char, Int]].combine(xm1, xm2) == x

Semigroup[Map[Int, List[String]]].combine(ym1, ym2) == y
```

# Exploiting laws: associativity

Since we know `Semigroup#combine` must be associative, we can exploit this when writing
code against `Semigroup`. For instance, to sum a `List[Int]` we can choose to either
`foldLeft` or `foldRight` since all that changes is associativity.

```tut:book
val leftwards = List(1, 2, 3).foldLeft(0)(_ |+| _)

val rightwards = List(1, 2, 3).foldRight(0)(_ |+| _)
```

Associativity also allows us to split a list apart and sum the parts in parallel, gathering the results in
the end.

```tut:book:silent
val list = List(1, 2, 3, 4, 5)
val (left, right) = list.splitAt(2)
```

```tut:book
val sumLeft = left.foldLeft(0)(_ |+| _)
val sumRight = right.foldLeft(0)(_ |+| _)
val result = sumLeft |+| sumRight
```

However, given just `Semigroup` we cannot write the above expressions generically. For instance, we quickly
run into issues if we try to write a generic `combineAll` function.

```scala
def combineAll[A: Semigroup](as: List[A]): A =
  as.foldLeft(/* ?? what goes here ?? */)(_ |+| _)
```

`Semigroup` isn't powerful enough for us to implement this function - namely, it doesn't give us an identity
or fallback value if the list is empty. We need a power expressive abstraction, which we can find in the
`Monoid` type class.

N.B.
Cats defines the `Semigroup` type class in cats-kernel. The
[`cats` package object](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/package.scala)
defines type aliases to the `Semigroup` from cats-kernel, so that you can simply import `cats.Semigroup`.

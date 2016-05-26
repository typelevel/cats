---
layout: default
title:  "Contravariant"
section: "typeclasses"
source: "core/src/main/scala/cats/functor/Contravariant.scala"
scaladoc: "#cats.functor.Contravariant"
---
# Contravariant

The `Contravariant` type class is for functors that define a `contramap`
function with the following type:

```scala
def contramap[A, B](f: B => A): F[A] => F[B]
```

It looks like regular (also called `Covariant`) [`Functor`](functor.html)'s `map`,
but with the `f` transformation reversed.

Generally speaking, if you have some context `F[A]` for type `A`,
and you can get an `A` value out of a `B` value — `Contravariant` allows you to get the `F[B]` context for `B`.

Examples of `Contravariant` instances are [`Show`](show.html) and `scala.math.Ordering` (along with `algebra.Order`).

## Contravariant instance for Show.

Say we have class `Money` with a `Show` instance, and `Salary` class. 

```tut:silent
import cats._
import cats.implicits._

case class Money(amount: Int)
case class Salary(size: Money)

implicit val showMoney: Show[Money] = Show.show(m => s"$$${m.amount}")
```

If we want to show a `Salary` instance, we can just convert it to a `Money` instance and show it instead.

Let's use `Show`'s `Contravariant`:
  
```tut:book
implicit val showSalary: Show[Salary] = showMoney.contramap(_.size)

Salary(Money(1000)).show
```

## Contravariant instance for scala.math.Ordering.

`Show` example is trivial and quite far-fetched, let's see how `Contravariant` can help with orderings.

`scala.math.Ordering` typeclass defines comparison operations, e.g. `compare`: 

```tut:book
Ordering.Int.compare(2, 1)
Ordering.Int.compare(1, 2)
```

There's also a method, called `by`, that creates new `Orderings` out of existing ones:

```scala
def by[T, S](f: T => S)(implicit ord: Ordering[S]): Ordering[T]
```

In fact, it is just `contramap`, defined in a slightly different way! We supply `T => S` to receive `F[S] => F[T]` back.

So let's use it in our advantage and get `Ordering[Money]` for free: 

```tut:book
// we need this for `<` to work
import scala.math.Ordered._

implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)

Money(100) < Money(200)
```


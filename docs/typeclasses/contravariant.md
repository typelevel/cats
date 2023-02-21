# Contravariant

API Documentation: @:api(cats.Contravariant)

The `Contravariant` type class is for functors that define a `contramap`
function with the following type:

```scala
def contramap[A, B](fa: F[A])(f: B => A): F[B]
```

It looks like regular (also called `Covariant`) [`Functor`](functor.md)'s `map`,
but with the `f` transformation reversed.

Generally speaking, if you have some context `F[A]` for type `A`,
and you can get an `A` value out of a `B` value — `Contravariant` allows you to get the `F[B]` context for `B`.

Examples of `Contravariant` instances are [`Show`](show.md) and `scala.math.Ordering` (along with `cats.kernel.Order`).

## Contravariant instance for Show.

Say we have a class `Money` with a `Show` instance, and a `Salary` class:

```scala mdoc:silent
import cats._

import cats.syntax.all._

case class Money(amount: Int)
case class Salary(size: Money)

implicit val showMoney: Show[Money] = Show.show(m => s"$$${m.amount}")
```

If we want to show a `Salary` instance, we can just convert it to a `Money` instance and show that instead.

Let's use `Show`'s `Contravariant`:

```scala mdoc
implicit val showSalary: Show[Salary] = showMoney.contramap(_.size)

Salary(Money(1000)).show
```

## Contravariant instance for scala.math.Ordering.

The `Show` example is trivial and quite far-fetched, let's see how `Contravariant` can help with orderings.

The `scala.math.Ordering` type class defines comparison operations, e.g. `compare`:

```scala mdoc
Ordering.Int.compare(2, 1)
Ordering.Int.compare(1, 2)
```

There's also a method, called `by`, that creates new `Orderings` out of existing ones:

```scala
def by[T, S](f: T => S)(implicit ord: Ordering[S]): Ordering[T]
```

In fact, it is just `contramap`, defined in a slightly different way! We supply `T => S` to receive `F[S] => F[T]` back.

So let's use it to our advantage and get `Ordering[Money]` for free:

```scala mdoc
// we need this for `<` to work
import scala.math.Ordered._

implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)

Money(100) < Money(200)
```

## Subtyping

Contravariant functors have a natural relationship with subtyping, dual to that of covariant functors:

```scala mdoc
class A
class B extends A
val b: B = new B
val a: A = b
val showA: Show[A] = Show.show(a => "a!")
val showB1: Show[B] = showA.contramap(b => b: A)
val showB2: Show[B] = showA.contramap(identity[A])
val showB3: Show[B] = Contravariant[Show].narrow[A, B](showA)
```

Subtyping relationships are "lifted backwards" by contravariant functors, such that if `F` is a
lawful contravariant functor and `B <: A` then `F[A] <: F[B]`, which is expressed by `Contravariant.narrow`.

---
layout: docs
title: "Divisible"
section: "typeclasses"
source: "core/src/main/scala/cats/Divisible.scala"
scaladoc: "#cats.Divisible"
---
# Divisible

The `Divisible` type class is for contravariant functors that define a
`contramap2` function that looks like:

```scala
def contramap2[A, B, C](fb: F[B], fc: F[C])(f: A => (B, C)): F[A]
```

This is similar to the `map2` function on the `Apply` typeclass, but

This is part of what makes people say `Divisible` is a
contravariant version of `Applicative`.

Basically, if you have two contexts `F[B]` and `F[C]` for types
`B` and `C`, as well as a way to produce types `B` and `C` simultaneously
from a type `A`, then `Divisible` allows you to obtain
a context `F[A]` for the type `A`.

Examples of `Divisible` instances are [`Eq`](eq.html) and [`Const`](../datatypes/const.html),
but there are also interesting instances for other types.

## Predicates Have `Divisible`

An example application would be the case of predicates. Consider the type,

```tut:book:silent
import cats._

import cats.implicits._

case class Predicate[A](run: A => Boolean)
```

Then, we can exhibit a `Divisible` for `Predicate` by basing it on the
`Monoid` for `Boolean` via `&&` as,

```tut:book:silent
implicit val divisiblePredicate: Divisible[Predicate] =
  new Divisible [Predicate] {
    def unit[A]: Predicate[A] = Predicate[A](Function.const(true))

    def contramap2[A, B, C](fb: Predicate[B], fc: Predicate[C]
      )(f: A => (B, C)): Predicate [A] = Predicate(
      (a: A) => f(a) match {
        case (b, c) => fb.run(b) && fc.run(c)
      })
  }
```

Just like for `Contravariant`, we can `contramap` to
pull `Predicates` back along functions.

```tut:book
case class Money(value: Long)
def isEven: Predicate[Long] = Predicate(_ % 2 == 0)

def isEvenMoney: Predicate[Money] = isEven.contramap(_.value)

isEvenMoney.run(Money(55))
```

More interestingly, we can combine two predicates using
a `contramap2`.

```tut:book
case class Transaction(value: Money, payee: String)

def isEvan: Predicate[String] = Predicate(_ == "Evan")

def isEvenPaymentToEvan =
  isEvenMoney.contramap2(isEvan)(
    (trans: Transaction) => (trans.value, trans.payee))

isEvenPaymentToEvan.run(Transaction(Money(56), "Evan"))
```

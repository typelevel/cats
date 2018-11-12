---
layout: docs
title: "Decideable"
section: "typeclasses"
source: "core/src/main/scala/cats/Decideable.scala"
scaladoc: "#cats.Decideable"
---
# Decideable

The `Decideable` type class is an extension of [`ContravariantMonoidal`](contravariantmonoidal.html) that allows the
definition of a `sum` function, allowing us to make choices in consuming our input.

It is the contravariant analogue of the [`Alternative`](alternative.html) type class.

```tut:silent
import cats.ContravariantMonoidal

trait Decideable[F[_]] extends ContravariantMonoidal[F] {
  def sum[A, B](fa: F[A], fb: F[B]): Either[F[A], F[B]]

  def decide[A, B, C](fa: F[A], fb: F[B])(f: C => Either[A, B]): F[C] =
    contramap(sum(fa, fb))(f)
}
```

`sum` lets us implement the `decide` function. Note how `decide` says, no matter what the outcome of a possibly multi-outcome procedure, I can consume either outcome.

In other words, if we have two contexts `F[A]` and `F[B]` and we have a procedure `C => Either[A, B]` that could produce either an A or a B depending on the input, we have a way to produce a context `F[C]`.

The classic example of a `Decideable` is that of predicates `A => Boolean`, leaning on the fact that both `&&` and `^` are valid monoids on `Boolean`.

We can write the instance as:

```tut:silent
implicit val decideableForPredicates = new Decideable[? => Boolean] {
  def empty[A]: A => Boolean = Function.const(false)
  def unit: Unit => Boolean = Function.const(true)
  def contramap[A, B](fa: A => Boolean)(f: B => A): B => Boolean =
    fa.compose(f)
  def product[A, B](fa: A => Boolean, fb: B => Boolean): ((A, B)) => Boolean =
    (ab: (A, B)) =>
      ab match {
        case (a, b) => fa(a) && fb(b)
    }
  def sum[A, B](fa: A => Boolean, fb: B => Boolean): Either[A, B] => Boolean =
    either => either.fold(fa, fb)
}
```

This can be used to combine predicates over both coproducts and products.

```tut
def isEven(i: Int): Boolean = i % 2 == 0
def isDivisibleByThree(l: Long): Boolean = l % 3 == 0

def isEvenRightOrDivisibleThreeLeft: Either[Int, Long] => Boolean =
  Decideable[? => Boolean].sum(isEven, isDivisibleByThree)

def isEvenRightAndDivisibleThreeLeft: (Int, Long) => Boolean =
  Decideable[? => Boolean].product(isEven, isDivisibleByThree)
```

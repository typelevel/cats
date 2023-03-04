# Contravariant Monoidal

API Documentation: @:api(cats.ContravariantMonoidal)

The `ContravariantMonoidal` type class is for [`Contravariant`](contravariant.md) functors that can define a
`product` function and a `unit` function.

```scala mdoc:silent
import cats.Contravariant

trait ContravariantMonoidal[F[_]] extends Contravariant[F] {
  def unit: F[Unit]

  def product[A, B](fa: F[A], fc: F[B]): F[(A, B)]

  def contramap2[A, B, C](fb: F[B], fc: F[C])(f: A => (B, C)): F[A] =
    contramap(product(fb, fc))(f)
}
```

Notice that this allows us to define the `contramap2` function, much like
the `map2` function and the `pure` function on the `Applicative` typeclass, but in reverse.

Basically, if you have two contexts `F[B]` and `F[C]` for types
`B` and `C`, as well as a way to produce types `B` and `C` simultaneously
from a type `A`, then `ContravariantMonoidal` allows you to obtain
a context `F[A]` for the type `A`.

Examples of `ContravariantMonoidal` instances are [`Eq`](eq.md) and [`Const`](../datatypes/const.md),
but there are also interesting instances for other types.

## Predicates Have `ContravariantMonoidal`

An example application would be the case of predicates. Consider the type,

```scala mdoc:silent:reset
import cats._

import cats.syntax.all._

case class Predicate[A](run: A => Boolean)
```

Then, we can exhibit a `ContravariantMonoidal` for `Predicate` by basing it on the
`Monoid` for `Boolean` via `&&` as,

```scala mdoc:silent
implicit val contravariantMonoidalPredicate: ContravariantMonoidal[Predicate] =
  new ContravariantMonoidal [Predicate] {
    def unit: Predicate[Unit] = Predicate[Unit](Function.const(true))

    def product[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[(A, B)] =
      Predicate(x => fa.run(x._1) && fb.run(x._2))

    def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] =
      Predicate(x => fa.run(f(x)))
  }
```

We could have also used `false` and `||`, but the "and" version
tends to be a little more convenient for this application.

Just like for `Contravariant`, we can `contramap` to
pull `Predicates` back along functions.

```scala mdoc
case class Money(value: Long)
def isEven: Predicate[Long] = Predicate(_ % 2 == 0)

def isEvenMoney: Predicate[Money] = isEven.contramap(_.value)

isEvenMoney.run(Money(55))
```

We can also lift functions contravariantly into
the context instead of contramapping repeatedly.

```scala mdoc
def times2Predicate: Predicate[Long] => Predicate[Long] =
  ContravariantMonoidal[Predicate].liftContravariant((x: Long) => 2*x)

def liftMoney: Predicate[Long] => Predicate[Money] =
  ContravariantMonoidal[Predicate].liftContravariant(_.value)

def trivial = times2Predicate(isEven)
trivial.run(2)
trivial.run(5)
```

More interestingly, we can combine multiple predicates using
a `contramapN`.

```scala mdoc
case class Transaction(value: Money, payee: String)

def isEvan: Predicate[String] = Predicate(_ == "Evan")

def isGreaterThan50Dollars: Predicate[Money] = liftMoney(Predicate(_ > 50))

def isEvenPaymentToEvanOfMoreThan50 =
  (isEvenMoney, isGreaterThan50Dollars, isEvan).contramapN(
    (trans: Transaction) => (trans.value, trans.value, trans.payee))

isEvenPaymentToEvanOfMoreThan50.run(Transaction(Money(56), "Evan"))
```

# Decidable

The `Decidable` type class is an extension of [`ContravariantMonoidal`](contravariantmonoidal.md) that allows the
definition of a `sum` function, allowing us to make choices in consuming our input.

It is the contravariant analogue of the [`Alternative`](alternative.md) type class.

```scala mdoc:silent
import cats.ContravariantMonoidal

trait Decidable[F[_]] extends ContravariantMonoidal[F] {
  def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
  def zero[A]: F[Nothing]

  def decide[A, B, C](fa: F[A], fb: F[B])(f: C => Either[A, B]): F[C] =
    contramap(sum(fa, fb))(f)
}
object Decidable {
  def apply[F[_]](implicit dec: Decidable[F]): Decidable[F] =
    dec
}
```

`sum` lets us implement the `decide` function. Note how `decide` says, no matter what the outcome of a possibly multi-outcome procedure, I can consume either outcome.

In other words, if we have two contexts `F[A]` and `F[B]` and we have a procedure `C => Either[A, B]` that could produce either an A or a B depending on the input, we have a way to produce a context `F[C]`.

The classic example of a `Decidable` is that of predicates `A => Boolean`, leaning on the fact that both `&&` and `^` are valid monoids on `Boolean`.

We can write the instance as:

```scala mdoc:silent
implicit val decideableForPredicates = new Decidable[* => Boolean] {
  def zero[A]: Nothing => Boolean = _ => true 
  def unit: Unit => Boolean = Function.const(false)
  def contramap[A, B](fa: A => Boolean)(f: B => A): B => Boolean =
    fa.compose(f)
  def product[A, B](fa: A => Boolean, fb: B => Boolean): ((A, B)) => Boolean =
    (ab: (A, B)) =>
      ab match {
        case (a, b) => fa(a) || fb(b)
    }
  def sum[A, B](fa: A => Boolean, fb: B => Boolean): Either[A, B] => Boolean =
    either => either.fold(fa, fb)
}
```

This can be used to combine predicates over both sum types and product types.

```scala mdoc
def isEven(i: Int): Boolean = i % 2 == 0
def isDivisibleByThree(l: Long): Boolean = l % 3 == 0

def isEvenRightOrDivisibleThreeLeft: Either[Int, Long] => Boolean =
  Decidable[* => Boolean].sum(isEven, isDivisibleByThree)

def isEvenRightAndDivisibleThreeLeft: ((Int, Long)) => Boolean =
  Decidable[* => Boolean].product(isEven, isDivisibleByThree)
```

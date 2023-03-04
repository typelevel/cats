# Monad

API Documentation: @:api(cats.Monad)

`Monad` extends the [`Applicative`](applicative.md) type class with a
new function `flatten`. Flatten takes a value in a nested context (eg.
`F[F[A]]` where F is the context) and "joins" the contexts together so
that we have a single context (ie. `F[A]`).

The name `flatten` should remind you of the functions of the same name on many
classes in the standard library.

```scala mdoc
Option(Option(1)).flatten
Option(None).flatten
List(List(1),List(2,3)).flatten
```

### Monad instances

If `Applicative` is already present and `flatten` is well-behaved,
extending the `Applicative` to a `Monad` is trivial. To provide evidence
that a type belongs in the `Monad` type class, cats' implementation
requires us to provide an implementation of `pure` (which can be reused
from `Applicative`) and `flatMap`.

We can use `flatten` to define `flatMap`: `flatMap` is just `map`
followed by `flatten`. Conversely, `flatten` is just `flatMap` using
the identity function `x => x` (i.e. `flatMap(_)(x => x)`).

```scala mdoc:silent
import cats._

implicit def optionMonad(implicit app: Applicative[Option]) =
  new Monad[Option] {
    // Define flatMap using Option's flatten method
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      app.map(fa)(f).flatten
    // Reuse this definition from Applicative.
    override def pure[A](a: A): Option[A] = app.pure(a)

    @annotation.tailrec
    def tailRecM[A, B](init: A)(fn: A => Option[Either[A, B]]): Option[B] =
      fn(init) match {
        case None => None
        case Some(Right(b)) => Some(b)
        case Some(Left(a)) => tailRecM(a)(fn)
      }
  }
```

### flatMap

`flatMap` is often considered to be the core function of `Monad`, and Cats
follows this tradition by providing implementations of `flatten` and `map`
derived from `flatMap` and `pure`.

Part of the reason for this is that name `flatMap` has special significance in
scala, as for-comprehensions rely on this method to chain together operations
in a monadic context.

```scala mdoc
import scala.reflect.runtime.universe

universe.reify(
  for {
    x <- Some(1)
    y <- Some(2)
  } yield x + y
).tree
```

### tailRecM

In addition to requiring `flatMap` and `pure`, Cats has chosen to require
`tailRecM` which encodes stack safe monadic recursion, as described in
[Stack Safety for Free](http://functorial.com/stack-safety-for-free/index.pdf) by
Phil Freeman. Because monadic recursion is so common in functional programming but
is not stack safe on the JVM, Cats has chosen to require this method of all monad implementations
as opposed to just a subset. All functions requiring monadic recursion in Cats do so via
`tailRecM`.

An example `Monad` implementation for `Option` is shown below. Note the tail recursive
and therefore stack safe implementation of `tailRecM`.

```scala mdoc:silent:reset

import cats.Monad
import scala.annotation.tailrec

implicit val optionMonad = new Monad[Option] {
  def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  def pure[A](a: A): Option[A] = Some(a)

  @tailrec
  def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
    case None              => None
    case Some(Left(nextA)) => tailRecM(nextA)(f) // continue the recursion
    case Some(Right(b))    => Some(b)            // recursion done
  }
}
```

More discussion about `tailRecM` can be found in the [FAQ](../faq.md#what-is-tailrecm).


### ifM

`Monad` provides the ability to choose later operations in a sequence based on
the results of earlier ones. This is embodied in `ifM`, which lifts an `if`
statement into the monadic context.

```scala mdoc
import cats.syntax.all._

Monad[List].ifM(List(true, false, true))(ifTrue = List(1, 2), ifFalse = List(3, 4))
```

### Composition
Unlike [`Functor`s](functor.md) and [`Applicative`s](applicative.md),
not all `Monad`s compose. This means that even if `M[_]` and `N[_]` are
both `Monad`s, `M[N[_]]` is not guaranteed to be a `Monad`.

However, many common cases do. One way of expressing this is to provide
instructions on how to compose any outer monad (`F` in the following
example) with a specific inner monad (`Option` in the following
example).

*Note*: the example below assumes usage of the [kind-projector compiler plugin](https://github.com/typelevel/kind-projector) and will not compile if it is not being used in a project.

```scala mdoc:silent:reset
import cats.Monad
import cats.syntax.all._

case class OptionT[F[_], A](value: F[Option[A]])

implicit def optionTMonad[F[_]](implicit F: Monad[F]): Monad[OptionT[F, *]] = {
  new Monad[OptionT[F, *]] {
    def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
    def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
      OptionT {
        F.flatMap(fa.value) {
          case None => F.pure(None)
          case Some(a) => f(a).value
        }
      }

    def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
      OptionT {
        F.tailRecM(a)(a0 => F.map(f(a0).value) {
          case None => Either.right[A, Option[B]](None)
          case Some(b0) => b0.map(Some(_))
        })
      }
  }
}
```

This sort of construction is called a monad transformer.

Cats has an [`OptionT`](../datatypes/optiont.md) monad transformer, which adds a lot of useful functions to the simple implementation above.

## FlatMap - a weakened Monad
A closely related type class is `FlatMap` which is identical to `Monad`, minus the `pure`
method. Indeed in Cats `Monad` is a subclass of `FlatMap` (from which it gets `flatMap`)
and `Applicative` (from which it gets `pure`).

```scala
trait FlatMap[F[_]] extends Apply[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

trait Monad[F[_]] extends FlatMap[F] with Applicative[F]
```

The laws for `FlatMap` are just the laws of `Monad` that don't mention `pure`.

One of the motivations for `FlatMap`'s existence is that some types have `FlatMap` instances but not
`Monad` - one example is `Map[K, *]`. Consider the behavior of `pure` for `Map[K, A]`. Given
a value of type `A`, we need to associate some arbitrary `K` to it but we have no way of doing that.

However, given existing `Map[K, A]` and `Map[K, B]` (or `Map[K, A => B]`), it is straightforward to
pair up (or apply functions to) values with the same key. Hence `Map[K, *]` has an `FlatMap` instance.


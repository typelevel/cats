---
layout: default
title:  "Monad"
section: "typeclasses"
source: "core/src/main/scala/cats/Monad.scala"
scaladoc: "#cats.Monad"
---
# Monad

`Monad` extends the [`Applicative`](applicative.html) type class with a
new function `flatten`. Flatten takes a value in a nested context (eg.
`F[F[A]]` where F is the context) and "joins" the contexts together so
that we have a single context (ie. `F[A]`).

The name `flatten` should remind you of the functions of the same name on many
classes in the standard library.

```tut:book
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

```tut:silent
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

`flatMap` is often considered to be the core function of `Monad`, and cats
follows this tradition by providing implementations of `flatten` and `map`
derived from `flatMap` and `pure`.

```tut:silent
implicit val listMonad = new Monad[List] {
  def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  def pure[A](a: A): List[A] = List(a)
  def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] =
    defaultTailRecM(a)(f)
}
```

Part of the reason for this is that name `flatMap` has special significance in
scala, as for-comprehensions rely on this method to chain together operations
in a monadic context.

```tut:book
import scala.reflect.runtime.universe

universe.reify(
  for {
    x <- Some(1)
    y <- Some(2)
  } yield x + y
).tree
```

### ifM

`Monad` provides the ability to choose later operations in a sequence based on
the results of earlier ones. This is embodied in `ifM`, which lifts an `if`
statement into the monadic context.

```tut:book
Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4))
```

### Composition
Unlike [`Functor`s](functor.html) and [`Applicative`s](applicative.html),
not all `Monad`s compose. This means that even if `M[_]` and `N[_]` are
both `Monad`s, `M[N[_]]` is not guaranteed to be a `Monad`.

However, many common cases do. One way of expressing this is to provide
instructions on how to compose any outer monad (`F` in the following
example) with a specific inner monad (`Option` in the following
example).

*Note*: the example below assumes usage of the [kind-projector compiler plugin](https://github.com/non/kind-projector) and will not compile if it is not being used in a project.

```tut:silent
case class OptionT[F[_], A](value: F[Option[A]])

implicit def optionTMonad[F[_]](implicit F : Monad[F]) = {
  new Monad[OptionT[F, ?]] {
    def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
    def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
      OptionT {
        F.flatMap(fa.value) {
          case None => F.pure(None)
          case Some(a) => f(a).value
        }
      }
    def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
      defaultTailRecM(a)(f)
  }
}
```

This sort of construction is called a monad transformer.

Cats has an [`OptionT`](optiont.html) monad transformer, which adds a lot of useful functions to the simple implementation above.

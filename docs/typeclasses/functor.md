# Functor

API Documentation: @:api(cats.Functor)

`Functor` is a type class that abstracts over type constructors that can be `map`'ed over. Examples of such
type constructors are `List`, `Option`, and `Future`.

```scala mdoc:silent
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// Example implementation for Option
implicit val functorForOption: Functor[Option] = new Functor[Option] {
  def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
    case None    => None
    case Some(a) => Some(f(a))
  }
}
```

A `Functor` instance must obey two laws:

* Composition: Mapping with `f` and then again with `g` is the same as mapping once with the composition of `f` and `g`
    * `fa.map(f).map(g) = fa.map(f.andThen(g))`

* Identity: Mapping with the identity function is a no-op
    * `fa.map(x => x) = fa`

## A different view

Another way of viewing a `Functor[F]` is that `F` allows the lifting of a pure function `A => B` into the effectful
function `F[A] => F[B]`. We can see this if we re-order the `map` signature above.

```scala mdoc:silent:nest
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)
}
```

## Functors for effect management

The `F` in `Functor` is often referred to as an "effect" or "computational context." Different effects will
abstract away different behaviors with respect to fundamental functions like `map`. For instance, `Option`'s effect
abstracts away potentially missing values, where `map` applies the function only in the `Some` case but
otherwise threads the `None` through.

Taking this view, we can view `Functor` as the ability to work with a **single** effect - we can apply a pure
function to a single effectful value without needing to "leave" the effect.

## Functors compose

If you've ever found yourself working with nested data types such as `Option[List[A]]` or
`List[Either[String, Future[A]]]` and tried to `map` over it, you've most likely found yourself doing something
like `_.map(_.map(_.map(f)))`. As it turns out, `Functor`s compose, which means if `F` and `G` have
`Functor` instances, then so does `F[G[_]]`.

Such composition can be achieved via the `Functor#compose` method.

```scala mdoc:reset:silent
import cats.Functor
import cats.syntax.all._
```

```scala mdoc
val listOption = List(Some(1), None, Some(2))

// Through Functor#compose
Functor[List].compose[Option].map(listOption)(_ + 1)
```

This approach will allow us to use composition without wrapping the value in question, but can
introduce complications in more complex use cases. For example, if we need to call another function which
requires a `Functor` and we want to use the composed `Functor`, we would have to explicitly pass in the
composed instance during the function call or create a local implicit.

```scala mdoc:silent
def needsFunctor[F[_]: Functor, A](fa: F[A]): F[Unit] = Functor[F].map(fa)(_ => ())

def foo: List[Option[Unit]] = {
  val listOptionFunctor = Functor[List].compose[Option]
  type ListOption[A] = List[Option[A]]
  needsFunctor[ListOption, Int](listOption)(listOptionFunctor)
}
```

We can make this nicer at the cost of boxing with the `Nested` data type.

```scala mdoc:silent
import cats.data.Nested
import cats.syntax.all._
```

```scala mdoc
val nested: Nested[List, Option, Int] = Nested(listOption)

nested.map(_ + 1)
```

The `Nested` approach, being a distinct type from its constituents, will resolve the usual way modulo
possible [SI-2712][si2712] issues (which can be addressed through [partial unification][partial-unification]),
but requires syntactic and runtime overhead from wrapping and unwrapping.

[partial-unification]: https://github.com/fiadliel/sbt-partial-unification "A sbt plugin for enabling partial unification"
[si2712]: https://issues.scala-lang.org/browse/SI-2712 "SI-2712: implement higher-order unification for type constructor inference"

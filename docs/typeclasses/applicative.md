# Applicative
`Applicative` extends [`Functor`](functor.md) with an `ap` and `pure` method.

```scala mdoc:silent
import cats.Functor

trait Applicative[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
}
```

`pure` wraps the value into the type constructor - for `Option` this could be `Some(_)`, for `Future`
`Future.successful`, and for `List` a singleton list.

`ap` is a bit tricky to explain and motivate, so we'll look at an alternative but equivalent
formulation via `product`.

```scala mdoc:silent:nest
trait Applicative[F[_]] extends Functor[F] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  def pure[A](a: A): F[A]
}

// Example implementation for right-biased Either
implicit def applicativeForEither[L]: Applicative[Either[L, *]] = new Applicative[Either[L, *]] {
  def product[A, B](fa: Either[L, A], fb: Either[L, B]): Either[L, (A, B)] = (fa, fb) match {
    case (Right(a), Right(b)) => Right((a, b))
    case (Left(l) , _       ) => Left(l)
    case (_       , Left(l) ) => Left(l)
  }

  def pure[A](a: A): Either[L, A] = Right(a)

  def map[A, B](fa: Either[L, A])(f: A => B): Either[L, B] = fa match {
    case Right(a) => Right(f(a))
    case Left(l)  => Left(l)
  }
}
```

Note that in this formulation `map` is left abstract, whereas in the previous one with `ap` `map`
could be implemented in terms of `ap` and `pure`. This suggests that `ap` is equivalent to
`map` and `product`, which is indeed the case.

Such an `Applicative` must obey three laws:

* Associativity: No matter the order in which you product together three values, the result is isomorphic
    * `fa.product(fb).product(fc) ~ fa.product(fb.product(fc))`
    * With `map`, this can be made into an equality with `fa.product(fb).product(fc) = fa.product(fb.product(fc)).map { case (a, (b, c)) => ((a, b), c) }`
* Left identity: Zipping a value on the left with unit results in something isomorphic to the original value
    * `pure(()).product(fa) ~ fa`
    * As an equality: `pure(()).product(fa).map(_._2) = fa`
* Right identity: Zipping a value on the right with unit results in something isomorphic to the original value
    * `fa.product(pure(())) ~ fa`
    * As an equality: `fa.product(pure(())).map(_._1) = fa`

## Applicatives for effect management

If we view `Functor` as the ability to work with a single effect, `Applicative` encodes working with
multiple **independent** effects. Between `product` and `map`, we can take two separate effectful values
and compose them. From there we can generalize to working with any N number of independent effects.

```scala mdoc:reset:silent
import cats.Applicative

def product3[F[_]: Applicative, A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] = {
  val F = Applicative[F]
  val fabc = F.product(F.product(fa, fb), fc)
  F.map(fabc) { case ((a, b), c) => (a, b, c) }
}
```

## What is ap?

Let's see what happens if we try to compose two effectful values with just `map`.

```scala mdoc:silent
import cats.implicits._

val f: (Int, Char) => Double = (i, c) => (i + c).toDouble

val int: Option[Int] = Some(5)
val char: Option[Char] = Some('a')
```

```scala mdoc
int.map(i => (c: Char) => f(i, c)) // what now?
```

We have an `Option[Char => Double]` and an `Option[Char]` to which we want to apply the function to,
but `map` doesn't give us enough power to do that. Hence, `ap`.

## Applicatives compose

Like [`Functor`](functor.md), `Applicative`s compose. If `F` and `G` have `Applicative` instances, then so
does `F[G[_]]`.

```scala mdoc:silent
import cats.data.Nested
import cats.implicits._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

val x: Future[Option[Int]] = Future.successful(Some(5))
val y: Future[Option[Char]] = Future.successful(Some('a'))
```

```scala mdoc
val composed = Applicative[Future].compose[Option].map2(x, y)(_ + _)

val nested = Applicative[Nested[Future, Option, *]].map2(Nested(x), Nested(y))(_ + _)
```

## Traverse

The straightforward way to use `product` and `map` (or just `ap`) is to compose `n` independent effects,
where `n` is a fixed number. In fact there are convenience methods named `apN`, `mapN`, and `tupleN` (replacing
`N` with a number 2 - 22) to make it even easier.

Imagine we have one `Option` representing a username, one representing a password, and another representing
a URL for logging into a database.

```scala mdoc:silent
import java.sql.Connection

val username: Option[String] = Some("username")
val password: Option[String] = Some("password")
val url: Option[String] = Some("some.login.url.here")

// Stub for demonstration purposes
def attemptConnect(username: String, password: String, url: String): Option[Connection] = None
```

We know statically we have 3 `Option`s, so we can use `map3` specifically.

```scala mdoc
Applicative[Option].map3(username, password, url)(attemptConnect)
```

Sometimes we don't know how many effects will be in play - perhaps we are receiving a list from user
input or getting rows from a database. This implies the need for a function:

```scala mdoc:silent
def sequenceOption[A](fa: List[Option[A]]): Option[List[A]] = ???

// Alternatively..
def traverseOption[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???
```

Users of the standard library `Future.sequence` or `Future.traverse` will find these names and signatures
familiar.

Let's implement `traverseOption` (you can implement `sequenceOption` in terms of `traverseOption`).

```scala mdoc:silent:nest
def traverseOption[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as.foldRight(Some(List.empty[B]): Option[List[B]]) { (a: A, acc: Option[List[B]]) =>
    val optB: Option[B] = f(a)
    // optB and acc are independent effects so we can use Applicative to compose
    Applicative[Option].map2(optB, acc)(_ :: _)
  }

traverseOption(List(1, 2, 3))(i => Some(i): Option[Int])
```

This works...but if we look carefully at the implementation there's nothing `Option`-specific going on. As
another example let's implement the same function but for `Either`.

```scala mdoc:silent
import cats.implicits._

def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  as.foldRight(Right(List.empty[B]): Either[E, List[B]]) { (a: A, acc: Either[E, List[B]]) =>
    val eitherB: Either[E, B] = f(a)
    Applicative[Either[E, *]].map2(eitherB, acc)(_ :: _)
  }
```

```scala mdoc
traverseEither(List(1, 2, 3))(i => if (i % 2 != 0) Left(s"${i} is not even") else Right(i / 2))
```

The implementation of `traverseOption` and `traverseEither` are more or less identical, modulo the initial
"accumulator" to `foldRight`. But even that could be made the same by delegating to `Applicative#pure`!
Generalizing `Option` and `Either` to any `F[_]: Applicative` gives us the fully polymorphic version.
Existing data types with `Applicative` instances (`Future`, `Option`, `Either[E, *]`, `Try`) can call it by fixing `F`
appropriately, and new data types need only be concerned with implementing `Applicative` to do so as well.

```scala mdoc:silent
def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
  as.foldRight(Applicative[F].pure(List.empty[B])) { (a: A, acc: F[List[B]]) =>
    val fb: F[B] = f(a)
    Applicative[F].map2(fb, acc)(_ :: _)
  }
```

This function is provided by Cats via the `Traverse[List]` instance and syntax, which is covered in another
tutorial.

```scala mdoc:silent
import cats.implicits._
```

```scala mdoc
List(1, 2, 3).traverse(i => Some(i): Option[Int])
```

With this addition of `traverse`, we can now compose any number of independent effects, statically known or otherwise.

## Apply - a weakened Applicative
A closely related type class is `Apply` which is identical to `Applicative`, modulo the `pure`
method. Indeed in Cats `Applicative` is a subclass of `Apply` with the addition of this method.

```scala
trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
}
```

The laws for `Apply` are just the laws of `Applicative` that don't mention `pure`. In the laws given
above, the only law would be associativity.

One of the motivations for `Apply`'s existence is that some types have `Apply` instances but not
`Applicative` - one example is `Map[K, *]`. Consider the behavior of `pure` for `Map[K, A]`. Given
a value of type `A`, we need to associate some arbitrary `K` to it but we have no way of doing that.

However, given existing `Map[K, A]` and `Map[K, B]` (or `Map[K, A => B]`), it is straightforward to
pair up (or apply functions to) values with the same key. Hence `Map[K, *]` has an `Apply` instance.

## Syntax

Syntax for `Applicative` (or `Apply`) is available under the `cats.implicits._` import. The most
interesting syntax is focused on composing independent effects: it works just like the methods
for composition we saw above (`map3`, `tuple3`, etc.), but achieves a slightly friendlier syntax
by enriching Scala's standard tuple types.


For example, we've already seen this code for mapping over three options together:

```scala mdoc
Applicative[Option].map3(username, password, url)(attemptConnect)
```
With the applicative syntax, we can change this to the slightly shorter:

```scala mdoc
import cats.implicits._

(username, password, url).mapN(attemptConnect)
```

We don't have to mention the type or specify the number of values we're composing
together, so there's a little less boilerplate here.

Another very useful `Apply` syntax is `tupled`, which allows a tuple of effectful values to be composed into a single effectful value containing a tuple. 

```scala mdoc
import cats.implicits._

val optPair: Option[(String, String)] = (username, password).tupled
```

### See also Parallel variants

Both `tupled` and `mapN` have [parallel](parallel.md) variant operations, named `parTupled` and `parMapN` respectively. Regular `tupled`/`mapN` evaluate their effects from left to right ("sequentially"), while `parTupled`/`parMapN` evaluate in an indeterminate order, or in parallel.

The difference can be understood intuitively when the effect is an executable task, such as `IO` from [Cats Effect](https://typelevel.org/cats-effect/docs/concepts#concurrent). In this case, the parallel variants enable you to compose tuples of tasks into a single task that will run its sub-tasks concurrently.

## Further Reading

* [Applicative Programming with Effects][applicativePaper] - McBride, Patterson. JFP 2008.

[applicativePaper]: http://www.staff.city.ac.uk/~ross/papers/Applicative.html "Applicative Programming with Effects"

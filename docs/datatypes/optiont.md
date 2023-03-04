# OptionT

API Documentation: @:api(cats.data.OptionT)

`OptionT[F[_], A]` is a light wrapper on an `F[Option[A]]`. Speaking technically, it is a monad transformer for `Option`, but you don't need to know what that means for it to be useful. `OptionT` can be more convenient to work with than using `F[Option[A]]` directly.

## Reduce map boilerplate

Consider the following scenario:

```scala mdoc:silent
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

val customGreeting: Future[Option[String]] = Future.successful(Some("welcome back, Lola"))
```

We want to try out various forms of our greetings.

```scala mdoc:silent
val excitedGreeting: Future[Option[String]] = customGreeting.map(_.map(_ + "!"))

val hasWelcome: Future[Option[String]] = customGreeting.map(_.filter(_.contains("welcome")))

val noWelcome: Future[Option[String]] = customGreeting.map(_.filterNot(_.contains("welcome")))

val withFallback: Future[String] = customGreeting.map(_.getOrElse("hello, there!"))
```

As you can see, the implementations of all of these variations are very similar. We want to call the `Option` operation (`map`, `filter`, `filterNot`, `getOrElse`), but since our `Option` is wrapped in a `Future`, we first need to `map` over the `Future`.

`OptionT` can help remove some of this boilerplate. It exposes methods that look like those on `Option`, but it handles the outer `map` call on the `Future` so we don't have to:

```scala mdoc:silent:nest
import cats.data.OptionT
import cats.syntax.all._

val customGreetingT: OptionT[Future, String] = OptionT(customGreeting)

val excitedGreeting: OptionT[Future, String] = customGreetingT.map(_ + "!")

val withWelcome: OptionT[Future, String] = customGreetingT.filter(_.contains("welcome"))

val noWelcome: OptionT[Future, String] = customGreetingT.filterNot(_.contains("welcome"))

val withFallback: Future[String] = customGreetingT.getOrElse("hello, there!")
```

## From `Option[A]` and/or `F[A]` to `OptionT[F, A]`

Sometimes you may have an `Option[A]` and/or `F[A]` and want to *lift* them into an `OptionT[F, A]`. For this purpose `OptionT` exposes two useful methods, namely `fromOption` and `liftF`, respectively. E.g.:

```scala mdoc:silent
val greetingFO: Future[Option[String]] = Future.successful(Some("Hello"))

val firstnameF: Future[String] = Future.successful("Jane")

val lastnameO: Option[String] = Some("Doe")

val ot: OptionT[Future, String] = for {
  g <- OptionT(greetingFO)
  f <- OptionT.liftF(firstnameF)
  l <- OptionT.fromOption[Future](lastnameO)
} yield s"$g $f $l"

val result: Future[Option[String]] = ot.value // Future(Some("Hello Jane Doe"))

```

## From `A` to `OptionT[F,A]`

If you have only an `A` and you wish to *lift* it into an `OptionT[F,A]` assuming you have an [`Applicative`](../typeclasses/applicative.md) instance for `F` you can use `some` which is an alias for `pure`. There also exists a `none` method which can be used to create an `OptionT[F,A]`, where the `Option` wrapped `A` type is actually a `None`:

```scala mdoc:silent
val greet: OptionT[Future,String] = OptionT.pure("Hola!")

val greetAlt: OptionT[Future,String] = OptionT.some("Hi!")

val failedGreet: OptionT[Future,String] = OptionT.none

```


## Beyond map

Sometimes the operation you want to perform on an `Future[Option[String]]` might not be as simple as just wrapping the `Option` method in a `Future.map` call. For example, what if we want to greet the customer with their custom greeting if it exists but otherwise fall back to a default `Future[String]` greeting? Without `OptionT`, this implementation might look like:

```scala mdoc:silent
val defaultGreeting: Future[String] = Future.successful("hello, there")

val greeting: Future[String] = customGreeting.flatMap(custom =>
  custom.map(Future.successful).getOrElse(defaultGreeting))
```

We can't quite turn to the `getOrElse` method on `OptionT`, because it takes a `default` value of type `A` instead of `Future[A]`. However, the `getOrElseF` method is exactly what we want:

```scala mdoc:silent:nest
val greeting: Future[String] = customGreetingT.getOrElseF(defaultGreeting)
```

## Getting to the underlying instance

If you want to get the `F[Option[A]]` value (in this case `Future[Option[String]]`) out of an `OptionT` instance, you can simply call  `value`:

```scala mdoc:silent:nest
val customGreeting: Future[Option[String]] = customGreetingT.value
```

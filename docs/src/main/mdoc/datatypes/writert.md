---
layout: docs
title:  "WriterT"
section: "data"
source: "core/src/main/scala/cats/data/WriterT.scala"
scaladoc: "#cats.data.WriterT"
---
# WriterT

`WriterT[F[_], L, V]` is a type wrapper on an `F[(L,
V)]`. Speaking technically, it is a monad transformer for [`Writer`](https://typelevel.org/cats/datatypes/writer.html),
but you don't need to know what that means for it to be
useful.

## Composition

`WriterT` can be more convenient to work with than using
`F[Writer[L, V]]` directly, this because it exposes operations that allow
you to work with the values of the inner [`Writer`](https://typelevel.org/cats/datatypes/writer.html) (`L` and
`V`) abstracting both the `F` and [`Writer`](https://typelevel.org/cats/datatypes/writer.html).

For example, `map` allow you to transform the inner `V` value, getting
back a `WriterT` that wraps around it.

```scala mdoc:nest
import cats.data.{WriterT, Writer}

WriterT[Option, String, Int](Some(("value", 10))).map(x => x * x)
```

Plus, when composing multiple `WriterT` computations, those will be
composed following the same behaviour of a
[`Writer`](https://typelevel.org/cats/datatypes/writer.html) and the
generic `F`.  Let's see two examples with `Option` and [`Either`](https://typelevel.org/cats/datatypes/either.html): if
one of the computations has a `None` or a `Left`, the whole
computation will return a `None` or a `Left` since the way the two
types compose typically behaves that way. Moreover, when the
computation succeed, the logging side of the
[`Writer`](https://typelevel.org/cats/datatypes/writer.html)s will be
combined.

```scala mdoc:silent
val optionWriterT1 : WriterT[Option, String, Int] = WriterT(Some(("writerT value 1", 123)))
val optionWriterT2 : WriterT[Option, String, Int] = WriterT(Some(("writerT value 1", 123)))
val optionWriterT3 : WriterT[Option, String, Int] = WriterT.valueT(None)


val eitherWriterT1 : WriterT[Either[String, *], String, Int] = WriterT(Right(("writerT value 1", 123)))
val eitherWriterT2 : WriterT[Either[String, *], String, Int] = WriterT(Right(("writerT value 1", 123)))
val eitherWriterT3 : WriterT[Either[String, *], String, Int] = WriterT.valueT(Left("error!!!"))
```

```scala mdoc
// This returns a Some since both are Some
for {
    v1 <- optionWriterT1
    v2 <- optionWriterT2
} yield v1 + v2

// This returns a None since one is a None
for {
    v1 <- optionWriterT1
    v2 <- optionWriterT2
    v3 <- optionWriterT3
} yield v1 + v2 + v3

// This returns a Right since both are Right
for {
    v1 <- eitherWriterT1
    v2 <- eitherWriterT2
} yield v1 + v2

// This returns a Left since one is a Left
for {
    v1 <- eitherWriterT1
    v2 <- eitherWriterT2
    v3 <- eitherWriterT3
} yield v1 + v2 + v3
```

Just for completeness, we can have a look at the same example, but
with
[`Validated`](https://typelevel.org/cats/datatypes/validated.html)
since it as a slightly different behaviour then
[`Either`](https://typelevel.org/cats/datatypes/either.html). Instead
of short-circuiting when the first error is encountered,
[`Validated`](https://typelevel.org/cats/datatypes/validated.html)
will accumulate all the errors. In the following example, you can see
how this behaviour is respected when
[`Validated`](https://typelevel.org/cats/datatypes/validated.html) is
wrapped as the `F` type of a `WriterT`. In addition, notice
how `flatMap` and for comprehension can't be used in this case, since
[`Validated`](https://typelevel.org/cats/datatypes/validated.html)
only extends [`Applicative`](https://typelevel.org/cats/typeclasses/applicative.html), but not [`Monad`](https://typelevel.org/cats/typeclasses/monad.html).

```scala mdoc:silent
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._

val validatedWriterT1 : WriterT[Validated[String, *], String, Int] = WriterT(Valid(("writerT value 1", 123)))
val validatedWriterT2 : WriterT[Validated[String, *], String, Int] = WriterT(Valid(("writerT value 1", 123)))
val validatedWriterT3 : WriterT[Validated[String, *], String, Int] =
WriterT(Invalid("error 1!!!") : Validated[String, (String, Int)])
val validatedWriterT4 : WriterT[Validated[String, *], String, Int] = WriterT(Invalid("error 2!!!"): Validated[String, (String, Int)])
```

```scala mdoc
// THIS returns a Right since both are Right
(validatedWriterT1,
validatedWriterT2
).mapN((v1, v2) => v1 + v2)

// This returns a Left since one is a Left
(validatedWriterT1,
 validatedWriterT2,
 validatedWriterT3,
 validatedWriterT4
).mapN((v1, v2, v3, v4) => v1 + v2 + v3 + v4)
```

## Construct a WriterT

A `WriterT` can be built starting from multiple values. Here is the
list of the main available constructors with a brief explanation and
an example.

`WriterT[F[_], L, V](run: F[(L, V)])`
:  This is the constructor of the datatype itself. It just builds the
type starting from the full wrapped value.

```scala mdoc:nest
// Here we use Option as our F[_]
  val value : Option[(String, Int)] = Some(("value", 123))
  WriterT(value)
```

`liftF[F[_], L, V](fv: F[V])(implicit monoidL: Monoid[L], F: Applicative[F]): WriterT[F, L, V]`
:  This function allows you to build the datatype starting from the
value `V` wrapped into an `F`. Notice how it requires:
* [`Monoid[L]`](https://typelevel.org/cats/typeclasses/monoid.html), since it uses the `empty` value from the typeclass.
to fill the `L` value not specified in the input.
* [`Applicative[F]`](https://typelevel.org/cats/typeclasses/applicative.html) to modify the inner value.

```scala mdoc:nest
  import cats.instances.option._

  val value : Option[Int] = Some(123)

  WriterT.liftF[Option, String, Int](value)
```

`put[F[_], L, V](v: V)(l: L)(implicit applicativeF: Applicative[F]): WriterT[F, L, V]`
:  As soon as there is an [`Applicative`](https://typelevel.org/cats/typeclasses/applicative.html) instance of `F`, this function
creates the datatype starting from the inner [`Writer`](https://typelevel.org/cats/datatypes/writer.html)'s values.

```scala mdoc:nest
  WriterT.put[Option, String, Int](123)("initial value")
```

`putT[F[_], L, V](vf: F[V])(l: L)(implicit functorF: Functor[F]): WriterT[F, L, V]`
:  Exactly as `put`, but the value `V` is already wrapped into `F`

```scala mdoc:nest
  WriterT.putT[Option, String, Int](Some(123))("initial value")
```

## Operations

Into the [Writer
definition](https://typelevel.org/cats/datatypes/writer.html#definition)
section, we showed how it is actually a `WriterT`. Therefore, all the
operations described into [Writer
operations](https://typelevel.org/cats/datatypes/writer.html#operations)
are valid for `WriterT` as well.

The only aspect we want to remark here is the following sentence from
[`Writer`](https://typelevel.org/cats/datatypes/writer.html)'s page:

> Most of the `WriterT` functions require a `Functor[F]` or
> `Monad[F]` instance. However, Cats provides all the necessary
> instances for the `Id` type, therefore we don't have to worry about
> them.

In the case of `WriterT`, the user needs to ensure the required
instances are present. Cats still provide a lot of default instances,
so there's a high chance you could find what you are searching for
with the right `import`.

## Example

As en example, we can consider a simple naive console application that
pings multiple HTTP well-known services and collect the time
spent in each call, returning the total time of the whole execution at
the end. We will simulate the calls by successful `Future` values.

Using `WriterT` we can log each step of our application,
compute, the time and work within the `Future` effect.

```scala mdoc:silent
import cats.data.WriterT

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

// Mocked HTTP calls
def pingService1() : Future[Int] = Future.successful(100)
def pingService2() : Future[Int] = Future.successful(200)
def pingService3() : Future[Int] = Future.successful(50)
def pingService4() : Future[Int] = Future.successful(75)

def pingToWriterT(ping: Future[Int], serviceName: String) : WriterT[Future, String, Int] =
    WriterT.valueT[Future, String, Int](ping)
        .tell(s"ping to $serviceName ")
        .flatMap(pingTime => WriterT.put(pingTime)(s"took $pingTime \n"))

val resultWriterT: WriterT[Future, String, Int] = for {
    ping1 <- pingToWriterT(pingService1(), "service #1")
    ping2 <- pingToWriterT(pingService2(), "service #2")
    ping3 <- pingToWriterT(pingService3(), "service #3")
    ping4 <- pingToWriterT(pingService4(), "service #4")
  } yield ping1 + ping2 + ping3 + ping4

val resultFuture: Future[String] = resultWriterT.run.map {
    case (log: String, totalTime: Int) => s"$log> Total time: $totalTime"
}
```

And the final result as expected:

```scala mdoc
Await.result(resultFuture, Duration.Inf)
```

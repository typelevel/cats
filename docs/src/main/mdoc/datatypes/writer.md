---
layout: docs
title:  "Writer"
section: "data"
source: "core/src/main/scala/cats/data/package.scala"
scaladoc: "#cats.data.Writer"
---
# Writer

The `Writer[L, A]` monad represents a computation that produce a tuple
containing a value of type `L` and one of type `A`. Usually, the value
`L` represents a description of the computation, a Log for
instance. Meanwhile, the value `A` is the actual output of the
computation.

The main features the `Writer` provides are:
- The flexibility regarding Log management. It can be modified
in multiple ways. See the [Operations section](#operations)
- When two functions are composed together, eg using `flatMap`, the
  logs of both functions will be combined using a `Semigroup`.

## Operations

The `Writer` datatype provides a set of functions that are quite
identical to the ones from the monad typeclass. In fact, they share
the same name and the same signature, but for the requirement of a
`Semigroup[L]` that allows the log merging.

`map` has an effect only on the value, keeping the log untouched.
```scala mdoc
import cats.data.Writer
import cats.instances._

val mapExample = Writer("map Example", 1).map(_ + 1)

mapExample.run
```

`ap` allows applying a function, wrapped into a Writer. It works
exactly like the `Applicative` as expected, but notice how the logs
are combined using the `Semigroup[String]`. Plus, here we show `run`
that just unwrap the datatype, returning its content.

```scala mdoc

val apExampleValue = Writer("ap value", 10)
val apExampleFunc = Writer("ap function ", (i: Int) => i % 7)

apExampleValue.ap(apExampleFunc).run
```

Same thing for `flatMap`

```scala mdoc

val flatMapExample1 = Writer("flatmap value", 5)
val flatMapExample2 = (x: Int) => Writer("flatmap function ", x * x)

flatMapExample1.flatMap(flatMapExample2).run

// We can use the for comprehension as well
val flatMapForResult = for {
    value <- Writer("flatmap value", 5)
    result <- flatMapExample2(value)
} yield result

flatMapForResult.run
```

Apart from those, `Writer` comes with some specific function to manage
the log side of the computation:

`tell`
:  Append a value to the log side. It requires a `Semigroup[L]`.

`swap`
:  Exchange the two values of the `Writer`.

`reset`
:  Delete the log side. It requires a `Monoid[L]`

`value`
:  Returns only the value of the `Writer`

`listen`
:  Transform the value of the `Writer` to a tuple containing the
   current value and the current log.

```scala mdoc

val tellExample = Writer("tell example", 1).tell("log append")

tellExample.run

val swapExample = Writer("new value", "new log").swap

swapExample.run

val resetExample = Writer("long log to discard", 42).reset

resetExample.run

val valueExample = Writer("some log", 55).value

valueExample

val listenExample = Writer("listen log", 10).listen

listenExample.run

```

### Definition

If we go looking at how `Writer` is actually defined, we will see
it is just a type alias:

```scala mdoc:silent
import cats.data.WriterT
import cats.Id
// cats/data/package.scala
type Writer[L, V] = WriterT[Id, L, V]
```

So, all the [Operations](#operations) defined in the previous section
are actually coming from the [WriterT
datatype](https://typelevel.org/cats/datatypes/writert.html)

Most of the `WriterT` functions require a `Functor[F]` or
`Monad[F]` instance. However, Cats provides all the necessary
instances for the `Id` type, therefore we don't have to worry about
them.

## Example

The example showed in here is taken from the [Rosetta Code
site](https://rosettacode.org/wiki/Monads/Writer_monad). It simply
apply a bunch of path operations, logging each of them with a log

```scala mdoc:silent:reset

import cats.data.Writer
import scala.math.sqrt

val writer1: Writer[String, Double] = Writer.value(5.0).tell("Initial value ")
val writer2: Writer[String, Double => Double] = Writer("sqrt ", (i: Double) => sqrt(i))
val writer3: Double => Writer[String, Double] = (x: Double) => Writer("add 1 ", x + 1)
val writer4: Writer[String, Double => Double] = Writer("divided by 2 ", (x: Double) => x / 2)

val writer5: Writer[String, Double => Double] = Writer[String, Double => Double](writer3(0).written,(x: Double) => writer3(x).value)
```

```scala mdoc
// Pay attention on the ordering of the logs

writer1
  .ap(writer2)
  .flatMap(writer3(_))
  .ap(writer4)
  .map(_.toString)
  .run

import cats.syntax.compose._

(for {
    initialValue <- writer1
    sqrt <- writer2
    addOne <- writer5
    divideBy2 <- writer4
    } yield (sqrt >>> addOne >>> divideBy2)(initialValue)
).run
```

If you are interested in logging solutions, we recommend the library [log4cats](https://christopherdavenport.github.io/log4cats/)

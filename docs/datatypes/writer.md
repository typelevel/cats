# Writer

API Documentation: @:api(cats.data.WriterT)

The `Writer[L, A]` datatype represents a computation that produces a
tuple containing a value of type `L` and one of type `A`. Usually, the
value `L` represents a description of the computation. A typical
example of an `L` value could be a logging `String` and that's why
from now on we will refer to it as the _Logging side_ of the
datatype. Meanwhile, the value `A` is the actual output of the
computation.

The main features that `Writer` provides are:

The flexibility regarding Log value management. It can be modified in
multiple ways. See the [Operations section](#operations)

When two functions are composed together, e.g. using `flatMap`, the logs
  of both functions will be combined using an implicit
  [Semigroup](../typeclasses/semigroup.md).

## Operations

The `Writer` datatype provides a set of functions that are similar to
the ones from the
[Monad](../typeclasses/monad.md)
typeclass. In fact, they share the same name and the same signature,
but have an additional requirement of a
[`Semigroup[L]`](../typeclasses/semigroup.md)
that allows the log merging.

`map` effects only the value, keeping the log side untouched. Plus, here we show `run`
that just unwrap the datatype, returning its content.

```scala mdoc
import cats.data.Writer
import cats.instances._

val mapExample = Writer("map Example", 1).map(_ + 1)

mapExample.run
```

`ap` allows applying a function, wrapped into a Writer. It works
exactly like the `Applicative` as expected, but notice how the logs
are combined using the [`Semigroup[String]`](../typeclasses/semigroup.md).

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

Apart from those, `Writer` comes with some specific functions to manage
the log side of the computation:

`tell`
:  Append a value to the log side. It requires a [`Semigroup[L]`](../typeclasses/semigroup.md).

`swap`
:  Exchange the two values of the `Writer`.

`reset`
:  Delete the log side. It requires a [`Monoid[L]`](../typeclasses/monoid.md) since it uses the `empty` value of the monoid.

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
datatype](writert.md)

Most of the [`WriterT`](writert.md) functions require a
[`Functor[F]`](../typeclasses/functor.md) or
[`Monad[F]`](../typeclasses/monad.md)
instance. However, Cats provides all the necessary instances for the
[`Id`](id.md) type, therefore
we don't have to worry about them.

## Example

The example showed in here is taken from the [Rosetta Code
site](https://rosettacode.org/wiki/Monads/Writer_monad). It simply
applies a bunch of Math operations, logging each one of them.

```scala mdoc:silent:reset
import cats.data.Writer
import scala.math.sqrt

val writer1: Writer[String, Double] = Writer.value[String, Double](5.0).tell("Initial value ")
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

If you are interested in logging solutions, we recommend the library [log4cats](https://typelevel.org/log4cats/)

---
layout: docs
title:  "Writer"
section: "data"
source: "core/src/main/scala/cats/data/package.scala"
scaladoc: "#cats.data.Writer"
---
# Writer

The `Writer[L, A]` monad represents a computation which produce a
tuple of a value of type `L` and one of type `A`, when
executed. Usually, the value `L` represent a descriptor of the
computation, a Log for instance. Meanwhile, the value
`A` is the actual output of the computation.

The main features the `Writer` provides are:
- The flexibility regarding the Log management. It can be modified
in multiple ways. See the [Operations section](#Operations)
- When two functions are composed together, eg using `flatMap`, the
  logs of both functions will be combined using a `Semigroup`.

## Operations

The `Writer` datatype provides a set of functions that are quite
identical to the ones from the monad typeclass. In fact, they share
the same name and the same signature, but for the requirement of a
`Semigroup[L]` that allow the log merging.

`map` has effect only on the value, keeping the log untouched.
```scala mdoc
import cats.data.Writer
import cats.instances._

val mapExample = Writer("map Example", 1).map(_ + 1)

mapExample.run
```

`ap` allow to apply a function, wrapped into a Writer. It works
exactly like the `Applicative`, but notice how the logs are combined
using the `Semigroup[String]`. Plus, here we show `run` that just
unwrap the datatype, returning its content.

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

### Definition

TODO: talk about the fact that Writer is a WriterT with an Id as F and
therefore the Functor/applicative ... are automatically satisfied.

## Example

TODO: https://rosettacode.org/wiki/Monads/Writer_monad#Haskell

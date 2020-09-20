---
layout: docs
title:  "WriterT"
section: "data"
source: "core/src/main/scala/cats/data/WriterT.scala"
scaladoc: "#cats.data.WriterT"
---
# WriterT

`WriterT[F[_], L, V]` is a light wrapper on an `F[(L,
V)]`. Speaking technically, it is a monad transformer for `Writer`,
but you don't need to know what that means for it to be
useful. `WriterT` can be more convenient to work with than using
`F[Writer[L, V]]` directly because it exposes operations that allow
you to work direcly with the values of the inner `Writer` (`L` and
`V`).

## Construct a WriterT

A `WriterT` can be built starting from multiple values. Here is the
list of available constructors with a brief explanation and an
example.

`WriterT[F[_], L, V](run: F[(L, V)])`
:  This is the constructor of the datatype itself. It just builds the
   type starting from the full wrapped value.

```scala mdoc

  import cats.data.{WriterT, Writer}

// Here we use Option as our F[_]
  val value : Option[(String, Int)] = Some(("value", 123))
  WriterT(value)
```

` liftF[F[_], L, V](fv: F[V])(implicit monoidL: Monoid[L], F: Applicative[F]): WriterT[F, L, V]`
:  This function allow you to build the datatype starting from the
   value `V` wrapped into an `F`. Notice how it requires:
   * `Monoid[L]`, since it uses the `empty` value from the typeclass
   to fill the `L` value not specified in input.
   * `Applicative[F]` to modify the inner value.

```scala mdoc:nest
  import cats.instances.option._

  val value : Option[Int] = Some(123)

  WriterT.liftF[Option, String, Int](value)
```

TODO: liftK, put, putT,

## Operations

TODO: reference to `Writer` operations section and the differences
with writerT

## Example

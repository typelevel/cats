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

`liftF[F[_], L, V](fv: F[V])(implicit monoidL: Monoid[L], F: Applicative[F]): WriterT[F, L, V]`
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

`put[F[_], L, V](v: V)(l: L)(implicit applicativeF: Applicative[F]): WriterT[F, L, V]`
:  As soon as there is an `Applicative` instance of `F`, this function
   creates the datatype starting from the inner `Writer`'s values.

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
we showed how it is actually a `WriterT`.
Therefore, all the operations described into [Writer
operations](https://typelevel.org/cats/datatypes/writer.html#operations)
are valid for `WriterT` as well.

The only aspect we want to remark is the follwoing sentence from
`Writer`'s page:

> Most of the the `WriterT` functions require a `Functor[F]` or
> `Monad[F]` instance. However, Cats provides all the necessary
> instances for the `Id` type, therefore we don't have to worry about
> them.

In the case of the `WriterT` the user needs to ensure the required
instances are present. Cats still provides a lot of default instances,
so there's an high chance you could get them for free.

## Example

---
layout: default
title:  "Applicative"
section: "typeclasses"
source: "core/src/main/scala/cats/Applicative.scala"
scaladoc: "#cats.Applicative"
---
# Applicative

`Applicative` extends [`Apply`](apply.html) by adding a single method,
`pure`:

```scala
def pure[A](x: A): F[A]
````

This method takes any value and returns the value in the context of
the functor. For many familiar functors, how to do this is
obvious. For `Option`, the `pure` operation wraps the value in
`Some`. For `List`, the `pure` operation returns a single element
`List`:

```tut:book
import cats._
import cats.implicits._

Applicative[Option].pure(1)
Applicative[List].pure(1)
```

Like [`Functor`](functor.html) and [`Apply`](apply.html), `Applicative`
functors also compose naturally with each other. When
you compose one `Applicative` with another, the resulting `pure`
operation will lift the passed value into one context, and the result
into the other context:

```tut:book
import cats.data.Nested
val nested = Applicative[Nested[List, Option, ?]].pure(1)
val unwrapped = nested.value
```

## Applicative Functors & Monads

`Applicative` is a generalization of [`Monad`](monad.html), allowing expression
of effectful computations in a pure functional way.

`Applicative` is generally preferred to `Monad` when the structure of a
computation is fixed a priori. That makes it possible to perform certain
kinds of static analysis on applicative values.

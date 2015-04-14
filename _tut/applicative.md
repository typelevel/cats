---
layout: default
title:  "Applicative"
section: "typeclasses"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/Applicative.scala"
scaladoc: "#cats.Applicative"
---
# Applicative

Applicative functors are a simple extension of the [Apply
functor](apply.md) which adds a single method, `pure`:

```scala
    def pure[A](x: A): F[A]
````

This method takes any value and returns the value in the context of
the functor. For many familiar functors, how to do this is
obvious. For Option, the `pure` operation wraps the value in
`Some`. For `List`, the `pure` operation returns a single element
`List`:

```scala
scala> import cats._
import cats._

scala> import cats.std.all._
import cats.std.all._

scala> Applicative[Option].pure(1)
res0: Option[Int] = Some(1)

scala> Applicative[List].pure(1)
res1: List[Int] = List(1)
```

Like [Functor](functor.md) and [Apply](apply.md), Applicative
functors also composes naturally with other Applicative functors. When
you compose one Applicative with another, the resulting `pure`
operation will lift the passed value into one context, and the result
into the other context:

```scala
scala> (Applicative[List] compose Applicative[Option]).pure(1)
res2: List[Option[Int]] = List(Some(1))
```

## Applicative Functors & Monads

Applicative functors are a generalization of Monads thus allowing to express 
effectful computations into a pure functional way.

Applicative functors are generally preferred to monads when the structure 
of a computation is fixed a priori. That makes it possible to perform certain
kinds of static analysis on applicative values.

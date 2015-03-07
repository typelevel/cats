---
layout: default
title:  "Applicative"
section: "typeclasses"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/Applicative.scala"
scaladoc: "#cats.Applicative"
---
# Applicative

Applicative functors are a simple extension of the [Apply
functor](apply.html) which adds a single method, `pure`:

```scala
    def pure[A](x: A): F[A]
````

This method takes any value and returns the value in the context of
the functor. For many familiar functors, how to do this is
obvious. For Option, the `pure` operation wraps the value in
`Some`. For `List`, the `pure` operation returns a single element
`List`:

```tut
import cats._
import cats.std.all._

Applicative[Option].pure(1)
Applicative[List].pure(1)
```

Like [Functor](functor.html) and [Apply](apply.html), Applicative
functors also composes naturally with other Applicative functors. When
you compose one Applicative with another, the resulting `pure`
operation will lift the passed value into one context, and the result
into the other context:

```tut
(Applicative[List] compose Applicative[Option]).pure(1)
```

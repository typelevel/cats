---
layout: docs
title:  "Functor"
section: "typeclasses"
source: "core/src/main/scala/cats/Bifunctor.scala"
scaladoc: "#cats.Bifunctor"
---
# Bifunctor

`Bifunctor` takes two type parameters instead of one, and is a functor in both
of these parameters. It defines a function `bimap`, which allows for mapping over both
arguments at the same time. Its signature is as follows:

```scala
def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
```

## Either as a Bifunctor

Probably the most widely used Bifunctor instance is the Either data type.

Say you have a value that is either an error or a `ZonedDateTime` instance.
You also want to react to both possibilities - if there was a failure, you want to
convert it to your own `DomainError`, and if the result was a success, you want to
convert it to an UNIX timestamp.

```tut:silent
import cats._
import cats.implicits._
import java.time._

case class DomainError(message: String)

def dateTimeFromUser: Either[Throwable, ZonedDateTime] = 
  Right(ZonedDateTime.now())  // Example definition
```

```tut:book
dateTimeFromUser.bimap(
  error => DomainError(error.getMessage),
  dateTime => dateTime.toEpochSecond
)
```

`Bifunctor` also defines a convenience function called `leftMap`, which is defined as follows:

```scala
def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, identity)
```

There is no `rightMap` however - use `map` instead. The reasoning behind this is that in Cats, the instances of
`Bifunctor` are also mostly instances of `Functor`, as it is the case with `Either`.

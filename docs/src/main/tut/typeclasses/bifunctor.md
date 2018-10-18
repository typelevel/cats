---
layout: docs
title:  "Bifunctor"
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

## Tuple2 as a Bifunctor

Another very popular `Bifunctor` is that for the `Tuple2` data type, or `(A, B)` for types `A` and `B`.

Let's say we have a list of balances and want divide them by the number of months in the lifetime of the account holder.
A bit contrived, but we want an average contribution per month to the given account.
The list of balances is given as a list of numeric strings (except when they aren't), and the account lifetime is given in years.

```tut:book
val records: List[String] = List("4500", "7700", "9900", "21", "oops")
val lifetimes: List[Int] = List(3, 4, 2, 4, 3)
val withLifetime: List[(String, Int)] = records.zip(lifetimes)

def decodeInt(s: String): Either[Throwable, Int] = Either.catchNonFatal(s.toInt)

def fillErrorsAt0(e: Either[Throwable, Int]): Int = e.getOrElse(0)

def calculateContributionPerMonth(balance: Int, lifetime: Int) = balance / lifetime

val withDecodedBalance = withLifetime.map(_.leftMap(decodeInt))

val result: List[Int] =
  withDecodedBalance.map(
      _.bimap(
        fillErrorsAt0,
        years => 12 * years
      )
    ).map((calculateContributionPerMonth _).tupled)
```

As you can see, this instance makes it convenient to process two related pieces of data in independent ways, especially when there is no state relationship between the two until processing is complete.

Note that, just as with the bifunctor for `Either`, we do not have a `rightMap` function since the relevant instances of `Bifunctor` induce a `Functor` in the second argument, so we just use `map`.

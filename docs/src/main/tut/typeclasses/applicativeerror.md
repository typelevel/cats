---
layout: docs
title:  "Applicative"
section: "typeclasses"
source: "core/src/main/scala/cats/ApplicativeError.scala"
scaladoc: "#cats.ApplicativeError"
---
# ApplicativeError

## Description
`ApplicativeError` extends `Applicative` to provide handling for types
that represent the quality of an exception or an error, for example, `Either[E, A]`

## TypeClass Definition
`ApplicativeError` is defined by the following `trait`

```tut:book:silent
trait ApplicativeError[F[_], E] extends Applicative[F] {
  def raiseError[A](e: E): F[A]
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  def handleError[A](fa: F[A])(f: E => A): F[A]
  def attempt[A](fa: F[A]): F[Either[E, A]]
  //More functions elided
}
```

## Use Case

### `Either`
We can start with a less abstract way of performing a function. Here 
we will divide one number by another. 

```tut:book:silent
def process(x: Int, y: Int): Either[String, Int] = {
   if (y == 0) Left("divisor is zero")
   else {
      Right(x / y)
   }
}
```

While fine in the above approach, we can abstract the `Either` away
to support any other kind of "error" type without having to 
create multiple functions.

```tut:book:silent
def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] = {
   if (y == 0) ae.raiseError("divisor is error")
   else {
      ae.pure(x/y)
   }
}
```

In the above `ApplicativeError` is summoned to provide behavior that represents an
error where the end user based on type will get their appropriate response.

A couple of items `ApplicativeError` is an [`Applicative`](https://typelevel.org/cats/typeclasses/applicative.html) 
which means all `Applicative` functions are available for use. One such method is `pure`, which
will return the `F[_]` representation, where `F` could represent `Either`.  Another method
that you will see is `raiseError` which will generate the specific _error type_ depending
on what `F[_]` represents. If `F[_]` is an `Either`, then `ae.raiseError` will return `Left`. 
If `F[_]`represents a `Validation` then `ae.raiseError` will return `Invalid`.
 
If I indeed, want to use an `Either` as my error representation, I can do the following.

```tut:book:silent
type OnError[A] = Either[String, A]
val e: OnError[Int] = process(30, 10)
```

or simply via assignment

```tut:book:silent
val e: Either[String, Int] = process(30, 10)
```

### `Validated`

Given the same function `process`, we can call that function again but 
with a different return type. 

We can call this method, but we have a choice, since the `ApplicativeError` can 
support other "error" based types.  Here we will use `cats.data.Validated`
when calling `process`. Notice that `process` is the same as we defined above, no
changes are made. 

```tut:book:silent
import cats.implicits._
import cats.data.Validated

def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] = {
   if (y == 0) ae.raiseError("divisor is zero")
   else {
     ae.pure(x / y)
   }
}

type MyValidated[A] = Validated[String, A]
val e = process[MyValidated](30, 10)
```

We can inline the right projection type alias, `MyValidated`, doing the following:

```tut:book:silent
val e = process[({ type T[A] = Validated[String, A]})#T](30, 10)
```

Or we can use [TypeProjector](http://example.net/) to make this more refined and readable

```tut:book:silent
val e = process[Validated[String, *]](30, 10)
```

## It is an `Applicative` after all

As a Reminder, this is an [`Applicative`](https://typelevel.org/cats/typeclasses/applicative.html) so all the methods are available
to you to use in manipulating your values, `ap`, `mapN`, etc. In the following 
example, notice we are using `Applicative`'s `map2`, and of course, `pure` which
also is from `Applicative`

```tut:book:silent
import cats.implicits._
def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[_] = {
   if (y == 0) ae.raiseError("divisor is error")
      else {
        val fa = ae.pure(x)
        val fb = ae.pure(y)
        ae.map2(fa, fb)(_ + _)
      }
   }
}
```

## Handling Errors

`ApplicativeError` has methods that will handle what to do when `F[_]` represents an
error. In the following example, process with create an error representation if the divisor
is `0` or `1` with a message.  The result will be 
fed into the `present` method, where it will "make it right", by calling `handleError`.

```tut:book:silent
import cats.implicits._
def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] =
  if (y == 0) ae.raiseError("Bad Math")
  else if (y == 1) ae.raiseError("Waste of Time")
  else ae.pure(x / y)

def present[F[_]](f: F[Int])(implicit ae: ApplicativeError[F, String]): F[Int] = {
  ae.handleError(f) {
    case "Bad Math"      => -1
    case "Waste of Time" => -2
    case _               => -3
  }
}
```    

Running the following will result in `Right(-1)`

```tut:book:silent
present(process(3, 0))
```

`handleErrorWith` is nearly the same as `handleError` but 
instead of returning a value `A`, we will return `F[_]`. This could provide us 
the opportunity to make it very abstract and return a value from a `Monoid.empty`.

```tut:book:silent
import cats.implicits._
def process[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] =
  if (y == 0) ae.raiseError("Bad Math")```
  else if (y == 1) ae.raiseError("Waste of Time")
  else ae.pure(x / y)

def present[F[_], M[_], A](f: F[A])(implicit F: ApplicativeError[F, String], M:Monoid[A]): F[A] = {
  F.handleErrorWith(f)(_ => F.pure(M.empty))
}
```

Running the following will result in `Right(0)`

```tut:book:silent
present(process(3, 0))
```

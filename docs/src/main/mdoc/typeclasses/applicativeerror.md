---
layout: docs
title:  "Applicative Error"
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

```
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

```scala mdoc:silent
def attemptDivide(x: Int, y: Int): Either[String, Int] = {
   if (y == 0) Left("divisor is zero")
   else {
      Right(x / y)
   }
}
```

While fine in the above approach, we can abstract the `Either` away
to support any other kind of "error" type without having to 
create multiple functions with different "container" types.

```scala mdoc:silent
import cats._
import cats.implicits._

def attemptDivideApplicativeError[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] = {
   if (y == 0) ae.raiseError("divisor is error")
   else {
      ae.pure(x/y)
   }
}
```

The above method summons `ApplicativeError` to provide behavior representing
an error where the end-user, based on type, will get their 
appropriate response.

`ApplicativeError` is an `Applicative`, which means all `Applicative` 
functions are available for use. One such method is `pure`, which will 
return the `F[_]` representation, where `F` could represent `Either`. 

Another method that you will see is `raiseError`, which will generate 
the specific error type depending on what `F[_]` represents. 
If `F[_]` is an `Either`, then `ae.raiseError` will return `Left`. 
If `F[_]` represents a `Validation`, then `ae.raiseError` will return `Invalid`.
For example, if we want to use an `Either` as our error representation,
we can do the following:

```scala mdoc:silent
type OnError[A] = Either[String, A]
val e: OnError[Int] = attemptDivideApplicativeError(30, 10)
```

or simply via assignment

```scala mdoc:silent
val f: Either[String, Int] = attemptDivideApplicativeError(30, 10)
```

### `Validated`

Given the same function `attemptDivideApplicativeError`, we can call that
function again but with a different return type, since the `ApplicativeError`
can support other "error" based types.  Here we will use `cats.data.Validated`
when calling `attemptDivideApplicativeError`. Notice that 
`attemptDivideApplicativeError` is the same as we defined above, 
so we make no other changes.

```scala mdoc:silent
import cats.implicits._
import cats.data.Validated

type MyValidated[A] = Validated[String, A]
val g = attemptDivideApplicativeError[MyValidated](30, 10)
```

We can inline the right projection type alias, `MyValidated`, doing the following:

```scala mdoc:silent
val h = attemptDivideApplicativeError[({ type T[A] = Validated[String, A]})#T](30, 10)
```

Or we can use [KindProjector](https://github.com/typelevel/kind-projector) 
to make this more refined and readable

```scala mdoc:silent
val j = attemptDivideApplicativeError[Validated[String, *]](30, 10)
```

## It is an `Applicative` after all

As a Reminder, this is an [`Applicative`](/cats/typeclasses/applicative.html) 
so all the methods of `Applicative` are available to you to use in 
manipulating your values, `ap`, `mapN`, etc. In the following example, notice
we are using `Applicative`'s `map2`, and of course, `pure` which also is a 
form of `Applicative`.

```scala mdoc:silent
import cats.implicits._
def attemptDivideApplicativeErrorWithMap2[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[_] = {
   if (y == 0) ae.raiseError("divisor is error")
   else {
     val fa = ae.pure(x)
     val fb = ae.pure(y)
     ae.map2(fa, fb)(_ + _)
   }
}
```

## Handling Errors

`ApplicativeError` has methods to handle what to do when `F[_]` represents an error. 
In the following example, `attemptDivideApplicativeErrorAbove2`
creates an error representation if the divisor
is `0` or `1` with the message "Bad Math" or "Waste of Time".  
We will feed the result from `attemptDivideApplicativeErrorAbove2` 
into the `handler` method, where this method will pattern match on the message
and provide an alternative outcome.

```scala mdoc:silent
import cats.implicits._
def attemptDivideApplicativeErrorAbove2[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] =
  if (y == 0) ae.raiseError("Bad Math")
  else if (y == 1) ae.raiseError("Waste of Time")
  else ae.pure(x / y)

def handler[F[_]](f: F[Int])(implicit ae: ApplicativeError[F, String]): F[Int] = {
  ae.handleError(f) {
    case "Bad Math"      => -1
    case "Waste of Time" => -2
    case _               => -3
  }
}
```    

Running the following will result in `Right(-1)`

```scala mdoc:silent
handler(attemptDivideApplicativeErrorAbove2(3, 0))
```

`handleErrorWith` is nearly the same as `handleError` but 
instead of returning a value `A`, we will return `F[_]`. This could provide us 
the opportunity to make it very abstract and return a value from a `Monoid.empty`.

```scala mdoc:silent
def handlerErrorWith[F[_], M[_], A](f: F[A])(implicit F: ApplicativeError[F, String], M:Monoid[A]): F[A] = {
  F.handleErrorWith(f)(_ => F.pure(M.empty))
}
```

Running the following will result in `Right(0)`

```scala mdoc:silent
handlerErrorWith(attemptDivideApplicativeErrorAbove2(3, 0))
```

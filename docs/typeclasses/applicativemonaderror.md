# ApplicativeError and MonadError

## Applicative Error

### Description
`ApplicativeError` extends `Applicative` to provide handling for types
that represent the quality of an exception or an error, for example, `Either[E, A]`

### TypeClass Definition
`ApplicativeError` is defined by the following `trait`

```scala
trait ApplicativeError[F[_], E] extends Applicative[F] {
  def raiseError[A](e: E): F[A]
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  def handleError[A](fa: F[A])(f: E => A): F[A]
  def attempt[A](fa: F[A]): F[Either[E, A]]
  //More functions elided
}
```

### Use Case

#### `Either`
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

#### `Validated`

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

### It is an `Applicative` after all

As a Reminder, this is an [`Applicative`](applicative.md) 
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
     ae.map2(fa, fb)(_ / _)
   }
}
```

### Handling Errors

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

## MonadError

### Description

Since a `Monad` extends an `Applicative`, there is naturally a `MonadError` that 
will extend the functionality of the `ApplicativeError` to provide `flatMap`
composition.

### TypeClass Definition

The Definition for `MonadError` extends `Monad` which provides the
methods, `flatMap`, `whileM_`.  `MonadError` also provides error 
handling methods like `ensure`, `ensureOr`, `adaptError`, `rethrow`.

```
trait MonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F] {
  def ensure[A](fa: F[A])(error: => E)(predicate: A => Boolean): F[A] 
  def ensureOr[A](fa: F[A])(error: A => E)(predicate: A => Boolean): F[A]
  def adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A]
  def rethrow[A, EE <: E](fa: F[Either[EE, A]]): F[A]
}
```

### Use Case

Given a method that accepts a tuple of coordinates, it finds the closest city. For this example
we will hard-code "Minneapolis, MN," but you can imagine for the sake of 
In this example, you would either consult a database or a web service.

```scala mdoc:silent
def getCityClosestToCoordinate[F[_]](x: (Int, Int))(implicit ae: ApplicativeError[F, String]): F[String] = {
  ae.pure("Minneapolis, MN")
}
```
Next, let's follow up with another method, `getTemperatureByCity`, that given a 
city, possibly a city that was just discovered by its coordinates, we get 
the temperature for that city. Here, for the sake of demonstration, 
we are hardcoding a temperature of 78°F.

```scala mdoc:silent
def getTemperatureByCity[F[_]](city: String)(implicit ae: ApplicativeError[F, String]): F[Int] = {
  ae.pure(78)
}
```

With the methods that we will compose in place let's create a method that will
compose the above methods using a for comprehension which
interprets to a `flatMap`-`map` combination.  

`getTemperatureFromByCoordinates` parameterized type 
`[F[_]:MonadError[*[_], String]` injects `F[_]` into `MonadError[*[_], String]`
thus if the "error type" you wish to use is `Either[String, *]`, the `Either`
would be placed in the hole of `MonadError`, in this case, 
`MonadError[Either[String, *], String]`

`getTemperatureFromByCoordinates` accepts a `Tuple2` of `Int` and `Int`, and we
return `F` which represents our `MonadError` which can be a type like `Either` or
`Validated`. In the method, since either `getCityClosestToCoordinate` and
`getTemperatureByCity` both return potential error types and they are monadic we can
compose them with a for comprehension.

```scala mdoc:silent
def getTemperatureByCoordinates[F[_]: MonadError[*[_], String]](x: (Int, Int)): F[Int] = {
  for { c <- getCityClosestToCoordinate[F](x)
        t <- getTemperatureByCity[F](c) } yield t
}
``` 

Invoking `getTemperatureByCoordinates` we can call it with the following sample, 
which will return `78`. 

NOTE: infix `->` creates a `Tuple2`. `1 -> "Bob"` is the same as `(1, "Bob")`

```scala mdoc:silent
type MyEither[A] = Either[String, A]
getTemperatureByCoordinates[MyEither](44 -> 93)
```

With TypeLevel Cats, how you structure your methods is up to you, if you wanted to
create `getTemperatureByCoordinates` without a Scala 
[context bound](https://docs.scala-lang.org/tutorials/FAQ/context-bounds.html) for `MonadError`,
but create an `implicit` parameter for your `MonadError` you can have access to some 
additional methods.

In the following example, we create an `implicit` `MonadError` parameter
and call it `me`. Using the `me` reference, we can call any one of its 
specialized methods, like `raiseError`, to raise an error representation
when things go wrong.

```scala mdoc:silent
def getTemperatureFromByCoordinatesAlternate[F[_]](x: (Int, Int))(implicit me: MonadError[F, String]): F[Int] = {
  if (x._1 < 0 || x._2 < 0) me.raiseError("Invalid Coordinates")
  else for { c <- getCityClosestToCoordinate[F](x)
        t <- getTemperatureByCity[F](c) } yield t
}
```

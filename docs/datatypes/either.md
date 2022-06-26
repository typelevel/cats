# Either

In day-to-day programming, it is fairly common to find ourselves writing functions that
can fail. For instance, querying a service may result in a connection issue, or some
unexpected JSON response.

To communicate these errors it has become common practice to throw exceptions. However,
exceptions are not tracked in any way, shape, or form by the Scala compiler. To see
what kind of exceptions (if any) a function may throw, we have to dig through the source code.
Then to handle these exceptions, we have to make sure we catch them at the call site.
This all becomes even more unwieldy when we try to compose exception-throwing procedures.

```scala
val throwsSomeStuff: Int => Double = ???

val throwsOtherThings: Double => String = ???

val moreThrowing: String => List[Char] = ???

val magic = throwsSomeStuff.andThen(throwsOtherThings).andThen(moreThrowing)
```

Assume we happily throw exceptions in our code. Looking at the types, any of those functions
can throw any number of exceptions, we don't know. When we compose, exceptions from any of
the constituent functions can be thrown. Moreover, they may throw the same kind of exception
(e.g. `IllegalArgumentException`) and thus it gets tricky tracking exactly where that
exception came from.

How then do we communicate an error? By making it explicit in the data type we return.

## Either

### Lifting Values into an Either

`asRight` and `asLeft` are provided by Cats to lift a value into an Either. The advantage of these operations over the `Right` and `Left` constructors provided in the standard library is that the lifted value can help guide type inference.

```scala mdoc
import cats.syntax.all._

val e1 = 5.asRight[String]
val e2 = "error".asLeft[Int]
```

### `Either` vs `Validated`

In general, `Validated` is used to accumulate errors, while `Either` is used to short-circuit a computation upon the first error. For more information, see the `Validated` vs `Either` section of the [`Validated` documentation](validated.md).

### Syntax

In Scala 2.10.x and 2.11.x, `Either` is unbiased. That is, usual combinators like `flatMap` and `map`
are missing from it. Instead, you call `.right` or `.left` to get a `RightProjection` or
`LeftProjection` (respectively) which does have the combinators. The direction of the projection indicates the direction
of bias. For instance, calling `map` on a `RightProjection` acts on the `Right` of an `Either`.

```scala mdoc:reset
val e1: Either[String, Int] = Right(5)
e1.right.map(_ + 1)

val e2: Either[String, Int] = Left("hello")
e2.right.map(_ + 1)
```

Note the return types are themselves back to `Either`, so if we want to make more calls to
`flatMap` or `map` then we again must call `right` or `left`.

However, the convention is almost always to right-bias `Either`. Indeed in Scala 2.12.x `Either` is
[right-biased](https://github.com/scala/scala/pull/5135) by default.

More often than not we want to just bias towards one side and call it a day - by convention,
the right side is most often chosen. In Scala 2.12.x this convention
[is implemented](https://github.com/scala/scala/pull/5135)
in the standard library. Since Cats builds on 2.10.x and 2.11.x, the gaps have been filled via syntax
enrichments available under `cats.syntax.either._` or `cats.implicits._`.

```scala mdoc
import cats.implicits._

val right: Either[String, Int] = Right(5)
right.map(_ + 1)

val left: Either[String, Int] = Left("hello")
left.map(_ + 1)
```

For the rest of this tutorial we will assume the syntax enrichment is in scope giving us right-biased `Either`
and a bunch of other useful combinators (both on `Either` and the companion object).

Because `Either` is right-biased, it is possible to define a `Monad` instance for it.
Since we only ever want the computation to continue in the case of `Right`, we fix the left type parameter
and leave the right one free.

*Note*: the example below assumes usage of the [kind-projector compiler plugin](https://github.com/typelevel/kind-projector) and will not compile if it is not being used in a project.

```scala mdoc:silent
import cats.Monad

implicit def eitherMonad[Err]: Monad[Either[Err, *]] =
  new Monad[Either[Err, *]] {
    def flatMap[A, B](fa: Either[Err, A])(f: A => Either[Err, B]): Either[Err, B] =
      fa.flatMap(f)

    def pure[A](x: A): Either[Err, A] = Either.right(x)

    @annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => Either[Err, Either[A, B]]): Either[Err, B] =
      f(a) match {
        case Right(Right(b)) => Either.right(b)
        case Right(Left(a)) => tailRecM(a)(f)
        case l@Left(_) => l.rightCast[B] // Cast the right type parameter to avoid allocation
      }
  }
```

### Example usage: Round 1
As a running example, we will have a series of functions that will parse a string into an integer,
take the reciprocal, and then turn the reciprocal into a string.

In exception-throwing code, we would have something like this:

```scala mdoc:silent
object ExceptionStyle {
  def parse(s: String): Int =
    if (s.matches("-?[0-9]+")) s.toInt
    else throw new NumberFormatException(s"${s} is not a valid integer.")

  def reciprocal(i: Int): Double =
    if (i == 0) throw new IllegalArgumentException("Cannot take reciprocal of 0.")
    else 1.0 / i

  def stringify(d: Double): String = d.toString
}
```

Instead, let's make the fact that some of our functions can fail explicit in the return type.

```scala mdoc:silent
object EitherStyle {
  def parse(s: String): Either[Exception, Int] =
    if (s.matches("-?[0-9]+")) Either.right(s.toInt)
    else Either.left(new NumberFormatException(s"${s} is not a valid integer."))

  def reciprocal(i: Int): Either[Exception, Double] =
    if (i == 0) Either.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
    else Either.right(1.0 / i)

  def stringify(d: Double): String = d.toString
}
```

Now, using combinators like `flatMap` and `map`, we can compose our functions together.

```scala mdoc:silent
import EitherStyle._

def magic(s: String): Either[Exception, String] =
  parse(s).flatMap(reciprocal).map(stringify)
```

With the composite function that we actually care about, we can pass in strings and then pattern
match on the exception. Because `Either` is a sealed type (often referred to as an algebraic data type,
or ADT), the compiler will complain if we do not check both the `Left` and `Right` case.

```scala mdoc
magic("123") match {
  case Left(_: NumberFormatException) => println("not a number!")
  case Left(_: IllegalArgumentException) => println("can't take reciprocal of 0!")
  case Left(_) => println("got unknown exception")
  case Right(s) => println(s"Got reciprocal: ${s}")
}
```

Not bad - if we leave out any of those clauses the compiler will yell at us, as it should. However,
note the `Left(_)` clause - the compiler will complain if we leave that out because it knows
that given the type `Either[Exception, String]`, there can be inhabitants of `Left` that are not
`NumberFormatException` or `IllegalArgumentException`. However, we "know" by inspection of the source
that those will be the only exceptions thrown, so it seems strange to have to account for other exceptions.
This implies that there is still room to improve.

### Example usage: Round 2
Instead of using exceptions as our error value, let's instead enumerate explicitly the things that
can go wrong in our program.

```scala mdoc:reset:silent
import cats.implicits._

object EitherStyle {
  sealed abstract class Error
  final case class NotANumber(string: String) extends Error
  case object NoZeroReciprocal extends Error

  def parse(s: String): Either[Error, Int] =
    if (s.matches("-?[0-9]+")) Either.right(s.toInt)
    else Either.left(NotANumber(s))

  def reciprocal(i: Int): Either[Error, Double] =
    if (i == 0) Either.left(NoZeroReciprocal)
    else Either.right(1.0 / i)

  def stringify(d: Double): String = d.toString

  def magic(s: String): Either[Error, String] =
    parse(s).flatMap(reciprocal).map(stringify)
}
```

For our little module, we enumerate any and all errors that can occur. Then, instead of using
exception classes as error values, we use one of the enumerated cases. Now when we pattern
match, we get much nicer matching. Moreover, since `Error` is `sealed`, no outside code can
add additional subtypes which we might fail to handle.

```scala mdoc
import EitherStyle._

magic("123") match {
  case Left(NotANumber(_)) => println("not a number!")
  case Left(NoZeroReciprocal) => println("can't take reciprocal of 0!")
  case Right(s) => println(s"Got reciprocal: ${s}")
}
```

## Either in the small, Either in the large
Once you start using `Either` for all your error-handling, you may quickly run into an issue where
you need to call into two separate modules which give back separate kinds of errors.

```scala mdoc:silent
sealed abstract class DatabaseError
trait DatabaseValue

object Database {
  def databaseThings(): Either[DatabaseError, DatabaseValue] = ???
}

sealed abstract class ServiceError
trait ServiceValue

object Service {
  def serviceThings(v: DatabaseValue): Either[ServiceError, ServiceValue] = ???
}
```

Let's say we have an application that wants to do database things, and then take database
values and do service things. Glancing at the types, it looks like `flatMap` will do it.

```scala
def doApp = Database.databaseThings().flatMap(Service.serviceThings)
```

This line will compile and work as expected, no matter if you're on 2.12 or an earlier
version of Scala.  The `flatMap` we get here (either provided by Cats's `Either` syntax for
Scala 2.10 and 2.11, or, in Scala 2.12, a method on `Either`) has this signature:

```scala
def flatMap[AA >: A, Y](f: (B) => Either[AA, Y]): Either[AA, Y]
```

This `flatMap` is different from the ones you'll find on `List` or `Option`, for example, in that it
has two type parameters, with the extra `AA` parameter allowing us to `flatMap` into an `Either`
with a different type on the left side.

This behavior is consistent with the covariance of `Either`, and in some cases it can be convenient,
but it also makes it easy to run into nasty variance issues - such as `Object` being inferred as the
type of the left side, as it is in this case.


### Solution 1: Application-wide errors
We may then be tempted to make our entire application share an error data type.

```scala mdoc:nest:silent
sealed abstract class AppError
case object DatabaseError1 extends AppError
case object DatabaseError2 extends AppError
case object ServiceError1 extends AppError
case object ServiceError2 extends AppError

trait DatabaseValue

object Database {
  def databaseThings(): Either[AppError, DatabaseValue] = ???
}

object Service {
  def serviceThings(v: DatabaseValue): Either[AppError, ServiceValue] = ???
}

def doApp = Database.databaseThings().flatMap(Service.serviceThings)
```

This certainly works, or at least it compiles. But consider the case where another module wants to just use
`Database`, and gets an `Either[AppError, DatabaseValue]` back. Should it want to inspect the errors, it
must inspect **all** the `AppError` cases, even though it was only intended for `Database` to use
`DatabaseError1` or `DatabaseError2`.

### Solution 2: ADTs all the way down
Instead of lumping all our errors into one big ADT, we can instead keep them local to each module, and have
an application-wide error ADT that wraps each error ADT we need.

```scala mdoc:nest:silent
sealed abstract class DatabaseError
trait DatabaseValue

object Database {
  def databaseThings(): Either[DatabaseError, DatabaseValue] = ???
}

sealed abstract class ServiceError
trait ServiceValue

object Service {
  def serviceThings(v: DatabaseValue): Either[ServiceError, ServiceValue] = ???
}

sealed abstract class AppError
object AppError {
  final case class Database(error: DatabaseError) extends AppError
  final case class Service(error: ServiceError) extends AppError
}
```

Now in our outer application, we can wrap/lift each module-specific error into `AppError` and then
call our combinators as usual. `Either` provides a convenient method to assist with this, called `Either.leftMap` -
it can be thought of as the same as `map`, but for the `Left` side.

```scala mdoc:silent
def doApp: Either[AppError, ServiceValue] =
  Database.databaseThings().leftMap[AppError](AppError.Database).
  flatMap(dv => Service.serviceThings(dv).leftMap(AppError.Service))
```

Hurrah! Each module only cares about its own errors as it should be, and more composite modules have their
own error ADT that encapsulates each constituent module's error ADT. Doing this also allows us to take action
on entire classes of errors instead of having to pattern match on each individual one.

```scala mdoc:silent
def awesome =
  doApp match {
    case Left(AppError.Database(_)) => "something in the database went wrong"
    case Left(AppError.Service(_))  => "something in the service went wrong"
    case Right(_)                   => "everything is alright!"
  }
```


## Working with exception-y code
There will inevitably come a time when your nice `Either` code will have to interact with exception-throwing
code. Handling such situations is easy enough.

```scala mdoc
val either: Either[NumberFormatException, Int] =
  try {
    Either.right("abc".toInt)
  } catch {
    case nfe: NumberFormatException => Either.left(nfe)
  }
```

However, this can get tedious quickly. `Either` has a `catchOnly` method on its companion object
(via syntax enrichment) that allows you to pass it a function, along with the type of exception you want to catch, and does the
above for you.

```scala mdoc:nest
val either: Either[NumberFormatException, Int] =
  Either.catchOnly[NumberFormatException]("abc".toInt)
```

If you want to catch all (non-fatal) throwables, you can use `catchNonFatal`.

```scala mdoc:nest
val either: Either[Throwable, Int] = Either.catchNonFatal("abc".toInt)
```

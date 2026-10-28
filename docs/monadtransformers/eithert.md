# EitherT

API Documentation: @:api(cats.data.EitherT)

@:callout(warning)

`EitherT` can interact poorly with more powerful effect types that provide concurrent computation, such as Cats Effect's `IO` or fs2's `Stream`. Because `IO` already provides its own error channel, `EitherT[IO, Throwable, A]` can lead to confusing behavior; prefer `IO`'s own error channel instead of carrying a second throwable error channel.

@:@

`Either` can be used for error handling in most situations. However, when
`Either` is placed into effectful types such as `Option`, a large
amount of boilerplate is required to handle errors. For example, consider the
following program:

```scala mdoc
import scala.util.Try
import cats.syntax.all._

def parseDouble(s: String): Either[String, Double] =
  Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))

def divide(a: Double, b: Double): Either[String, Double] =
  Either.cond(b != 0, a / b, "Cannot divide by zero")

def divisionProgram(inputA: String, inputB: String): Either[String, Double] =
  for {
    a <- parseDouble(inputA)
    b <- parseDouble(inputB)
    result <- divide(a, b)
  } yield result

divisionProgram("4", "2") // Right(2.0)
divisionProgram("a", "b") // Left("a is not a number")
```

Suppose `parseDouble` and `divide` are rewritten to be asynchronous and return
`Future[Either[String, Double]]` instead. The for-comprehension can no longer be
used since `divisionProgram` must now compose `Future` and `Either` together,
which means that the error handling must be performed explicitly to ensure that
the proper types are returned:

```scala mdoc:silent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

def parseDoubleAsync(s: String): Future[Either[String, Double]] =
  Future.successful(parseDouble(s))
def divideAsync(a: Double, b: Double): Future[Either[String, Double]] =
  Future.successful(divide(a, b))

def divisionProgramAsync(inputA: String, inputB: String): Future[Either[String, Double]] =
  parseDoubleAsync(inputA) flatMap { eitherA =>
    parseDoubleAsync(inputB) flatMap { eitherB =>
      (eitherA, eitherB) match {
        case (Right(a), Right(b)) => divideAsync(a, b)
        case (Left(err), _) => Future.successful(Left(err))
        case (_, Left(err)) => Future.successful(Left(err))
      }
    }
  }
```

Clearly, the updated code is less readable and more verbose: the details of the
program are now mixed with the error handling. In addition, as more `Either`s
and `Futures` are included, the amount of boilerplate required to properly
handle the errors will increase dramatically.

## EitherT

`EitherT[F[_], A, B]` is a lightweight wrapper for `F[Either[A, B]]` that makes
it easy to compose `Either`s and `F`s together. To use `EitherT`, values of
`Either`, `F`, `A`, and `B` are first converted into `EitherT`, and the
resulting `EitherT` values are then composed using combinators. For example, the asynchronous division program can be rewritten as follows:

```scala mdoc:nest
import cats.data.EitherT
import cats.syntax.all._

def divisionProgramAsync(inputA: String, inputB: String): EitherT[Future, String, Double] =
  for {
    a <- EitherT(parseDoubleAsync(inputA))
    b <- EitherT(parseDoubleAsync(inputB))
    result <- EitherT(divideAsync(a, b))
  } yield result

divisionProgramAsync("4", "2").value
divisionProgramAsync("a", "b").value
```

Note that when `F` is a monad, then `EitherT` will also form a monad, allowing
monadic combinators such as `flatMap` to be used in composing `EitherT` values.

## Extracting an `F[Either[A, B]]` from an `EitherT[F, A, B]`

Use the `value` method defined on `EitherT` to retrieve the underlying `F[Either[A, B]]`:

```scala mdoc:nest
val errorT: EitherT[Future, String, Int] = EitherT.leftT("foo")

val error: Future[Either[String, Int]] = errorT.value
```

## Right Bias

Like `Either`, `EitherT` is _right-biased_, therefore plain `map` actually deals with the `Right` value. There are also certain methods in `EitherT` (that you're likely to need at some point) which map over the `Left` value, like `leftMap`, or over both `Left` and `Right` values, like `fold` or `bimap`.

## Transforming to `EitherT[F, A, B]`

### From `A` or `B`

To obtain a left version or a right version of `EitherT` when given an `A` or a
`B`, use `EitherT.leftT` and `EitherT.rightT` (which is an alias for
`EitherT.pure`), respectively.

```scala mdoc:silent
val number: EitherT[Option, String, Int] = EitherT.rightT(5)
val numberError: EitherT[Option, String, Int] = EitherT.leftT("Not a number")
```

### From `F[A]` or `F[B]`

Similarly, use `EitherT.left` and `EitherT.right` to convert an `F[A]` or an `F[B]`
into an `EitherT`. It is also possible to use `EitherT.liftF` as an alias for
`EitherT.right`.

```scala mdoc:nest:silent
val numberO: Option[Int] = Some(5)
val errorO: Option[String] = Some("Not a number")

val number: EitherT[Option, String, Int] = EitherT.right(numberO)
val error: EitherT[Option, String, Int] = EitherT.left(errorO)
```

### From `Either[A, B]` or `F[Either[A, B]]`

Use `EitherT.fromEither` to lift a value of `Either[A, B]` into `EitherT[F, A, B]`.
An `F[Either[A, B]]` can be converted into `EitherT` using the `EitherT` constructor.

```scala mdoc:silent
val numberE: Either[String, Int] = Right(100)
val errorE: Either[String, Int] = Left("Not a number")
val numberFE: List[Either[String, Int]] = List(Right(250))

val numberET: EitherT[List, String, Int] = EitherT.fromEither(numberE)
val errorET: EitherT[List, String, Int] = EitherT.fromEither(errorE)
val numberFET: EitherT[List, String, Int] = EitherT(numberFE)
```

### From `Option[B]` or `F[Option[B]]`

An `Option[B]` or an `F[Option[B]]`, along with a default value, can be passed to
`EitherT.fromOption` and `EitherT.fromOptionF`, respectively, to produce an
`EitherT`. For `F[Option[B]]` and default `F[A]`, there is `EitherT.fromOptionM`.

```scala mdoc
val myOption: Option[Int] = None
val myOptionList: List[Option[Int]] = List(None, Some(2), Some(3), None, Some(5))

val myOptionET = EitherT.fromOption[Future](myOption, "option not defined")
val myOptionListET = EitherT.fromOptionF(myOptionList, "option not defined")
val myOptionListETM = EitherT.fromOptionM(myOptionList, List("option not defined"))
```

### From `OptionT[F, A]`

Another useful way to construct an `EitherT` instance is to use `OptionT`'s methods `toLeft` and `toRight`:

```scala mdoc:silent
import cats.data.OptionT

abstract class BaseException(message: String) extends Exception(message)

case class UserNotFoundException(message: String) extends BaseException(message)

def getUserById(userId: Int): Future[Option[String]] = { /* ... */ ??? }

def ensureUserExists(userId: Int): Future[Either[BaseException, String]] =
  OptionT(getUserById(userId))
    .toRight(left = UserNotFoundException(s"user not found, userId=$userId"))
    .value
```

`toRight` is pretty analogous to the method `Either.fromOption` mentioned before: just as `fromOption` built an `Either` from an `Option`, `toRight` creates an `EitherT` from an `OptionT`.
If the original `OptionT` stores `Some` value, it will be wrapped into `Right`; otherwise the value provided as the `left` parameter will be wrapped into a `Left`.
To provide the `left` value within the monad, there is corresponding `toRightF` method.

`toLeft` is `toRight`'s counterpart which wraps the `Some` value into `Left` and transforms `None` into `Right` enclosing the provided `right` value.
This is less commonly used in practice, but can serve e.g. for enforcing uniqueness checks in code.
We return `Left` if the value has been found, and `Right` if it doesn't yet exist in the system.

### From `ApplicativeError[F, E]`

An `ApplicativeError[F, E]` or a `MonadError[F, E]`, can be converted into `EitherT[F, E, A]` 
using the `attemptT` method on them.

```scala mdoc:silent
val myTry: Try[Int] = Try(2)
val myFuture: Future[String] = Future.failed(new Exception())

val myTryET: EitherT[Try, Throwable, Int] = myTry.attemptT
val myFutureET: EitherT[Future, Throwable, String] = myFuture.attemptT
```

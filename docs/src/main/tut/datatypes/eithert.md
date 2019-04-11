---
layout: docs
title:  "EitherT"
section: "data"
source: "core/src/main/scala/cats/data/EitherT.scala"
scaladoc: "#cats.data.EitherT"
---
# EitherT

`Either` can be used for error handling in most situations. However, when
`Either` is placed into effectful types such as `Option` or`Future`, a large
amount of boilerplate is required to handle errors. For example, consider the
following program:

```tut:book
import scala.util.Try
import cats.implicits._

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

```tut:silent
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
resulting `EitherT` values are then composed using combinators. For example, the
asynchronous division program can be rewritten as follows:

```tut:book
import cats.data.EitherT
import cats.implicits._

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

## From `A` or `B` to `EitherT[F, A, B]`

To obtain a left version or a right version of `EitherT` when given an `A` or a
`B`, use `EitherT.leftT` and `EitherT.rightT` (which is an alias for
`EitherT.pure`), respectively.

```tut:silent
val number: EitherT[Option, String, Int] = EitherT.rightT(5)
val error: EitherT[Option, String, Int] = EitherT.leftT("Not a number")
```

## From `F[A]` or `F[B]` to `EitherT[F, A, B]`

Similarly, use `EitherT.left` and `EitherT.right` to convert an `F[A]` or an `F[B]`
into an `EitherT`. It is also possible to use `EitherT.liftF` as an alias for
`EitherT.right`.

```tut:silent
val numberO: Option[Int] = Some(5)
val errorO: Option[String] = Some("Not a number")

val number: EitherT[Option, String, Int] = EitherT.right(numberO)
val error: EitherT[Option, String, Int] = EitherT.left(errorO)
```

## From `Either[A, B]` or `F[Either[A, B]]` to `EitherT[F, A, B]`

Use `EitherT.fromEither` to lift a value of `Either[A, B]` into `EitherT[F, A, B]`.
An `F[Either[A, B]]` can be converted into `EitherT` using the `EitherT` constructor.

```tut:silent
val numberE: Either[String, Int] = Right(100)
val errorE: Either[String, Int] = Left("Not a number")
val numberFE: List[Either[String, Int]] = List(Right(250))

val numberET: EitherT[List, String, Int] = EitherT.fromEither(numberE)
val errorET: EitherT[List, String, Int] = EitherT.fromEither(errorE)
val numberFET: EitherT[List, String, Int] = EitherT(numberFE)
```

## From `Option[B]` or `F[Option[B]]` to `EitherT[F, A, B]`

An `Option[B]` or an `F[Option[B]]`, along with a default value, can be passed to
`EitherT.fromOption` and `EitherT.fromOptionF`, respectively, to produce an
`EitherT`.

```tut:book
val myOption: Option[Int] = None
val myOptionList: List[Option[Int]] = List(None, Some(2), Some(3), None, Some(5))

val myOptionET = EitherT.fromOption[Future](myOption, "option not defined")
val myOptionListET = EitherT.fromOptionF(myOptionList, "option not defined")
```

## Extracting an `F[Either[A, B]]` from an `EitherT[F, A, B]`

Use the `value` method defined on `EitherT` to retrieve the underlying `F[Either[A, B]]`:

```tut:book
val errorT: EitherT[Future, String, Int] = EitherT.leftT("foo")

val error: Future[Either[String, Int]] = errorT.value
```

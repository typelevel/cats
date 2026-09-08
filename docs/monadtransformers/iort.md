# IorT

API Documentation: @:api(cats.data.IorT)

`IorT[F[_], A, B]` is a light wrapper on an `F[Ior[A, B]]`. Similar to
`OptionT[F[_], A]` and `EitherT[F[_], A, B]`, it is a monad transformer for
`Ior`, that can be more convenient to work with than using `F[Ior[A, B]]`
directly.

## The boilerplate

Consider the following program that uses `Ior` to propagate log messages when
validating an address:

```scala mdoc:silent
import cats.data.Ior
import cats.data.{ NonEmptyChain => Nec }
import cats.syntax.all._
import scala.util.{Success, Try}

type Logs = Nec[String]

def parseNumber(input: String): Ior[Logs, Option[Int]] =
  Try(input.trim.toInt) match {
    case Success(number) if number > 0 => Ior.Right(Some(number))
    case Success(_) => Ior.Both(Nec.one(s"'$input' is non-positive number"), None)
    case _ => Ior.Both(Nec.one(s"'$input' is not a number"), None)
  }

def parseStreet(input: String): Ior[Logs, String] = {
  if (input.trim.isEmpty)
    Ior.Left(Nec.one(s"'$input' is not a street"))
  else
    Ior.Right(input)
}

def numberToString(number: Option[Int]): Ior[Logs, String] =
  number match {
    case Some(n) => Ior.Right(n.toString)
    case None => Ior.Both(Nec.one("used default address number"), "n/a")
  }

def addressProgram(numberInput: String, streetInput: String): Ior[Logs, String] =
  for {
    number <- parseNumber(numberInput)
    street <- parseStreet(streetInput)
    sNumber <- numberToString(number)
  } yield s"$sNumber, $street"
```

Due to the monadic nature of `Ior` combining the results of `parseNumber`,
`parseStreet`, and `numberToString` can be as concise as a for-comprehension.
As the following examples demonstrate, log messages of the different processing
steps are combined when using `flatMap`.

```scala mdoc
addressProgram("7", "Buckingham Palace Rd")
addressProgram("SW1W", "Buckingham Palace Rd")
addressProgram("SW1W", "")
```

Suppose `parseNumber`, `parseStreet`, and `numberToString` are rewritten to be
asynchronous and return `Future[Ior[Logs, *]]` instead. The for-comprehension
can no longer be used since `addressProgram` must now compose `Future` and
`Ior` together, which means that the error handling must be performed
explicitly to ensure that the proper types are returned:

```scala mdoc:silent
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

def parseNumberAsync(input: String): Future[Ior[Logs, Option[Int]]] =
  Future.successful(parseNumber(input))

def parseStreetAsync(input: String): Future[Ior[Logs, String]] =
  Future.successful(parseStreet(input))

def numberToStringAsync(number: Option[Int]): Future[Ior[Logs, String]] =
  Future.successful(numberToString(number))

def programHelper(number: Option[Int], streetInput: String): Future[Ior[Logs, String]] =
  parseStreetAsync(streetInput).flatMap { streetIor =>
    numberToStringAsync(number).map { sNumberIor =>
      for {
        street <- streetIor
        sNumber <- sNumberIor
      } yield s"$sNumber, $street"
    }
  }

def addressProgramAsync(numberInput: String, streetInput: String): Future[Ior[Logs, String]] =
  parseNumberAsync(numberInput).flatMap {
    case Ior.Left(logs) => Future.successful(Ior.Left(logs))
    case Ior.Right(number) => programHelper(number, streetInput)
    case b @ Ior.Both(_, number) => programHelper(number, streetInput).map(s => b.flatMap(_ => s))
  }
```

To keep some readability the program was split in two parts, otherwise code
would be repeated. Note that when `parseNumberAsync` returns an `Ior.Both` it
is necessary to combine it with the result of `programHelper`, otherwise some
log messages would be lost.

```scala mdoc
import scala.concurrent.Await
import scala.concurrent.duration._

Await.result(addressProgramAsync("7", "Buckingham Palace Rd"), 1.second)
Await.result(addressProgramAsync("SW1W", "Buckingham Palace Rd"), 1.second)
Await.result(addressProgramAsync("SW1W", ""), 1.second)
```

## IorT to the rescue

The program of the previous section can be re-written using `IorT` as follows:

```scala mdoc:silent
import cats.data.IorT

def addressProgramAsyncIorT(numberInput: String, streetInput: String): IorT[Future, Logs, String] =
  for {
    number <- IorT(parseNumberAsync(numberInput))
    street <- IorT(parseStreetAsync(streetInput))
    sNumber <- IorT(numberToStringAsync(number))
  } yield s"$sNumber, $street"
```

This version of `addressProgramAsync` is almost as concise as the
non-asynchronous version. Note that when `F` is a monad, then `IorT` will also
form a monad, allowing monadic combinators such as `flatMap` to be used in
composing `IorT` values.

```scala mdoc
Await.result(addressProgramAsyncIorT("7", "Buckingham Palace Rd").value, 1.second)
Await.result(addressProgramAsyncIorT("SW1W", "Buckingham Palace Rd").value, 1.second)
Await.result(addressProgramAsyncIorT("SW1W", "").value, 1.second)
```

Looking back at the implementation of `parseStreet` the return type could be
`Either[Logs, String]` instead. Thinking of situations like this, where not all
the types match, `IorT` provides factory methods of `IorT[F, A, B]` from:
* `A`, `B` or both
* `F[A]`, `F[B]` or both
* `Ior[A, B]` or `F[Ior[A, B]]`
* `Either[A, B]` or `F[Either[A, B]]`
* `Option[B]` or `F[Option[B]]`
* A `Boolean` test

## From `A` and/or `B` to `IorT[F, A, B]`

To obtain a left version of `IorT` when given an `A` use `IorT.leftT`. When
given a `B` a right version of `IorT` can be obtained with `IorT.rightT` (which
is an alias for `IorT.pure`). Given both an `A` and a `B` use `IorT.bothT`.

Note the two styles for providing the `IorT` missing type parameters. The first
three expressions only specify not inferable types, while the last expression
specifies all types.

```scala mdoc:nest
val number = IorT.rightT[Option, String](5)
val error = IorT.leftT[Option, Int]("Not a number")
val weirdNumber = IorT.bothT[Option]("Not positive", -1)

val numberPure: IorT[Option, String, Int] = IorT.pure(5)
```

## From `F[A]` and/or `F[B]` to `IorT[F, A, B]`

Similarly, use `IorT.left`, `IorT.right`, `IorT.both` to convert an `F[A]`
and/or `F[B]` into an `IorT`. It is also possible to use `IorT.liftF` as an
alias for `IorT.right`.

```scala mdoc:silent:nest
val numberF: Option[Int] = Some(5)
val errorF: Option[String] = Some("Not a number")

val warningF: Option[String] = Some("Not positive")
val weirdNumberF: Option[Int] = Some(-1)

val number: IorT[Option, String, Int] = IorT.right(numberF)
val error: IorT[Option, String, Int] = IorT.left(errorF)
val weirdNumber: IorT[Option, String, Int] = IorT.both(warningF, weirdNumberF)
```

## From `Ior[A, B]` or `F[Ior[A, B]]` to `IorT[F, A, B]`

Use `IorT.fromIor` to a lift a value of `Ior[A, B]` into `IorT[F, A, B]`. An
`F[Ior[A, B]]` can be converted into `IorT` using the `IorT` constructor.

```scala mdoc:silent:nest
val numberIor: Ior[String, Int] = Ior.Right(5)
val errorIor: Ior[String, Int] = Ior.Left("Not a number")
val weirdNumberIor: Ior[String, Int] = Ior.both("Not positive", -1)
val numberFIor: Option[Ior[String, Int]] = Option(Ior.Right(5))

val number: IorT[Option, String, Int] = IorT.fromIor(numberIor)
val error: IorT[Option, String, Int] = IorT.fromIor(errorIor)
val weirdNumber: IorT[Option, String, Int] = IorT.fromIor(weirdNumberIor)
val numberF: IorT[Option, String, Int] = IorT(numberFIor)
```

## From `Either[A, B]` or `F[Either[A, B]]` to `IorT[F, A, B]`

Use `IorT.fromEither` or `IorT.fromEitherF` to create a value of
`IorT[F, A, B]` from an `Either[A, B]` or a `F[Either[A, B]]`, respectively.

```scala mdoc:silent:nest
val numberEither: Either[String, Int] = Right(5)
val errorEither: Either[String, Int] = Left("Not a number")
val numberFEither: Option[Either[String, Int]] = Option(Right(5))

val number: IorT[Option, String, Int] = IorT.fromEither(numberEither)
val error: IorT[Option, String, Int] = IorT.fromEither(errorEither)
val numberF: IorT[Option, String, Int] = IorT.fromEitherF(numberFEither)
```

## From `Option[B]` or `F[Option[B]]` to `IorT[F, A, B]`

An `Option[B]` or an `F[Option[B]]`, along with a default value, can be passed
to `IorT.fromOption` and `IorT.fromOptionF`, respectively, to produce an
`IorT`. For `F[Option[B]]` and default `F[A]`, there is `IorT.fromOptionM`.

```scala mdoc:silent:nest
val numberOption: Option[Int] = None
val numberFOption: List[Option[Int]] = List(None, Some(2), None, Some(5))

val number = IorT.fromOption[List](numberOption, "Not defined")
val numberF = IorT.fromOptionF(numberFOption, "Not defined")
val numberM = IorT.fromOptionM(numberFOption, List("Not defined"))
```

## Creating an `IorT[F, A, B]` from a `Boolean` test

`IorT.cond` allows concise creation of an `IorT[F, A, B]` based on a `Boolean`
test, an `A` and a `B`. Similarly, `IorT.condF` uses `F[A]` and `F[B]`.

```scala mdoc:silent:nest
val number: Int = 10
val informedNumber: IorT[Option, String, Int] = IorT.cond(number % 10 != 0, number, "Number is multiple of 10")
val uninformedNumber: IorT[Option, String, Int] = IorT.condF(number % 10 != 0, Some(number), None)
```

## Extracting an `F[Ior[A, B]]` from an `IorT[F, A, B]`

Use the `value` method defined on `IorT` to retrieve the underlying
`F[Ior[A, B]]`:

```scala mdoc:silent:nest
val errorT: IorT[Option, String, Int] = IorT.leftT("Not a number")

val error: Option[Ior[String, Int]] = errorT.value
```

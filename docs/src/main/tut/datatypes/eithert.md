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

def stringToDouble(s: String): Either[String, Double] =
  Try(s.toDouble).toEither match {
    case Left(_) => Left(s"$s is not a number")
    case Right(n) => Right(n)
  }

def divide(a: Double, b: Double): Either[String, Double] =
  Either.cond(b == 0, a / b, "Cannot divide by zero")

def divisionProgram(inputA: String, inputB: String): Either[String, Double] =
  for {
    a <- stringToDouble(inputA)
    b <- stringToDouble(inputB)
    result <- divide(a, b)
  } yield result

divisionProgram("4", "2")
divisionProgram("a", "b")
```

Suppose `stringToDouble` and `divide` are rewritten to be asynchronous and
return `Future[Either[String, Double]]` instead. The for-comprehension can no
longer be used since `divisionProgram` must now compose `Future` and `Either`
together.

```tut:silent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

def stringToDouble(s: String): Future[Either[String, Double]] = ???
def divide(a: Double, b: Double): Future[Either[String, Double]] = ???

def divisionProgram(inputA: String, inputB: String): Future[Either[String, Double]] =
  stringToDouble(inputA) flatMap { eitherA =>
    stringToDouble(inputB) flatMap { eitherB =>
      val parseResult = for {
        a <- eitherA
        b <- eitherB
      } yield (a, b)
      
      parseResult match {
        case Right((a, b)) => divide(a, b)
        case l@Left(err) => Future.successful(Left(err))
      }
    }
  }
```

It is easy to see that as additional `Either`s and `Futures` are included, the
amount of boilerplate required to properly handle the errors will increase
dramatically.

## EitherT

`EitherT[F[_], A, B]` is a lightweight wrapper for `F[Either[A, B]]` that makes
it easy to compose `Either`s and `F`s together. Using `EitherT`, the asynchronous
division program can be rewritten as follows:

```tut:silent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try
import cats.data.EitherT
import cats.implicits._

def stringToDouble(s: String): Future[Either[String, Double]] = ???
def divide(a: Double, b: Double): Future[Either[String, Double]] = ???

def divisionProgram(inputA: String, inputB: String): EitherT[Future, String, Double] =
  for {
    a <- EitherT(stringToDouble(inputA))
    b <- EitherT(stringToDouble(inputB))
    result <- EitherT(divide(a, b))
  } yield result
```

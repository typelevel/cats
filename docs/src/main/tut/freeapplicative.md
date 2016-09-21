---
layout: default
title:  "FreeApplicatives"
section: "data"
source: "free/src/main/scala/cats/free/FreeApplicative.scala"
scaladoc: "#cats.free.FreeApplicative"
---
# Free Applicative

`FreeApplicative`s are similar to `Free` (monads) in that they provide a nice way to represent
computations as data and are useful for building embedded DSLs (EDSLs). However, they differ
from `Free` in that the kinds of operations they support are limited, much like the distinction
between `Applicative` and `Monad`.

## Dependency

If you'd like to use cats' free applicative, you'll need to add a library dependency
for the `cats-free` module.

## Example
Consider building an EDSL for validating strings - to keep things simple we'll just have
a way to check a string is at least a certain size and to ensure the string contains numbers.

```tut:silent
sealed abstract class ValidationOp[A]
case class Size(size: Int) extends ValidationOp[Boolean]
case object HasNumber extends ValidationOp[Boolean]
```

Much like the `Free` monad tutorial, we use smart constructors to lift our algebra into the `FreeApplicative`.

```tut:silent
import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift

type Validation[A] = FreeApplicative[ValidationOp, A]

def size(size: Int): Validation[Boolean] = lift(Size(size))

val hasNumber: Validation[Boolean] = lift(HasNumber)
```

Because a `FreeApplicative` only supports the operations of `Applicative`, we do not get the nicety
of a for-comprehension. We can however still use `Applicative` syntax provided by Cats.

```tut:silent
import cats.implicits._

val prog: Validation[Boolean] = (size(5) |@| hasNumber).map { case (l, r) => l && r}
```

As it stands, our program is just an instance of a data structure - nothing has happened
at this point. To make our program useful we need to interpret it.

```tut:silent
import cats.Id
import cats.arrow.FunctionK
import cats.implicits._

// a function that takes a string as input
type FromString[A] = String => A

val compiler =
   λ[FunctionK[ValidationOp, FromString]] { fa =>
      str =>
        fa match {
          case Size(size) => str.size >= size
          case HasNumber  => str.exists(c => "0123456789".contains(c))
        }
   }
```

```tut:book
val validator = prog.foldMap[FromString](compiler)
validator("1234")
validator("12345")
```

## Differences from `Free`
So far everything we've been doing has been not much different from `Free` - we've built
an algebra and interpreted it. However, there are some things `FreeApplicative` can do that
`Free` cannot.

Recall a key distinction between the type classes `Applicative` and `Monad` - `Applicative`
captures the idea of independent computations, whereas `Monad` captures that of dependent
computations. Put differently `Applicative`s cannot branch based on the value of an existing/prior
computation. Therefore when using `Applicative`s, we must hand in all our data in one go.

In the context of `FreeApplicative`s, we can leverage this static knowledge in our interpreter.

### Parallelism
Because we have everything we need up front and know there can be no branching, we can easily
write a validator that validates in parallel.

```tut:silent
import cats.data.Kleisli
import cats.implicits._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

// recall Kleisli[Future, String, A] is the same as String => Future[A]
type ParValidator[A] = Kleisli[Future, String, A]

val parCompiler =
  λ[FunctionK[ValidationOp, ParValidator]] { fa =>
    Kleisli { str =>
      fa match {
        case Size(size) => Future { str.size >= size }
        case HasNumber  => Future { str.exists(c => "0123456789".contains(c)) }
      }
    }
  }

val parValidation = prog.foldMap[ParValidator](parCompiler)
```

### Logging
We can also write an interpreter that simply creates a list of strings indicating the filters that
have been used - this could be useful for logging purposes. Note that we need not actually evaluate
the rules against a string for this, we simply need to map each rule to some identifier. Therefore
we can completely ignore the return type of the operation and return just a `List[String]` - the
`Const` data type is useful for this.

```tut:silent
import cats.data.Const
import cats.implicits._

type Log[A] = Const[List[String], A]

val logCompiler =
  λ[FunctionK[ValidationOp, Log]] {
    case Size(size) => Const(List(s"size >= $size"))
    case HasNumber  => Const(List("has number"))
  }

def logValidation[A](validation: Validation[A]): List[String] =
  validation.foldMap[Log](logCompiler).getConst
```

```tut:book
logValidation(prog)
logValidation(size(5) *> hasNumber *> size(10))
logValidation((hasNumber |@| size(3)).map(_ || _))
```

### Why not both?
It is perhaps more plausible and useful to have both the actual validation function and the logging
strings. While we could easily compile our program twice, once for each interpreter as we have above,
we could also do it in one go - this would avoid multiple traversals of the same structure.

Another useful property `Applicative`s have over `Monad`s is that given two `Applicative`s `F[_]` and
`G[_]`, their product `type FG[A] = (F[A], G[A])` is also an `Applicative`. This is not true in the general
case for monads.

Therefore, we can write an interpreter that uses the product of the `ParValidator` and `Log` `Applicative`s
to interpret our program in one go.

```tut:silent
import cats.data.Prod

type ValidateAndLog[A] = Prod[ParValidator, Log, A]

val prodCompiler =
  λ[FunctionK[ValidationOp, ValidateAndLog]] {
    case Size(size) =>
      val f: ParValidator[Boolean] = Kleisli(str => Future { str.size >= size })
      val l: Log[Boolean] = Const(List(s"size > $size"))
      Prod[ParValidator, Log, Boolean](f, l)
    case HasNumber  =>
      val f: ParValidator[Boolean] = Kleisli(str => Future(str.exists(c => "0123456789".contains(c))))
      val l: Log[Boolean] = Const(List("has number"))
      Prod[ParValidator, Log, Boolean](f, l)
  }

val prodValidation = prog.foldMap[ValidateAndLog](prodCompiler)
```

## References
Deeper explanations can be found in this paper [Free Applicative Functors by Paolo Capriotti](http://www.paolocapriotti.com/assets/applicative.pdf)

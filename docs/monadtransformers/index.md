# Overview

Monad transformers are data types used to simplify the composition of monads. For example:

## OptionT

```scala mdoc:silent
import cats.data.OptionT
```

An instance of [`OptionT[F, A]`](optiont.md) can be thought of as a wrapper over `F[Option[A]]`
which adds a couple of useful methods specific to nested types that aren't available in `F` or `Option` itself.
Most typically, your `F` will be `IO` from [Cats Effect](https://typelevel.org/cats-effect), or something like [doobie](https://typelevel.org/doobie/index.html)'s `ConnectionIO`).
Wrappers such as `OptionT` are generally known as _monad transformers_.

A quite common pattern is mapping the inner value stored inside an instance of `F[Option[A]]` to an instance of `F[Option[B]]` with a function of type `A => B`.
This can be done with rather verbose syntax like:
```scala mdoc:silent
import scala.util.Try

lazy val resultTry: Try[Option[Int]] = ???

def mappedResultTry: Try[Option[String]] = resultTry.map { maybeValue =>
  maybeValue.map { value =>
    // Do something with the value and return String
    ???
  }
}
```

With the use of `OptionT`, this can be simplified as follows:

```scala mdoc:silent
import cats.data.OptionT

def mappedResultTry2: OptionT[Try, String] = OptionT(resultTry).map { value =>
  // Do something with the value and return String
  ???
}
```

The above `map` will return a value of type `OptionT[Try, String]`.
To get the underlying `Try[Option[String]]` value, simply call `.value` on the `OptionT` instance.

There are several ways to construct an `OptionT` instance.
The method headers in the table below are slightly simplified: the type parameters and type classes required by each method are skipped.

| Method | Takes | Returns |
| :---: | :---: | :---: |
| `OptionT.apply` or `OptionT(...)` | `F[Option[A]]` | `OptionT[F, A]` |
| `OptionT.fromOption` | `Option[A]` | `OptionT[F, A]` |
| `OptionT.liftF` | `F[A]` | `OptionT[F, A]` |
| `OptionT.pure` | `A` | `OptionT[F, A]` |

In production code you'll most commonly use the `OptionT(...)` syntax in order to wrap an instance of `F[Option[A]]` into `OptionT[F, A]`.
The other methods, in turn, prove useful to set up `OptionT`-typed dummy values in unit tests.

We have already come across one of `OptionT`'s methods, namely `map`.
There are several other methods available and they mostly differ by the signature of the function they accept as the parameter.
As was the case with the previous table, the expected type classes are skipped.

| Method | Takes | Returns
| :---: | :---: | :---: |
| `map[B]` | `A => B` | `OptionT[F, B]` |
| `subflatMap[B]` | `A => Option[B]` | `OptionT[F, B]` |
| `semiflatMap[B]` | `A => F[B]` | `OptionT[F, B]` |
| `flatMapF[B]` | `A => F[Option[B]]` | `OptionT[F, B]` |
| `flatMap[B]` | `A => OptionT[F, B]` | `OptionT[F, B]` |

In practice, you're most likely to use `map` and `semiflatMap`.

As is always the case with `flatMap` and `map`, you can use it not only explicitly, but also under the hood in `for` comprehensions, as in the example below:

```scala mdoc:silent

case class User(accountId: Long) { /* ... */ }
class Account { /* ... */ }
class Money { /* ... */ }

def findUserById(userId: Long): Try[Option[User]] = { /* ... */ ??? }

def findAccountById(accountId: Long): Try[Option[Account]] = { /* ... */ ??? }

def getReservedFundsForAccount(account: Account): Try[Option[Money]] = { /* ... */ ??? }

def getReservedFundsForUser(userId: Long): Try[Option[Money]] = (
  for {
    user <- OptionT(findUserById(userId))
    account <- OptionT(findAccountById(user.accountId))
    funds <- OptionT(getReservedFundsForAccount(account))
  } yield funds
).value
```

The `OptionT[Try, Money]` instance returned by `getReservedFundsForUser` will enclose a `None` value if any of the three composed methods returns an `OptionT` corresponding to `None`.
Otherwise, if the result of all three calls contains `Some`, the final outcome will also contain `Some`.

## Pitfalls

In principle, monad transformers can be stacked to create data types with complex behavior.
While powerful, this can lead to unwieldy type signatures and poor type inference. 
The [Cats MTL](https://typelevel.org/cats-mtl/) project provides a capability based approach to extend effect types without requiring as much boilerplate.

Some monad transformers can also interact poorly with more powerful effect types that provide concurrent computation, such as Cats Effect's `IO` or fs2's `Stream`. You should avoid `StateT` and `WriterT` and use the concurrent data types provided by those libraries instead. Because `IO` already provides its own error channel, `EitherT[IO, Throwable]` can also lead to confusing behavior; prefer `IO`'s own error channel instead.

## Recommended Usage

To minimize these pitfalls, monad transformers should only be used as locally-scoped utilities to reduce boilerplate.
Do not include monad transformers in public method signatures. 
Calling code can always use monad transformers themselves.
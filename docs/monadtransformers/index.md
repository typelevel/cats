# Overview

Monad transformers are data types used to simplify the composition of monads. For example:

## OptionT

```scala mdoc:silent
import cats.data.OptionT
```

An instance of [`OptionT[F, A]`](optiont.md) can be thought of as a wrapper over `F[Option[A]]`
which adds a couple of useful methods specific to nested types that aren't available in `F` or `Option` itself.
Most typically, your `F` will be `IO` (or something like [doobie](https://typelevel.org/doobie/index.html)'s `ConnectionIO`).
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
In practice, you should not include monad transformers in function signatures, and only use them as locally-scoped utilities to reduce boilerplate so that calling code is not required to care about the composition.

There are several ways to construct an `OptionT` instance.
The method headers in the table below are slightly simplified: the type parameters and type classes required by each method are skipped.

| Method | Takes | Returns |
| :---: | :---: | :---: |
| `OptionT.apply` or `OptionT(...)` | `F[Option[A]]` | `OptionT[F, A]` |
| `OptionT.fromOption` | `Option[A]` | `OptionT[F, A]` |
| `OptionT.liftF` | `F[A]` | `OptionT[F, A]` |
| `OptionT.pure` | `A` | `OptionT[F, A]` |

In production code you'll most commonly use the `OptionT(...)` syntax in order to wrap an instance of `Try[Option[A]]` into `OptionT[F, A]`.
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


## EitherT

```scala mdoc:silent
import cats.data.EitherT
```

[`EitherT[F, A, B]`](eithert.md) is the monad transformer for `Either` — you can think of it as a wrapper over a `F[Either[A, B]]` value.

Just as in the above section, I simplified the method headers, skipping type parameters or their context bounds and lower bounds.

Let's have a quick look at how to create an `EitherT` instance:

| Method | Takes | Returns |
| :---: | :---: | :---: |
| `EitherT.apply` or `EitherT(...)` | `F[Either[A, B]]` | `EitherT[F, A, B]` |
| `EitherT.fromEither` | `Either[A, B]` | `EitherT[F, A, B]` (wraps the provided `Either` value into `F`) |
| `EitherT.right` or `EitherT.liftF` | `F[B]` | `EitherT[F, A, B]` (wraps value inside `F[B]` into `Right`) |
| `EitherT.left` | `F[A]` | `EitherT[F, A, B]` (wraps value inside `F[B]` into `Left`) |
| `EitherT.pure` | `A` | `EitherT[F, A, B]` (wraps value into `Right` and then into `F`) |

Another useful way to construct an `EitherT` instance is to use `OptionT`'s methods `toLeft` and `toRight`:

```scala mdoc:silent
abstract class BaseException(message: String) extends Exception(message)

case class UserNotFoundException(message: String) extends BaseException(message)

def getUserById(userId: Int): Try[Option[User]] = { /* ... */ ??? }

def ensureUserExists(userId: Int): EitherT[Try, BaseException, User] = {
  OptionT(getUserById(userId))
    .toRight(left = UserNotFoundException(s"user not found, userId=$userId"))
}
```

`toRight` is pretty analogous to the method `Either.fromOption` mentioned before: just as `fromOption` built an `Either` from an `Option`, `toRight` creates an `EitherT` from an `OptionT`.
If the original `OptionT` stores `Some` value, it will be wrapped into `Right`; otherwise the value provided as the `left` parameter will be wrapped into a `Left`.
To provide the `left` value within the monad, there is corresponding `toRightF` method.

`toLeft` is `toRight`'s counterpart which wraps the `Some` value into `Left` and transforms `None` into `Right` enclosing the provided `right` value.
This is less commonly used in practice, but can serve e.g. for enforcing uniqueness checks in code.
We return `Left` if the value has been found, and `Right` if it doesn't yet exist in the system.

The methods available in `EitherT` are pretty similar to those we've seen in `OptionT`, but there are some notable differences.

You might get into some confusion at first when it comes to e.g. `map`.
In the case of `OptionT`, it was pretty obvious what should be done: `map` should go over the `Option` enclosed within `Try`, and then map the enclosed `Option` itself.
This is slightly less obvious in case of `EitherT`: should it map over both `Left` and `Right` values, or only the `Right` value?

The answer is that `EitherT` is _right-biased_, therefore plain `map` actually deals with the `Right` value.

Having said that, let's take a quick look at the right-biased methods that `EitherT` offers:

| Method | Takes | Returns |
| :---: | --- | --- |
| `map[D]` | `B => D` | `EitherT[F, A, D]` |
| `subflatMap[D]` | `B => Either[A, D]` | `EitherT[F, A, D]` |
| `semiflatMap[D]` | `B => F[D]` | `EitherT[F, A, D]` |
| `flatMapF[D]` | `B => F[Either[A, D]]` | `EitherT[F, A, D]` |
| `flatMap[D]` | `B => EitherT[F, A, D]` | `EitherT[F, A, D]` |

As a side note, there are also certain methods in `EitherT` (that you're likely to need at some point) which map over the `Left` value, like `leftMap`, or over both `Left` and `Right` values, like `fold` or `bimap`.

`EitherT` is very useful for fail-fast chained verifications:

```scala mdoc:silent

case class Item(state: String)
class ItemOrder  { /* ... */ }

case class ItemNotFoundException(message: String) extends BaseException(message)
case class InvalidItemStateException(message: String) extends BaseException(message)

def getItemById(itemId: Int): Try[Option[Item]] = { /* .. */ ??? }

def ensureItemExists(itemId: Int): EitherT[Try, BaseException, Item] = {
  OptionT(getItemById(itemId))
    .toRight(ItemNotFoundException(s"item not found, itemId = $itemId"))
}

def ensureItemStateIs(actual: String, expected: String): EitherT[Try, BaseException, Unit] = {
  // Returns a Unit value wrapped into Right and then into Try if condition is true,
  // otherwise the provided exception wrapped into Left and then into Try.
  EitherT.cond(actual == expected, (), InvalidItemStateException(s"actual=$actual, expected=$expected"))
}

def placeOrderForItem(userId: Int, itemId: Int, count: Int): Try[ItemOrder] = { /* ... */ ??? }

def buyItem(userId: Int, itemId: Int, count: Int): EitherT[Try, BaseException, ItemOrder] = {
  for {
    user <- ensureUserExists(userId)
    item <- ensureItemExists(itemId)
    _ <- ensureItemStateIs(item.state, "AVAILABLE_IN_STOCK")
    // EitherT.liftF is necessary to make EitherT[Try, BaseException, ItemOrder] out of Try[ItemOrder]
    placedOrder <- EitherT.liftF(placeOrderForItem(userId, itemId, count))
  } yield placedOrder
}
```

In the above example, we're running various checks against the item one by one.
If any of the checks fails, the resulting `EitherT` will contain a `Left` value.
Otherwise, if all of the checks yield a `Right` (of course we mean a `Right` wrapped into an `EitherT`), then the final outcome will also contain `Right`.
This is a fail-fast behavior: we're effectively stopping the `for` comprehension flow at the first `Left`-ish result.

If you're instead looking for validation that accumulates the errors (e.g. when dealing with user-provided form data), [Validated](../datatypes/validated.md) or the [Parallel](../typeclasses/parallel.md) applicative functor for `Either` may be a good choice.

## Pitfalls

In principle, monad transformers can be stacked to create data types with complex behavior.
While powerful, this can lead to unwieldy type signatures and poor type inference. 
The [Cats MTL](https://typelevel.org/cats-mtl/) project provides a capability based approach to extend effect types without requiring as much boilerplate.

Some monad transformers can also interact poorly with more powerful effect types that provide concurrent computation, such as Cats Effect's `IO` or fs2's `Stream`. You should avoid `StateT` and `WriterT` and use the concurrent data types provided by those libraries instead. Because `IO` already provides its own error channel, `EitherT[IO, Throwable]` can also lead to confusing behavior; prefer `IO`'s own error channel instead.
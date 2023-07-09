{% laika.title = Jump Start Guide %}

# Introduction

This jump start guide barely scratches the surface of what Cats can do, but instead provides a concise hands-on introduction to the patterns you're most likely to take advantage of in your Scala project.
If you're using the constructs like [`Future`](http://www.scala-lang.org/api/current/scala/concurrent/Future.html),
[`Option`](http://www.scala-lang.org/api/current/scala/Option.html) or
[`Either`](http://www.scala-lang.org/api/current/scala/util/Either.html) on a daily basis,
it's very likely that Cats can simplify and improve the readability of your code.

Please refer to the [wiki on GitHub](https://github.com/typelevel/cats) for guidelines on how to add the library to your project dependencies.
We are sticking to [version 1.0.1](https://github.com/typelevel/cats/releases/tag/v1.0.1) in the entire post.

Let's go through the library package-wise, looking at the syntax available in each package.


# Option Helpers

```scala mdoc:silent
import cats.syntax.all._
```

Importing this package enables `obj.some` syntax — equivalent to `Some(obj)`.
The only real difference is that the value is already upcast to `Option[T]` from `Some[T]`.

Using `obj.some` instead of `Some(obj)` can sometimes e.g. improve readability when you need to provide dummy implementation for service methods for the purpose of testing.
For example, if you put the following implicit class into the scope:

```scala mdoc:silent
import scala.concurrent.Future

implicit class ToFutureSuccessful[T](obj: T) {
  def asFuture: Future[T] = Future.successful(obj)
}
```

then you can use the chained syntax shown below:

```scala mdoc:silent
class Account { /* ... */ }

trait AccountService {
  def getAccountById(id: Int): Future[Option[Account]]
}

class DummyAccountServiceImpl extends AccountService {

  def dummyAccount: Account = ???

  /* ... */
  override def getAccountById(id: Int): Future[Option[Account]] = dummyAccount.some.asFuture
  /* ... */
}
```

That's more readable than `Future.successful(Some(dummyAccount))`, especially if this pattern repeats frequently.
Chaining `.some.asFuture` at the end rather than putting it at the front also helps focus on what's actually being returned rather than on the expected wrapper type.

`none[T]`, in turn, is shorthand for `Option.empty[T]` which is just `None`, but already upcast from `None.type` to `Option[T]`.
Providing a more specialized type sometimes helps the Scala compiler properly infer the type of expressions containing `None`.


# Either Helpers

```scala mdoc:silent
import cats.syntax.all._
```

`obj.asRight` is `Right(obj)`, `obj.asLeft` is `Left(obj)`.
In both cases the type of returned value is widened from `Right` or `Left` to `Either`.
Just as was the case with `.some`, these helpers are handy to combine with `.asFuture` to improve readability:

```scala mdoc:silent
case class User(accountId: Long) { /* ... */ }

trait UserService {
  def ensureUserExists(id: Int): Future[Either[Exception, User]]
}

class UserServiceSpec extends UserService {

  def dummyUser: User = ???

  /* ... */
  override def ensureUserExists(id: Int): Future[Either[Exception, User]] = dummyUser.asRight.asFuture // instead of Future.successful(Right(dummyUser))
  /* ... */
}
```

`Either.fromOption(option: Option[A], ifNone: => E)`, in turn, is a useful helper for converting an `Option` to an `Either`.
If the provided `option` is `Some(x)`, it becomes `Right(x)`.
Otherwise it becomes `Left` with the provided `ifNone` value inside.


# Tuples

The `apply` package provides `(..., ..., ...).mapN` syntax, which allows for an intuitive construct for applying a function that takes more than one parameter to multiple effectful values (like futures).

Let's say we have 3 futures, one of type `Int`, one of type `String`, one of type `User` and a method accepting three parameters — `Int`, `String` and `User`.

```scala mdoc:silent
import scala.concurrent.ExecutionContext.Implicits.global

class ProcessingResult { /* ... */ }

def intFuture: Future[Int] = { /* ... */ ??? }
def stringFuture: Future[String] = { /* ... */ ??? }
def userFuture: Future[User] = { /* ... */ ??? }

def process(value: Int, contents: String, user: User): ProcessingResult = { /* ... */ ??? }
```

Our goal is to apply the function to the values computed by those 3 futures.
With `apply` syntax this becomes very easy and concise:

```scala mdoc:silent
import cats.syntax.all._

def processAsync: Future[ProcessingResult] = {
  (intFuture, stringFuture, userFuture).mapN {
    (value, contents, user) =>
      process(value, contents, user)
  }
}
```

By default the implicit instances (namely, [`Functor[Future]`](@API_URL@/cats/Functor.html) and
[`Semigroupal[Future]`](@API_URL@/cats/Semigroupal.html)) required for `mapN` to work properly are always visible. They are present in the respective companion objects of the instances and hence we do not need to import them explicitly.  

This above idea can be expressed even shorter, just:

```scala mdoc:silent
def processAsync2: Future[ProcessingResult] = (intFuture, stringFuture, userFuture).mapN(process)
```

If any of the chained futures fails, the resulting future will also fail with the same exception as the first failing future in the chain (this is fail-fast behavior).

What's important, all futures will run in parallel, as opposed to what would happen in a `for` comprehension:

```scala mdoc:silent
def processAsync3: Future[ProcessingResult] = {
  for {
    value <- intFuture
    contents <- stringFuture
    user <- userFuture
  } yield process(value, contents, user)
}
```

In the above snippet (which under the hood translates to `flatMap` and `map` calls), `stringFuture` will not run until `intFuture` is successfully completed,
and in the same way `userFuture` will be run only after `stringFuture` completes.
But since the computations are independent of one another, it's perfectly viable to run them in parallel with `mapN` instead.



# Traversing

```scala mdoc:silent
import cats.syntax.all._
```

## `traverse`

If you have an instance `obj` of type `F[A]` that can be mapped over (like `Future`) and a function `fun` of type `A => G[B]`, then calling `obj.map(fun)` would give you `F[G[B]]`.
In many common real-life cases, like when `F` is `Option` and `G` is `Future`, you would get `Option[Future[B]]`, which most likely isn't what you wanted.

`traverse` comes as a solution here.
If you call `traverse` instead of `map`, like `obj.traverse(fun)`, you'll get `G[F[A]]`, which will be `Future[Option[B]]` in our case; this is much more useful and easier to process than `Option[Future[B]]`.

```scala mdoc:silent
def updateUser(user: User): Future[User] = { /* ... */ ??? }

def updateUsers(users: List[User]): Future[List[User]] = {
  users.traverse(updateUser)
}
```

As a side note, there is also a dedicated method `Future.traverse` in the [`Future`](http://www.scala-lang.org/api/current/scala/concurrent/Future$.html) companion object,
but the Cats version is far more readable and can easily work on any structure for which certain type classes are available.

## `sequence`

`sequence` represents an even simpler concept: it can be thought of as simply swapping the types from `F[G[A]]` to `G[F[A]]` without even mapping the enclosed value like `traverse` does.

```scala mdoc:silent
val foo: List[Future[String]] = List(Future("hello"), Future("world"))
val bar = foo.sequence // will have Future[List[String]] type
```

`obj.sequence` is in fact implemented in Cats as `obj.traverse(identity)`.

On the other hand, `obj.traverse(fun)` is roughly equivalent to `obj.map(fun).sequence`.

## `flatTraverse`

If you have an `obj` of type `F[A]` and a function `fun` of type `A => G[F[B]]`, then doing `obj.map(f)` yields result of type `F[G[F[B]]]` — very unlikely to be what you wanted.

Traversing the `obj` instead of mapping helps a little — you'll get `G[F[F[B]]` instead.
Since `G` is usually something like `Future` and `F` is `List` or `Option`, you would end up with `Future[Option[Option[A]]` or `Future[List[List[A]]]` — a bit awkward to process.

```scala mdoc:silent
lazy val valueOpt: Option[Int] = { /* ... */ ??? }
def compute(value: Int): Future[Option[Int]] = { /* ... */ ??? }
def computeOverValue: Future[Option[Option[Int]]] = valueOpt.traverse(compute) // has type Future[Option[Option[Int]]], not good
```

The solution could be to map the result with a ```_.flatten``` call like:
```scala mdoc:silent
def computeOverValue2: Future[Option[Int]] = valueOpt.traverse(compute).map(_.flatten)
```
and this way you'll get the desired type `G[F[B]]` at the end.

However, there is a neat shortcut for this called `flatTraverse`:
```scala mdoc:silent
def computeOverValue3: Future[Option[Int]] = valueOpt.flatTraverse(compute)
```
and that solves our problem for good.


# Monad Transformers

## OptionT

```scala mdoc:silent
import cats.data.OptionT
```

An instance of [`OptionT[F, A]`](datatypes/optiont.md) can be thought of as a wrapper over `F[Option[A]]`
which adds a couple of useful methods specific to nested types that aren't available in `F` or `Option` itself.
Most typically, your `F` will be `Future` (or sometimes slick's `DBIO`, but this requires having an implementation of Cats type classes like `Functor` or `Monad` for `DBIO`).
Wrappers such as `OptionT` are generally known as _monad transformers_.

A quite common pattern is mapping the inner value stored inside an instance of `F[Option[A]]` to an instance of `F[Option[B]]` with a function of type `A => B`.
This can be done with rather verbose syntax like:
```scala mdoc:silent
lazy val resultFuture: Future[Option[Int]] = ???

def mappedResultFuture: Future[Option[String]] = resultFuture.map { maybeValue =>
  maybeValue.map { value =>
    // Do something with the value and return String
    ???
  }
}
```

With the use of `OptionT`, this can be simplified as follows:

```scala mdoc:silent
def mappedResultFuture2: OptionT[Future, String] = OptionT(resultFuture).map { value =>
  // Do something with the value and return String
  ???
}
```

The above `map` will return a value of type `OptionT[Future, String]`.

To get the underlying `Future[Option[String]]` value, simply call `.value` on the `OptionT` instance.
It's also a viable solution to fully switch to `OptionT[Future, A]` in method parameter/return types and completely (or almost completely) ditch `Future[Option[A]]` in type declarations.

There are several ways to construct an `OptionT` instance.
The method headers in the table below are slightly simplified: the type parameters and type classes required by each method are skipped.

| Method | Takes | Returns |
| :---: | :---: | :---: |
| `OptionT.apply` or `OptionT(...)` | `F[Option[A]]` | `OptionT[F, A]` |
| `OptionT.fromOption` | `Option[A]` | `OptionT[F, A]` |
| `OptionT.liftF` | `F[A]` | `OptionT[F, A]` |
| `OptionT.pure` | `A` | `OptionT[F, A]` |

In production code you'll most commonly use the `OptionT(...)` syntax in order to wrap an instance of `Future[Option[A]]` into `OptionT[F, A]`.
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
class Money { /* ... */ }

def findUserById(userId: Long): OptionT[Future, User] = { /* ... */ ??? }

def findAccountById(accountId: Long): OptionT[Future, Account] = { /* ... */ ??? }

def getReservedFundsForAccount(account: Account): OptionT[Future, Money] = { /* ... */ ??? }

def getReservedFundsForUser(userId: Long): OptionT[Future, Money] = for {
  user <- findUserById(userId)
  account <- findAccountById(user.accountId)
  funds <- getReservedFundsForAccount(account)
} yield funds
```

The `OptionT[Future, Money]` instance returned by `getReservedFundsForUser` will enclose a `None` value if any of the three composed methods returns an `OptionT` corresponding to `None`.
Otherwise, if the result of all three calls contains `Some`, the final outcome will also contain `Some`.


## EitherT

```scala mdoc:silent
import cats.data.EitherT
```

[`EitherT[F, A, B]`](datatypes/eithert.md) is the monad transformer for `Either` — you can think of it as a wrapper over a `F[Either[A, B]]` value.

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

def getUserById(userId: Int): Future[Option[User]] = { /* ... */ ??? }

def ensureUserExists(userId: Int): EitherT[Future, BaseException, User] = {
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
In the case of `OptionT`, it was pretty obvious what should be done: `map` should go over the `Option` enclosed within `Future`, and then map the enclosed `Option` itself.
This is slightly less obvious in case of `EitherT`: should it map over both `Left` and `Right` values, or only the `Right` value?

The answer is that `EitherT` is _right-biased_, therefore plain `map` actually deals with the `Right` value.
This is unlike `Either` in the Scala standard library up to 2.11, which is in turn _unbiased_: there's no `map` available in `Either`, only for its left and right projections.

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

def getItemById(itemId: Int): Future[Option[Item]] = { /* .. */ ??? }

def ensureItemExists(itemId: Int): EitherT[Future, BaseException, Item] = {
  OptionT(getItemById(itemId))
    .toRight(ItemNotFoundException(s"item not found, itemId = $itemId"))
}

def ensureItemStateIs(actual: String, expected: String): EitherT[Future, BaseException, Unit] = {
  // Returns a Unit value wrapped into Right and then into Future if condition is true,
  // otherwise the provided exception wrapped into Left and then into Future.
  EitherT.cond(actual == expected, (), InvalidItemStateException(s"actual=$actual, expected=$expected"))
}

def placeOrderForItem(userId: Int, itemId: Int, count: Int): Future[ItemOrder] = { /* ... */ ??? }

def buyItem(userId: Int, itemId: Int, count: Int): EitherT[Future, BaseException, ItemOrder] = {
  for {
    user <- ensureUserExists(userId)
    item <- ensureItemExists(itemId)
    _ <- ensureItemStateIs(item.state, "AVAILABLE_IN_STOCK")
    // EitherT.liftF is necessary to make EitherT[Future, BaseException, ItemOrder] out of Future[ItemOrder]
    placedOrder <- EitherT.liftF(placeOrderForItem(userId, itemId, count))
  } yield placedOrder
}
```

In the above example, we're running various checks against the item one by one.
If any of the checks fails, the resulting `EitherT` will contain a `Left` value.
Otherwise, if all of the checks yield a `Right` (of course we mean a `Right` wrapped into an `EitherT`), then the final outcome will also contain `Right`.
This is a fail-fast behavior: we're effectively stopping the `for` comprehension flow at the first `Left`-ish result.

If you're instead looking for validation that accumulates the errors (e.g. when dealing with user-provided form data), `cats.data.Validated` may be a good choice.



# Common issues

The Cats type class instances for standard library types are available in implicit scope and hence no longer have to be imported. If anything doesn't compile as expected, first make sure all the required Cats syntax implicits are in the scope — try importing ```cats.syntax.all._``` and see if the problem persists. The only exception is here is Cat's own ```Order``` and ```PartialOrder``` type classes which are available by importing ```cats.implicits._```. 
As mentioned before, though, it's better to use narrow imports, but if the code doesn't compile it's sometimes worth just importing the entire library to check if it solves the problem.

If you're using `Futures`, make sure to provide an implicit `ExecutionContext` in the scope, otherwise Cats won't be able to infer implicit instances for `Future`'s type classes.

IntelliJ sometimes reports errors in Cats-loaded code even though the source passes under scalac.
One such example are invocations of the methods of `cats.data.Nested` class, which compile correctly under scalac, but don't type check under IntelliJ's presentation compiler.
It should work without trouble under [Scala IDE](http://scala-ide.org/), though.

As an advice for your future learning: the `Applicative` type class, despite it's key significance in functional programming, comes slightly difficult to understand.
In my opinion it's much less intuitive than `Functor` or `Monad`, even though it actually stands right between `Functor` and `Monad` in the inheritance hierarchy.
The best approach to grasp `Applicative` is to first understand how `product` (which transforms an `F[A]` and `F[B]` to an `F[(A, B)]`) works rather than focus on the somewhat exotic `ap` operation itself.

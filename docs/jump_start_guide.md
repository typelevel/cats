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

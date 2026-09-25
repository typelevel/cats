# Nested

API Documentation: @:api(cats.data.Nested)

## Motivation

In day-to-day programming we quite often end up with data inside nested
effects, e.g. an integer inside an `Either`, which in turn is nested inside
an `Option`:

```scala mdoc:silent
import cats.data.Validated
import cats.data.Validated.Valid
val x: Option[Validated[String, Int]] = Some(Valid(123))
```

This can be quite annoying to work with, as you have to traverse the nested
structure every time you want to perform a `map` or something similar:

```scala mdoc
x.map(_.map(_.toString))
```

`Nested` can help with this by composing the two `map` operations into one:

```scala mdoc:silent
import cats.data.Nested
import cats.syntax.all._
val nested: Nested[Option, Validated[String, *], Int] = Nested(Some(Valid(123)))
```

```scala mdoc
nested.map(_.toString).value
```

In a sense, `Nested` is similar to monad transformers like `OptionT` and
`EitherT`, as it represents the nesting of effects inside each other. But
`Nested` is more general - it does not place any restriction on the type of the
two nested effects:

```scala
final case class Nested[F[_], G[_], A](value: F[G[A]])
```

Instead, it provides a set of inference rules based on the properties of `F[_]`
and `G[_]`. For example:

* If `F[_]` and `G[_]` are both `Functor`s, then `Nested[F, G, *]` is also a
    `Functor` (we saw this in action in the example above)
* If `F[_]` and `G[_]` are both `Applicative`s, then `Nested[F, G, *]` is also an
    `Applicative`
* If `F[_]` is an `ApplicativeError` and `G[_]` is an `Applicative`, then
    `Nested[F, G, *]` is an `ApplicativeError`
* If `F[_]` and `G[_]` are both `Traverse`s, then `Nested[F, G, *]` is also a
    `Traverse`

You can see the full list of these rules in the `Nested` companion object.

### A more interesting example

(courtesy of [Channing Walton and Luka Jacobowitz via
Twitter](https://twitter.com/LukaJacobowitz/status/1017355319473786880),
slightly adapted)

Say we have an API for creating users:

```scala mdoc:silent
import scala.concurrent.Future

case class UserInfo(name: String, age: Int)
case class User(id: String, name: String, age: Int)

def createUser(userInfo: UserInfo): Future[Either[List[String], User]] =
  Future.successful(Right(User("user 123", userInfo.name, userInfo.age)))
```

Using `Nested` we can write a function that, given a list of `UserInfo`s,
creates a list of `User`s:

```scala mdoc:silent
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.Applicative
import cats.data.Nested
import cats.syntax.all._

def createUsers(userInfos: List[UserInfo]): Future[Either[List[String], List[User]]] =
  userInfos.traverse(userInfo => Nested(createUser(userInfo))).value

val userInfos = List(
  UserInfo("Alice", 42),
  UserInfo("Bob", 99)
)
```

```scala mdoc
Await.result(createUsers(userInfos), 1.second)
```

Note that if we hadn't used `Nested`, the behaviour of our function would have
been different, resulting in a different return type:

```scala mdoc:silent
def createUsersNotNested(userInfos: List[UserInfo]): Future[List[Either[List[String], User]]] =
  userInfos.traverse(createUser)
```

```scala mdoc
Await.result(createUsersNotNested(userInfos), 1.second)
```

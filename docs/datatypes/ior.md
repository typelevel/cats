# Ior

API Documentation: @:api(cats.data.Ior)

`Ior` represents an inclusive-or relationship between two data types.
This makes it very similar to the [`Either`](either.md) data type, which represents an "exclusive-or" relationship.
What this means, is that an `Ior[A, B]` (also written as `A Ior B`) can contain either an `A`, a `B`, or both an `A` and `B`.
Another similarity to `Either` is that `Ior` is right-biased,
which means that the `map` and `flatMap` functions will work on the right side of the `Ior`, in our case the `B` value.
You can see this in the function signature of `map`:

```scala
def map[B, C](fa: A Ior B)(f: B => C): A Ior C
```

We can create `Ior` values using `Ior.left`, `Ior.right` and `Ior.both`:

```scala mdoc
import cats.data._

val right = Ior.right[String, Int](3)

val left = Ior.left[String, Int]("Error")

val both = Ior.both("Warning", 3)
```

Cats also offers syntax enrichment for `Ior`. The `leftIor` and `rightIor` functions can be imported from `cats.syntax.ior._`:

```scala mdoc:nest
import cats.syntax.all._

val right = 3.rightIor

val left = "Error".leftIor
```


When we look at the `Monad` or `Applicative` instances of `Ior`, we can see that they actually requires a `Semigroup` instance on the left side.
This is because `Ior` will actually accumulate failures on the left side, very similar to how the [`Validated`](validated.md) data type does.
This means we can accumulate data on the left side while also being able to short-circuit upon the first left-side-only value.
For example, sometimes, we might want to accumulate warnings together with a valid result and only halt the computation on a "hard error"
Here's an example of how we might be able to do that:

```scala mdoc:reset-object:silent
import cats.syntax.all._
import cats.data.{ NonEmptyChain => Nec, Ior}


type Failures = Nec[String]

case class Username(value: String) extends AnyVal
case class Password(value: String) extends AnyVal

case class User(name: Username, pw: Password)

def validateUsername(u: String): Failures Ior Username = {
  if (u.isEmpty)
    Nec.one("Can't be empty").leftIor
  else if (u.contains("."))
    Ior.both(Nec.one("Dot in name is deprecated"), Username(u))
  else
    Username(u).rightIor
}

def validatePassword(p: String): Failures Ior Password = {
  if (p.length < 8)
    Nec.one("Password too short").leftIor
  else if (p.length < 10)
    Ior.both(Nec.one("Password should be longer"), Password(p))
  else
    Password(p).rightIor
}

def validateUser(name: String, password: String): Failures Ior User =
  (validateUsername(name), validatePassword(password)).mapN(User)

```

Now we're able to validate user data and also accumulate non-fatal warnings:

```scala mdoc

validateUser("John", "password12")

validateUser("john.doe", "password")

validateUser("jane", "short")

```

To extract the values, we can use the `fold` method, which expects a function for each case the `Ior` can represent:

```scala mdoc

validateUser("john.doe", "password").fold(
  errorNec => s"Error: ${errorNec.head}",
  user => s"Success: $user",
  (warnings, user) => s"Warning: ${user.name.value}; The following warnings occurred: ${warnings.show}"
)

```

## Using with NonEmptyChain

Similar to [Validated](validated.md), there is also a type alias for using a `NonEmptyChain` on the left side.

```scala mdoc:silent
import cats.data.NonEmptyChain

type IorNec[B, A] = Ior[NonEmptyChain[B], A]
```


```scala mdoc:nest
import cats.syntax.all._, cats.data.NonEmptyChain

val left: IorNec[String, Int] = Ior.fromEither("Error".leftNec[Int])

```

## Conversions

We can also convert our `Ior` to `Either`, `Validated` or `Option`.
All of these conversions will discard the left side value if both are available:

```scala mdoc
Ior.both("Warning", 42).toEither
```

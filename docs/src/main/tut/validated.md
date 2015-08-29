---
layout: default
title:  "Validated"
section: "data"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/data/Validated.scala"
scaladoc: "#cats.data.Validated"
---
# Validated

Imagine you are filling out a web form to signup for an account. You input your username and password and submit.
Response comes back saying your username can't have dashes in it, so you make some changes and resubmit. Can't
have special characters either. Change, resubmit. Passwords need to have at least one capital letter. Change,
resubmit. Password needs to have at least one number.

Or perhaps you're reading from a configuration file. One could imagine the configuration library you're using returns
a `scala.util.Try`, or maybe a `scala.util.Either` (or `cats.data.Xor`). Your parsing may look something like:

```scala
for {
  url  <- config[String]("url")
  port <- config[Int]("port")
} yield ConnectionParams(url, port)
```

You run your program and it says key "url" not found, turns out the key was "endpoint". So you change your code
and re-run. Now it says the "port" key was not a well-formed integer.

It would be nice to have all of these errors be reported simultaneously. That the username can't have dashes can
be validated separately from it not having special characters, as well as from the password needing to have certain
requirements. A misspelled (or missing) field in a config can be validated separately from another field not being
well-formed.

Enter `Validated`.

## Parallel validation
Our goal is to report any and all errors across independent bits of data. For instance, when we ask for several
pieces of configuration, each configuration field can be validated separately from one another. How then do we
enforce that the data we are working with is independent? We ask for both of them up front.

As our running example, we will look at config parsing. Our config will be represented by a
`Map[String, String]`. Parsing will be handled by a `Read` type class - we provide instances
just for `String` and `Int` for brevity.

```tut
trait Read[A] {
  def read(s: String): Option[A]
}

object Read {
  def apply[A](implicit A: Read[A]): Read[A] = A

  implicit val stringRead: Read[String] =
    new Read[String] { def read(s: String): Option[String] = Some(s) }

  implicit val intRead: Read[Int] =
    new Read[Int] {
      def read(s: String): Option[Int] =
        if (s.matches("-?[0-9]+")) Some(s.toInt)
        else None
    }
}
```

Then we enumerate our errors - when asking for a config value, one of two things can
go wrong: the field is missing, or it is not well-formed with regards to the expected
type.

```tut
sealed abstract class ConfigError
final case class MissingConfig(field: String) extends ConfigError
final case class ParseError(field: String) extends ConfigError
```

We need a data type that can represent either a successful value (a parsed configuration),
or an error.

```scala
sealed abstract class Validated[+E, +A]

object Validated {
  final case class Valid[+A](a: A) extends Validated[Nothing, A]
  final case class Invalid[+E](e: E) extends Validated[E, Nothing]
}
```

Now we are ready to write our parser.

```tut
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

case class Config(map: Map[String, String]) {
  def parse[A : Read](key: String): Validated[ConfigError, A] =
    map.get(key) match {
      case None        => Invalid(MissingConfig(key))
      case Some(value) =>
        Read[A].read(value) match {
          case None    => Invalid(ParseError(key))
          case Some(a) => Valid(a)
        }
    }
}
```

Everything is in place to write the parallel validator. Recall that we can only do parallel
validation if each piece is independent. How do we enforce the data is independent? By asking
for all of it up front. Let's start with two pieces of data.

```tut
def parallelValidate[E, A, B, C](v1: Validated[E, A], v2: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
  (v1, v2) match {
    case (Valid(a), Valid(b))       => Valid(f(a, b))
    case (Valid(_), i@Invalid(_))   => i
    case (i@Invalid(_), Valid(_))   => i
    case (Invalid(e1), Invalid(e2)) => ???
  }
```

We've run into a problem. In the case where both have errors, we want to report both. But we have
no way of combining the two errors into one error! Perhaps we can put both errors into a `List`,
but that seems needlessly specific - clients may want to define their own way of combining errors.

How then do we abstract over a binary operation? The `Semigroup` type class captures this idea.

```tut
import cats.Semigroup

def parallelValidate[E : Semigroup, A, B, C](v1: Validated[E, A], v2: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
  (v1, v2) match {
    case (Valid(a), Valid(b))       => Valid(f(a, b))
    case (Valid(_), i@Invalid(_))   => i
    case (i@Invalid(_), Valid(_))   => i
    case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
  }
```

Perfect! But.. going back to our example, we don't have a way to combine `ConfigError`s. But as clients,
we can change our `Validated` values where the error can be combined, say, a `List[ConfigError]`. It is
more common however to use a `NonEmptyList[ConfigError]` - the `NonEmptyList` statically guarantees we
have at least one value, which aligns with the fact that if we have an `Invalid`, then we most
certainly have at least one error. This technique is so common there is a convenient method on `Validated`
called `toValidatedNel` that turns any `Validated[E, A]` value to a `Validated[NonEmptyList[E], A]`.
Additionally, the type alias `ValidatedNel[E, A]` is provided.

Time to parse.

```tut
import cats.SemigroupK
import cats.data.NonEmptyList
import cats.std.list._

case class ConnectionParams(url: String, port: Int)

val config = Config(Map(("endpoint", "127.0.0.1"), ("port", "not an int")))

implicit val nelSemigroup: Semigroup[NonEmptyList[ConfigError]] =
  SemigroupK[NonEmptyList].algebra[ConfigError]

implicit val readString: Read[String] = Read.stringRead
implicit val readInt: Read[Int] = Read.intRead

val v1 = parallelValidate(config.parse[String]("url").toValidatedNel,
                          config.parse[Int]("port").toValidatedNel)(ConnectionParams.apply)

val v2 = parallelValidate(config.parse[String]("endpoint").toValidatedNel,
                          config.parse[Int]("port").toValidatedNel)(ConnectionParams.apply)

val config = Config(Map(("endpoint", "127.0.0.1"), ("port", "1234")))
val v3 = parallelValidate(config.parse[String]("endpoint").toValidatedNel,
                          config.parse[Int]("port").toValidatedNel)(ConnectionParams.apply)
```

Any and all errors are reported!

## Apply
Our `parallelValidate` function looks awfully like the `Apply#map2` function.

```scala
def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
```

Which can be defined in terms of `Apply#ap` and `Apply#map`, the very functions needed to create an `Apply` instance.

Can we perhaps define an `Apply` instance for `Validated`? Better yet, can we define an `Applicative` instance?

```tut
import cats.Applicative

implicit def validatedApplicative[E : Semigroup]: Applicative[Validated[E, ?]] =
  new Applicative[Validated[E, ?]] {
    def ap[A, B](fa: Validated[E, A])(f: Validated[E, A => B]): Validated[E, B] =
      (fa, f) match {
        case (Valid(a), Valid(fab)) => Valid(fab(a))
        case (i@Invalid(_), Valid(_)) => i
        case (Valid(_), i@Invalid(_)) => i
        case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
      }

    def pure[A](x: A): Validated[E, A] = Validated.valid(x)
  }
```

Awesome! And now we also get access to all the goodness of `Applicative`, among which include
`map{2-22}`, as well as the `Apply` syntax `|@|`.

We can now easily ask for several bits of configuration and get any and all errors returned back.

```tut
import cats.Apply
import cats.data.ValidatedNel

implicit val nelSemigroup: Semigroup[NonEmptyList[ConfigError]] =
  SemigroupK[NonEmptyList].algebra[ConfigError]

val config = Config(Map(("name", "cat"), ("age", "not a number"), ("houseNumber", "1234"), ("lane", "feline street")))

case class Address(houseNumber: Int, street: String)
case class Person(name: String, age: Int, address: Address)

val personFromConfig: ValidatedNel[ConfigError, Person] =
  Apply[ValidatedNel[ConfigError, ?]].map4(config.parse[String]("name").toValidatedNel,
                                           config.parse[Int]("age").toValidatedNel,
                                           config.parse[Int]("house_number").toValidatedNel,
                                           config.parse[String]("street").toValidatedNel) {
    case (name, age, houseNumber, street) => Person(name, age, Address(houseNumber, street))
  }
```

## Of `flatMap`s and `Xor`s
`Option` has `flatMap`, `Xor` has `flatMap`, where's `Validated`'s? Let's try to implement it - better yet,
let's implement the `Monad` type class.

```tut
import cats.Monad

implicit def validatedMonad[E]: Monad[Validated[E, ?]] =
  new Monad[Validated[E, ?]] {
    def flatMap[A, B](fa: Validated[E, A])(f: A => Validated[E, B]): Validated[E, B] =
      fa match {
        case Valid(a)     => f(a)
        case i@Invalid(_) => i
      }

    def pure[A](x: A): Validated[E, A] = Valid(x)
  }
```

Note that all `Monad` instances are also `Applicative` instances, where `ap` is defined as

```tut
trait Monad[F[_]] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def pure[A](x: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(f.andThen(pure))

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B] =
    flatMap(fa)(a => map(f)(fab => fab(a)))
}
```

However, the `ap` behavior defined in terms of `flatMap` does not behave the same as that of
our `ap` defined above. Observe:

```tut
val v = validatedMonad.tuple2(Validated.invalidNel[String, Int]("oops"), Validated.invalidNel[String, Double]("uh oh"))
```

This one short circuits! Therefore, if we were to define a `Monad` (or `FlatMap`) instance for `Validated` we would
have to override `ap` to get the behavior we want. But then the behavior of `flatMap` would be inconsistent with
that of `ap`, not good. Therefore, `Validated` has only an `Applicative` instance.

For very much the same reasons, despite the shape of `Validated` matching exactly that of `Xor`, we have
two separate data types due to their different natures.

### The nature of `flatMap`
Another reason we would be hesitant to define a `flatMap` method on `Validated` lies in the nature of `flatMap`.

```scala
def flatMap[F[_], A, B](fa: F[A])(f: A => F[B]): F[B]
```

Note we have an `F[A]`, but we can only ever get an `F[B]` upon successful inspection of the `A`. This implies a
dependency that `F[B]` has on `F[A]`, which is in conflict with the nature of `Validated`.

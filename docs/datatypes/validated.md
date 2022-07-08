# Validated

Imagine you are filling out a web form to signup for an account. You input your username and password and submit.
Response comes back saying your username can't have dashes in it, so you make some changes and resubmit. Can't
have special characters either. Change, resubmit. Passwords need to have at least one capital letter. Change,
resubmit. Password needs to have at least one number.

It would be nice to have all of these errors be reported simultaneously. That the username can't have dashes can
be validated separately from it not having special characters, as well as from the password needing to have certain
requirements. A misspelled (or missing) field in a config can be validated separately from another field not being
well-formed.

Enter `Validated`.

## A first approach

You'll note firsthand that `Validated` is very similar to `Either` because it also has two possible values: errors on the left side or successful computations on the right side.

Signature of the structure is as follows:

```scala
sealed abstract class Validated[+E, +A] extends Product with Serializable {
  // Implementation elided
}
```

And its _projections_:

```scala
final case class Valid[+A](a: A) extends Validated[Nothing, A]
final case class Invalid[+E](e: E) extends Validated[E, Nothing]
```

Before diving into `Validated`, let's take a look at an `Either` based first approach to address our validation necessity.

Our data will be represented this way:

```scala mdoc:silent
final case class RegistrationData(username: String, password: String, firstName: String, lastName: String, age: Int)
```

And our error model:

```scala mdoc:silent
sealed trait DomainValidation {
  def errorMessage: String
}

case object UsernameHasSpecialCharacters extends DomainValidation {
  def errorMessage: String = "Username cannot contain special characters."
}

case object PasswordDoesNotMeetCriteria extends DomainValidation {
  def errorMessage: String = "Password must be at least 10 characters long, including an uppercase and a lowercase letter, one number and one special character."
}

case object FirstNameHasSpecialCharacters extends DomainValidation {
  def errorMessage: String = "First name cannot contain spaces, numbers or special characters."
}

case object LastNameHasSpecialCharacters extends DomainValidation {
  def errorMessage: String = "Last name cannot contain spaces, numbers or special characters."
}

case object AgeIsInvalid extends DomainValidation {
  def errorMessage: String = "You must be aged 18 and not older than 75 to use our services."
}
```

We have our `RegistrationData` case class that will hold the information the user has submitted, alongside the definition of the error model that we'll be using for displaying the possible errors of every field. Now, let's explore the proposed implementation:

```scala mdoc:silent
import cats.implicits._

sealed trait FormValidator {
 def validateUserName(userName: String): Either[DomainValidation, String] =
    Either.cond(
      userName.matches("^[a-zA-Z0-9]+$"),
      userName,
      UsernameHasSpecialCharacters
    )

 def validatePassword(password: String): Either[DomainValidation, String] =
    Either.cond(
      password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$"),
      password,
      PasswordDoesNotMeetCriteria
    )

 def validateFirstName(firstName: String): Either[DomainValidation, String] =
    Either.cond(
      firstName.matches("^[a-zA-Z]+$"),
      firstName,
      FirstNameHasSpecialCharacters
    )

 def validateLastName(lastName: String): Either[DomainValidation, String] =
    Either.cond(
      lastName.matches("^[a-zA-Z]+$"),
      lastName,
      LastNameHasSpecialCharacters
    )

 def validateAge(age: Int): Either[DomainValidation, Int] =
    Either.cond(
      age >= 18 && age <= 75,
      age,
      AgeIsInvalid
    )

  def validateForm(username: String, password: String, firstName: String, lastName: String, age: Int): Either[DomainValidation, RegistrationData] = {

    for {
      validatedUserName <- validateUserName(username)
      validatedPassword <- validatePassword(password)
      validatedFirstName <- validateFirstName(firstName)
      validatedLastName <- validateLastName(lastName)
      validatedAge <- validateAge(age)
    } yield RegistrationData(validatedUserName, validatedPassword, validatedFirstName, validatedLastName, validatedAge)
  }

}

object FormValidator extends FormValidator

```

The logic of the validation process is as follows: **check every individual field based on the established rules for each one of them. If the validation is successful, then return the field wrapped in a `Right` instance; If not, then return a `DomainValidation` with the respective message, wrapped in a `Left` instance**.
Note that we took advantage of the `.cond` method of `Either`, that is equivalent to do `if (cond) Right(value) else Left(error)`.

Our service has the `validateForm` method for checking all the fields and, if the process succeeds it will create an instance of `RegistrationData`, right?

Well, yes, but the error reporting part will have the downside of showing only the first error.

Let's look this in detail:

```scala mdoc:silent:fail
for {
  validatedUserName <- validateUserName(username)
  validatedPassword <- validatePassword(password)
  validatedFirstName <- validateFirstName(firstName)
  validatedLastName <- validateLastName(lastName)
  validatedAge <- validateAge(age)
} yield RegistrationData(validatedUserName, validatedPassword, validatedFirstName, validatedLastName, validatedAge)
```

A for-comprehension is _fail-fast_. If some of the evaluations in the `for` block fails for some reason, the `yield` statement will not complete. In our case, if that happens we won't be getting the accumulated list of errors.

If we run our code:

```scala mdoc
FormValidator.validateForm(
  username = "fakeUs3rname",
  password = "password",
  firstName = "John",
  lastName = "Doe",
  age = 15
)
```

We should have gotten another `DomainValidation` object denoting the invalid age.

### An iteration with `Validated`

Time to do some refactoring! We're going to try a `Validated` approach:

```scala mdoc:silent
import cats.data._
import cats.data.Validated._
import cats.implicits._

def validateUserName(userName: String): Validated[DomainValidation, String] = FormValidator.validateUserName(userName).toValidated

def validatePassword(password: String): Validated[DomainValidation, String] = FormValidator.validatePassword(password).toValidated

def validateFirstName(firstName: String): Validated[DomainValidation, String] = FormValidator.validateFirstName(firstName).toValidated

def validateLastName(lastName: String): Validated[DomainValidation, String] = FormValidator.validateLastName(lastName).toValidated

def validateAge(age: Int): Validated[DomainValidation, Int] = FormValidator.validateAge(age).toValidated
```
```scala mdoc:fail
def validateForm(username: String, password: String, firstName: String, lastName: String, age: Int): Validated[DomainValidation, RegistrationData] = {
  for {
    validatedUserName <- validateUserName(username)
    validatedPassword <- validatePassword(password)
    validatedFirstName <- validateFirstName(firstName)
    validatedLastName <- validateLastName(lastName)
    validatedAge <- validateAge(age)
  } yield RegistrationData(validatedUserName, validatedPassword, validatedFirstName, validatedLastName, validatedAge)
}
```

What we've done here was to reuse the previously created validation functions and convert their output to a `Validated` instance with the `.toValidated` combinator. This one takes an `Either` and converts it to its equivalent `Validated`.
This datatype, as with `Either` has two projections: `Valid` and `Invalid`, analogous to `Right` and `Left`, respectively.

Remember that our goal is to get all the validation errors for displaying it to the user, but you'll find that this approach won't compile, as you can see in the previous snippet. Why?

Without diving into details about monads, a for-comprehension uses the `flatMap` method for composition. Monads like `Either` can be composed in that way, but the thing with `Validated` is that it isn't a monad, but an [_Applicative Functor_](../typeclasses/applicativetraverse.md).
That's why you see the message: `error: value flatMap is not a member of cats.data.Validated[DomainValidation,String]`.

So, how do we do here?

### Meeting applicative

We have to look into another direction: a for-comprehension plays well in a fail-fast scenario, but the structure in our previous example was designed to catch one error at a time, so, our next step is to tweak the implementation a bit.

```scala mdoc:silent
sealed trait FormValidatorNec {

  type ValidationResult[A] = ValidatedNec[DomainValidation, A]

  private def validateUserName(userName: String): ValidationResult[String] =
    if (userName.matches("^[a-zA-Z0-9]+$")) userName.validNec else UsernameHasSpecialCharacters.invalidNec

  private def validatePassword(password: String): ValidationResult[String] =
    if (password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$")) password.validNec
    else PasswordDoesNotMeetCriteria.invalidNec

  private def validateFirstName(firstName: String): ValidationResult[String] =
    if (firstName.matches("^[a-zA-Z]+$")) firstName.validNec else FirstNameHasSpecialCharacters.invalidNec

  private def validateLastName(lastName: String): ValidationResult[String] =
    if (lastName.matches("^[a-zA-Z]+$")) lastName.validNec else LastNameHasSpecialCharacters.invalidNec

  private def validateAge(age: Int): ValidationResult[Int] =
    if (age >= 18 && age <= 75) age.validNec else AgeIsInvalid.invalidNec

  def validateForm(username: String, password: String, firstName: String, lastName: String, age: Int): ValidationResult[RegistrationData] = {
    (validateUserName(username),
    validatePassword(password),
    validateFirstName(firstName),
    validateLastName(lastName),
    validateAge(age)).mapN(RegistrationData)
  }

}

object FormValidatorNec extends FormValidatorNec
```

Let's see what changed here:

1. In this new implementation, we're using a [NonEmptyChain](https://github.com/typelevel/cats/blob/main/core/src/main/scala/cats/data/NonEmptyChain.scala), a data structure that guarantees that at least one element will be present. In case that multiple errors arise, you'll get a chain of `DomainValidation`.
2. `ValidatedNec[DomainValidation, A]` is an alias for `Validated[NonEmptyChain[DomainValidation], A]`. When you use `ValidatedNec` you're stating that your accumulative structure will be a `NonEmptyChain`. With `Validated`, you have the choice about which data structure you want for reporting the errors (more on that soon).
3. We've declared the type alias `ValidationResult` that conveniently expresses the return type of our validation.
4. `.validNec` and `.invalidNec` combinators lets you _lift_ the success or failure in their respective container (either a `Valid` or `Invalid[NonEmptyChain[A]]`).
5. The [applicative](../typeclasses/applicative.md) syntax `(a, b, c, ...).mapN(...)` provides us a way to accumulatively apply the validation functions and yield a product with their successful result or the accumulated errors in the `NonEmptyChain`. Then, we transform that product with `mapN` into a valid instance of `RegistrationData`.

**Deprecation notice:** since Cats `1.0.0-MF` the cartesian syntax `|@|` for applicatives is deprecated. If you're using `0.9.0` or less, you can use the syntax: `(a |@| b |@| ...).map(...)`.

Note that, at the end, we expect to lift the result of the validation functions in a `RegistrationData` instance. If the process fails, we'll get our `NonEmptyChain` detailing what went wrong.

For example:

```scala mdoc
FormValidatorNec.validateForm(
  username = "Joe",
  password = "Passw0r$1234",
  firstName = "John",
  lastName = "Doe",
  age = 21
)

FormValidatorNec.validateForm(
  username = "Joe%%%",
  password = "password",
  firstName = "John",
  lastName = "Doe",
  age = 21
)
```

Sweet success! Now you can take your validation process to the next level!

### A short detour

As previously stated, `ValidatedNec[DomainValidation, A]` is an alias for `Validated[NonEmptyChain[DomainValidation], A]`. Typically, you'll see that `Validated` is accompanied by a `NonEmptyChain` when it comes to accumulation. The thing here is that you can define your own accumulative data structure and you're not limited to the aforementioned construction.

For doing this, you have to provide a `Semigroup` instance. `NonEmptyChain`, by definition has its own `Semigroup`. For those who don't know what a `Semigroup` is, you can find out more [here](../typeclasses/semigroup.md).

#### Accumulative Structures

Let's take a look about how a `Semigroup` works in a `NonEmptyChain`:

```scala mdoc
NonEmptyChain.one[DomainValidation](UsernameHasSpecialCharacters) |+| NonEmptyChain[DomainValidation](FirstNameHasSpecialCharacters, LastNameHasSpecialCharacters)
```

We're combining a couple of `NonEmptyChain`s. The first one has its mandatory element (note that we've built an instance of it with `.one`) and the second has a couple of elements. As you can see, the output of the combination, expressed by the `|+|` operator is another `NonEmptyChain` with the three elements.

But, what about if we want _another_ way of combining? We can provide our custom `Semigroup` instance with the desired combining logic and pass it implicitly to your scope.

### Going back and forth

Cats offers you a nice set of combinators for transforming your `Validated` based approach to an `Either` one and vice-versa.
We've used `.toValidated` in our second example, now let's see how to use `.toEither`.

#### From `Validated` to `Either`

To do this, simply use `.toEither` combinator:

```scala mdoc
// Successful case
FormValidatorNec.validateForm(
  username = "Joe",
  password = "Passw0r$1234",
  firstName = "John",
  lastName = "Doe",
  age = 21
).toEither

// Invalid case
FormValidatorNec.validateForm(
  username = "Joe123#",
  password = "password",
  firstName = "John",
  lastName = "Doe",
  age = 5
).toEither
```

With this conversion, as you can see, we got an `Either` with a `NonEmptyChain` detailing the possible validation errors or our `RegistrationData` object.

## Another case

Perhaps you're reading from a configuration file. One could imagine the configuration library you're using returns
a `scala.util.Try`, or maybe a `scala.util.Either`. Your parsing may look something like:

```scala
for {
  url  <- config[String]("url")
  port <- config[Int]("port")
} yield ConnectionParams(url, port)
```

You run your program and it says key "url" not found, turns out the key was "endpoint". So you change your code
and re-run. Now it says the "port" key was not a well-formed integer.



## Parallel validation
Our goal is to report any and all errors across independent bits of data. For instance, when we ask for several
pieces of configuration, each configuration field can be validated separately from one another. How then do we
enforce that the data we are working with is independent? We ask for both of them up front.

As our running example, we will look at config parsing. Our config will be represented by a
`Map[String, String]`. Parsing will be handled by a `Read` type class - we provide instances
just for `String` and `Int` for brevity.

```scala mdoc:silent:reset
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

```scala mdoc:silent
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

```scala mdoc:silent
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

```scala mdoc:silent
def parallelValidateSimple[E, A, B, C](v1: Validated[E, A], v2: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
  (v1, v2) match {
    case (Valid(a), Valid(b))       => Valid(f(a, b))
    case (Valid(_), i@Invalid(_))   => i
    case (i@Invalid(_), Valid(_))   => i
    case (Invalid(e1), Invalid(e2)) => ???
  }
```

We've run into a problem. In the case where both have errors, we want to report both. But we have
no way of combining the two errors into one error! Perhaps we can put both errors into a `Chain`,
but that seems needlessly specific - clients may want to define their own way of combining errors.

How then do we abstract over a binary operation? The `Semigroup` type class captures this idea.

```scala mdoc:silent
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
we can change our `Validated` values where the error can be combined, say, a `Chain[ConfigError]`. It is
more common however to use a `NonEmptyChain[ConfigError]` - the `NonEmptyChain` statically guarantees we
have at least one value, which aligns with the fact that if we have an `Invalid`, then we most
certainly have at least one error. This technique is so common there is a convenient method on `Validated`
called `toValidatedNec` that turns any `Validated[E, A]` value to a `Validated[NonEmptyChain[E], A]`.
Additionally, the type alias `ValidatedNec[E, A]` is provided.

Time to parse.

```scala mdoc:silent:nest
import cats.SemigroupK
import cats.data.NonEmptyChain
import cats.implicits._

case class ConnectionParams(url: String, port: Int)

val config = Config(Map(("endpoint", "127.0.0.1"), ("port", "not an int")))

implicit val necSemigroup: Semigroup[NonEmptyChain[ConfigError]] =
  SemigroupK[NonEmptyChain].algebra[ConfigError]

implicit val readString: Read[String] = Read.stringRead
implicit val readInt: Read[Int] = Read.intRead
```

Any and all errors are reported!

```scala mdoc:nest
val v1 = parallelValidate(config.parse[String]("url").toValidatedNec,
                          config.parse[Int]("port").toValidatedNec)(ConnectionParams.apply)

val v2 = parallelValidate(config.parse[String]("endpoint").toValidatedNec,
                          config.parse[Int]("port").toValidatedNec)(ConnectionParams.apply)
```

```scala mdoc:nest
val config = Config(Map(("endpoint", "127.0.0.1"), ("port", "1234")))
val v3 = parallelValidate(config.parse[String]("endpoint").toValidatedNec,
                          config.parse[Int]("port").toValidatedNec)(ConnectionParams.apply)
```

## Apply
Our `parallelValidate` function looks awfully like the `Apply#map2` function.

```scala
def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
```

Which can be defined in terms of `Apply#ap` and `Apply#map`, the very functions needed to create an `Apply` instance.

Can we perhaps define an `Apply` instance for `Validated`? Better yet, can we define an `Applicative` instance?

*Note*: the example below assumes usage of the [kind-projector compiler plugin](https://github.com/typelevel/kind-projector) and will not compile if it is not being used in a project.

```scala mdoc:silent
import cats.Applicative

implicit def validatedApplicative[E : Semigroup]: Applicative[Validated[E, *]] =
  new Applicative[Validated[E, *]] {
    def ap[A, B](f: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
      (fa, f) match {
        case (Valid(a), Valid(fab)) => Valid(fab(a))
        case (i@Invalid(_), Valid(_)) => i
        case (Valid(_), i@Invalid(_)) => i
        case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
      }

    def pure[A](x: A): Validated[E, A] = Validated.valid(x)
  }
```

Awesome! And now we also get access to all the goodness of `Applicative`, which includes `map{2-22}`, as well as the
`Semigroupal` tuple syntax.

We can now easily ask for several bits of configuration and get any and all errors returned back.

```scala mdoc:silent:nest
import cats.Apply
import cats.data.ValidatedNec

implicit def necSemigroup: Semigroup[NonEmptyChain[ConfigError]] =
  SemigroupK[NonEmptyChain].algebra[ConfigError]

val personConfig = Config(Map(("name", "cat"), ("age", "not a number"), ("houseNumber", "1234"), ("lane", "feline street")))

case class Address(houseNumber: Int, street: String)
case class Person(name: String, age: Int, address: Address)

val personFromConfig: ValidatedNec[ConfigError, Person] =
  Apply[ValidatedNec[ConfigError, *]].map4(personConfig.parse[String]("name").toValidatedNec,
                                           personConfig.parse[Int]("age").toValidatedNec,
                                           personConfig.parse[Int]("house_number").toValidatedNec,
                                           personConfig.parse[String]("street").toValidatedNec) {
    case (name, age, houseNumber, street) => Person(name, age, Address(houseNumber, street))
  }
```

## Of `flatMap`s and `Either`s
`Option` has `flatMap`, `Either` has `flatMap`, where's `Validated`'s? Let's try to implement it - better yet,
let's implement the `Monad` type class.

```scala mdoc:silent:nest
import cats.Monad

implicit def validatedMonad[E]: Monad[Validated[E, *]] =
  new Monad[Validated[E, *]] {
    def flatMap[A, B](fa: Validated[E, A])(f: A => Validated[E, B]): Validated[E, B] =
      fa match {
        case Valid(a)     => f(a)
        case i@Invalid(_) => i
      }

    def pure[A](x: A): Validated[E, A] = Valid(x)

    @annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => Validated[E, Either[A, B]]): Validated[E, B] =
      f(a) match {
        case Valid(Right(b)) => Valid(b)
        case Valid(Left(a)) => tailRecM(a)(f)
        case i@Invalid(_) => i
      }
  }
```

Note that all `Monad` instances are also `Applicative` instances, where `ap` is defined as

```scala mdoc:compile-only
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

```scala mdoc
validatedMonad.tuple2(Validated.invalidNec[String, Int]("oops"), Validated.invalidNec[String, Double]("uh oh"))
```

This one short circuits! Therefore, if we were to define a `Monad` (or `FlatMap`) instance for `Validated` we would
have to override `ap` to get the behavior we want. 

```scala mdoc:silent:nest
import cats.Monad

implicit def accumulatingValidatedMonad[E: Semigroup]: Monad[Validated[E, *]] =
  new Monad[Validated[E, *]] {
    def flatMap[A, B](fa: Validated[E, A])(f: A => Validated[E, B]): Validated[E, B] =
      fa match {
        case Valid(a)     => f(a)
        case i@Invalid(_) => i
      }

    def pure[A](x: A): Validated[E, A] = Valid(x)

    @annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => Validated[E, Either[A, B]]): Validated[E, B] =
      f(a) match {
        case Valid(Right(b)) => Valid(b)
        case Valid(Left(a)) => tailRecM(a)(f)
        case i@Invalid(_) => i
      }

    override def ap[A, B](f: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
      (fa, f) match {
        case (Valid(a), Valid(fab)) => Valid(fab(a))
        case (i@Invalid(_), Valid(_)) => i
        case (Valid(_), i@Invalid(_)) => i
        case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
      }
  }
```

But then the behavior of `flatMap` would be inconsistent with that of `ap`, and this will violate one of the [FlatMap laws](https://github.com/typelevel/cats/blob/main/laws/src/main/scala/cats/laws/FlatMapLaws.scala), `flatMapConsistentApply`:

```scala
// the `<->` operator means "is equivalent to" and returns a data structure
// `IsEq` that is used to verify the equivalence of the two expressions
def flatMapConsistentApply[A, B](fa: F[A], fab: F[A => B]): IsEq[F[B]] = 
  fab.ap(fa) <-> fab.flatMap(f => fa.map(f))
```

```scala mdoc:silent
import cats.laws._

val flatMapLawsForAccumulatingValidatedMonad = 
  FlatMapLaws[Validated[NonEmptyChain[String], *]](accumulatingValidatedMonad)

val fa  = Validated.invalidNec[String, Int]("oops")
val fab = Validated.invalidNec[String, Int => Double]("Broken function")
```

```scala mdoc
flatMapLawsForAccumulatingValidatedMonad.flatMapConsistentApply(fa , fab)
```

Therefore, `Validated` has only an `Applicative` instance.

## `Validated` vs `Either`

We've established that an error-accumulating data type such as `Validated` can't have a valid `Monad` instance. Sometimes the task at hand requires error-accumulation. However, sometimes we want a monadic structure that we can use for sequential validation (such as in a for-comprehension). This leaves us in a bit of a conundrum.

Cats has decided to solve this problem by using separate data structures for error-accumulation (`Validated`) and short-circuiting monadic behavior (`Either`).

If you are trying to decide whether you want to use `Validated` or `Either`, a simple heuristic is to use `Validated` if you want error-accumulation and to otherwise use `Either`.

## Sequential Validation

If you do want error accumulation but occasionally run into places where you sequential validation is needed, then `Validated` provides a couple methods that may be helpful.

### `andThen`
The `andThen` method is similar to `flatMap` (such as `Either.flatMap`). In the case of success, it passes the valid value into a function that returns a new `Validated` instance.

```scala mdoc
val houseNumber = config.parse[Int]("house_number").andThen{ n =>
   if (n >= 0) Validated.valid(n)
   else Validated.invalid(ParseError("house_number"))
}
```

### `withEither`
The `withEither` method allows you to temporarily turn a `Validated` instance into an `Either` instance and apply it to a function.

```scala mdoc:silent
def positive(field: String, i: Int): Either[ConfigError, Int] = {
  if (i >= 0) Right(i)
  else Left(ParseError(field))
}
```

Thus.

```scala mdoc:nest
val houseNumber = config.parse[Int]("house_number").withEither{ either: Either[ConfigError, Int] =>
  either.flatMap{ i =>
    positive("house_number", i)
  }
}
```

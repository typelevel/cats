# Kleisli

API Documentation: @:api(cats.data.Kleisli)

Kleisli enables composition of functions that return a monadic value, for instance an `Option[Int]` 
or a `Either[String, List[Double]]`, without having functions take an `Option` or `Either` as a parameter, 
which can be strange and unwieldy.

We may also have several functions which depend on some environment and want a nice way to compose these functions
to ensure they all receive the same environment. Or perhaps we have functions which depend on their own "local"
configuration and all the configurations together make up a "global" application configuration. How do we
have these functions play nice with each other despite each only knowing about their own local requirements?

These situations are where `Kleisli` is immensely helpful.

## Functions
One of the most useful properties of functions is that they **compose**. That is, given a function
`A => B` and a function `B => C`, we can combine them to create a new function `A => C`. It is through
this compositional property that we are able to write many small functions and compose them together
to create a larger one that suits our needs.

```scala mdoc:silent
val twice: Int => Int =
  x => x * 2

val countCats: Int => String =
  x => if (x == 1) "1 cat" else s"$x cats"

val twiceAsManyCats: Int => String =
  twice andThen countCats // equivalent to: countCats compose twice  
```

Thus.

```scala mdoc
twiceAsManyCats(1) // "2 cats"
```

Sometimes, our functions will need to return monadic values. For instance, consider the following set of functions.

```scala mdoc:silent
val parse: String => Option[Int] =
  s => if (s.matches("-?[0-9]+")) Some(s.toInt) else None

val reciprocal: Int => Option[Double] =
  i => if (i != 0) Some(1.0 / i) else None
```

As it stands we cannot use `Function1.compose` (or `Function1.andThen`) to compose these two functions.
The output type of `parse` is `Option[Int]` whereas the input type of `reciprocal` is `Int`.

This is where `Kleisli` comes into play.

## Kleisli
At its core, `Kleisli[F[_], A, B]` is just a wrapper around the function `A => F[B]`. Depending on the
properties of the `F[_]`, we can do different things with `Kleisli`s. For instance, if `F[_]` has a
`FlatMap[F]` instance (we can call `flatMap` on `F[A]` values), we can
compose two `Kleisli`s much like we can two functions.

```scala mdoc:silent
import cats.FlatMap
import cats.syntax.all._

final case class Kleisli[F[_], A, B](run: A => F[B]) {
  def compose[Z](k: Kleisli[F, Z, A])(implicit F: FlatMap[F]): Kleisli[F, Z, B] =
    Kleisli[F, Z, B](z => k.run(z).flatMap(run))
}
```

Returning to our earlier example:

```scala mdoc:silent:nest
// Bring in cats.FlatMap[Option] instance
import cats.syntax.all._

val parse: Kleisli[Option,String,Int] =
  Kleisli((s: String) => if (s.matches("-?[0-9]+")) Some(s.toInt) else None)

val reciprocal: Kleisli[Option,Int,Double] =
  Kleisli((i: Int) => if (i != 0) Some(1.0 / i) else None)

val parseAndReciprocal: Kleisli[Option,String,Double] =
  reciprocal.compose(parse)
```

`Kleisli#andThen` can be defined similarly.

It is important to note that the `F[_]` having a `FlatMap` (or a `Monad`) instance is not a hard requirement -
we can do useful things with weaker requirements. Such an example would be `Kleisli#map`, which only requires
that `F[_]` have a `Functor` instance (e.g. is equipped with `map: F[A] => (A => B) => F[B]`).

```scala mdoc:silent:nest
import cats.Functor

final case class Kleisli[F[_], A, B](run: A => F[B]) {
  def map[C](f: B => C)(implicit F: Functor[F]): Kleisli[F, A, C] =
    Kleisli[F, A, C](a => F.map(run(a))(f))
}
```

Below are some more methods on `Kleisli` that can be used as long as the constraint on `F[_]`
is satisfied.

```
Method    | Constraint on `F[_]`
--------- | -------------------
andThen   | FlatMap
compose   | FlatMap
flatMap   | FlatMap
lower     | Monad
map       | Functor
traverse  | Applicative
```

### Type class instances
The type class instances for `Kleisli`, like that for functions, often fix the input type (and the `F[_]`) and leave
the output type free. What type class instances it has tends to depend on what instances the `F[_]` has. For
instance, `Kleisli[F, A, B]` has a `Functor` instance as long as the chosen `F[_]` does. It has a `Monad`
instance as long as the chosen `F[_]` does. The instances in Cats are laid out in a way such that implicit
resolution will pick up the most specific instance it can (depending on the `F[_]`).

An example of a `Monad` instance for `Kleisli` is shown below.

*Note*: the example below assumes usage of the [kind-projector compiler plugin](https://github.com/typelevel/kind-projector) and will not compile if it is not being used in a project.

```scala mdoc:silent
import cats.syntax.all._

// We can define a FlatMap instance for Kleisli if the F[_] we chose has a FlatMap instance
// Note the input type and F are fixed, with the output type left free
implicit def kleisliFlatMap[F[_], Z](implicit F: FlatMap[F]): FlatMap[Kleisli[F, Z, *]] =
  new FlatMap[Kleisli[F, Z, *]] {
    def flatMap[A, B](fa: Kleisli[F, Z, A])(f: A => Kleisli[F, Z, B]): Kleisli[F, Z, B] =
      Kleisli(z => fa.run(z).flatMap(a => f(a).run(z)))

    def map[A, B](fa: Kleisli[F, Z, A])(f: A => B): Kleisli[F, Z, B] =
      Kleisli(z => fa.run(z).map(f))

    def tailRecM[A, B](a: A)(f: A => Kleisli[F, Z, Either[A, B]]) =
      Kleisli[F, Z, B]({ z => FlatMap[F].tailRecM(a) { f(_).run(z) } })
  }
```

Below is a table of some of the type class instances `Kleisli` can have depending on what instances `F[_]` has.

```
Type class     | Constraint on `F[_]`
-------------- | -------------------
Functor        | Functor
Apply          | Apply
Applicative    | Applicative
FlatMap        | FlatMap
Monad          | Monad
Arrow          | Monad
Split          | FlatMap
Strong         | Functor
SemigroupK*    | FlatMap
MonoidK*       | Monad
```

*These instances only exist for Kleisli arrows with identical input and output types; that is, 
`Kleisli[F, A, A]` for some type A. These instances use Kleisli composition as the `combine` operation,
and `Monad.pure` as the `empty` value.

Also, there is an instance of `Monoid[Kleisli[F, A, B]]` if there is an instance of `Monoid[F[B]]`. 
`Monoid.combine` here creates a new Kleisli arrow which takes an `A` value and feeds it into each 
of the combined Kleisli arrows, which together return two `F[B]` values. Then, they are combined into one 
using the `Monoid[F[B]]` instance.

## Other uses
### Monad Transformers
Many data types have a monad transformer equivalent that allows us to compose the `Monad` instance of the data
type with any other `Monad` instance. For instance, `OptionT[F[_], A]` allows us to compose the monadic properties
of `Option` with any other `F[_]`, such as a `List`. This allows us to work with nested contexts/effects in a
nice way (for example, in for-comprehensions).

`Kleisli` can be viewed as the monad transformer for functions. Recall that at its essence, `Kleisli[F, A, B]`
is just a function `A => F[B]`, with niceties to make working with the value we actually care about, the `B`, easy.
`Kleisli` allows us to take the effects of functions and have them play nice with the effects of any other `F[_]`.

This may raise the question, what exactly is the "effect" of a function?

Well, if we take a look at any function, we can see it takes some input and produces some output with it, without
having touched the input (assuming the function is pure, i.e.
[referentially transparent](https://en.wikipedia.org/wiki/Referential_transparency_%28computer_science%29)).
That is, we take a read-only value, and produce some value with it. For this reason, the type class instances for
functions often refer to the function as a `Reader`. For instance, it is common to hear about the `Reader` monad.
In the same spirit, Cats defines a `Reader` type alias along the lines of:

```scala mdoc:silent
// We want A => B, but Kleisli provides A => F[B]. To make the types/shapes match,
// we need an F[_] such that providing it a type A is equivalent to A
// This can be thought of as the type-level equivalent of the identity function
type Id[A] = A

type Reader[A, B] = Kleisli[Id, A, B]
object Reader {
  // Lifts a plain function A => B into a Kleisli, giving us access
  // to all the useful methods and type class instances
  def apply[A, B](f: A => B): Reader[A, B] = Kleisli[Id, A, B](f)
}

type ReaderT[F[_], A, B] = Kleisli[F, A, B]
val ReaderT = Kleisli
```

The `ReaderT` value alias exists to allow users to use the `Kleisli` companion object as if it were `ReaderT`, if
they were so inclined.

The topic of functions as a read-only environment brings us to our next common use case of `Kleisli` - configuration.

### Configuration
Functional programming advocates the creation of programs and modules by composing smaller, simpler modules. This
philosophy intentionally mirrors that of function composition - write many small functions, and compose them
to build larger ones. After all, our programs are just functions.

Let's look at some example modules, where each module has its own configuration that is validated by a function.
If the configuration is good, we return a `Some` of the module, otherwise a `None`. This example uses `Option` for
simplicity - if you want to provide error messages or other failure context, consider using `Either` instead.

```scala mdoc:silent
case class DbConfig(url: String, user: String, pass: String)
trait Db
object Db {
  val fromDbConfig: Kleisli[Option, DbConfig, Db] = ???
}

case class ServiceConfig(addr: String, port: Int)
trait Service
object Service {
  val fromServiceConfig: Kleisli[Option, ServiceConfig, Service] = ???
}
```

We have two independent modules, a `Db` (allowing access to a database) and a `Service` (supporting an API to provide
data over the web). Both depend on their own configuration parameters. Neither know or care about the other, as it
should be. However our application needs both of these modules to work. It is plausible we then have a more global
application configuration.

```scala mdoc:silent
case class AppConfig(dbConfig: DbConfig, serviceConfig: ServiceConfig)

class App(db: Db, service: Service)
```

As it stands, we cannot use both `Kleisli` validation functions together nicely - one takes a `DbConfig`, the
other a `ServiceConfig`. That means the `FlatMap` (and by extension, the `Monad`) instances differ (recall the
input type is fixed in the type class instances). However, there is a nice function on `Kleisli` called `local`.

```scala mdoc:silent:nest
final case class Kleisli[F[_], A, B](run: A => F[B]) {
  def local[AA](f: AA => A): Kleisli[F, AA, B] = Kleisli(f.andThen(run))
}
```

What `local` allows us to do is essentially "expand" our input type to a more "general" one. In our case, we
can take a `Kleisli` that expects a `DbConfig` or `ServiceConfig` and turn it into one that expects an `AppConfig`,
as long as we tell it how to go from an `AppConfig` to the other configs.

Now we can create our application config validator!

```scala mdoc:silent:nest
final case class Kleisli[F[_], Z, A](run: Z => F[A]) {
  def flatMap[B](f: A => Kleisli[F, Z, B])(implicit F: FlatMap[F]): Kleisli[F, Z, B] =
    Kleisli(z => F.flatMap(run(z))(a => f(a).run(z)))

  def map[B](f: A => B)(implicit F: Functor[F]): Kleisli[F, Z, B] =
    Kleisli(z => F.map(run(z))(f))

  def local[ZZ](f: ZZ => Z): Kleisli[F, ZZ, A] = Kleisli(f.andThen(run))
}

case class DbConfig(url: String, user: String, pass: String)
trait Db
object Db {
  val fromDbConfig: Kleisli[Option, DbConfig, Db] = ???
}

case class ServiceConfig(addr: String, port: Int)
trait Service
object Service {
  val fromServiceConfig: Kleisli[Option, ServiceConfig, Service] = ???
}

case class AppConfig(dbConfig: DbConfig, serviceConfig: ServiceConfig)

class App(db: Db, service: Service)

def appFromAppConfig: Kleisli[Option, AppConfig, App] =
  for {
    db <- Db.fromDbConfig.local[AppConfig](_.dbConfig)
    sv <- Service.fromServiceConfig.local[AppConfig](_.serviceConfig)
  } yield new App(db, sv)
```

What if we need a module that doesn't need any config validation, say a strategy to log events? We would have such a
module be instantiated from a config directly, without an `Option` - we would have something like
`Kleisli[Id, LogConfig, Log]` (alternatively, `Reader[LogConfig, Log]`). However, this won't play nice with our other
`Kleisli`s since those use `Option` instead of `Id`.

We can define a `lift` method on `Kleisli` (available already on `Kleisli` in Cats) that takes a type parameter `G[_]`
such that `G` has an `Applicative` instance and lifts a `Kleisli` value such that its output type is `G[F[B]]`. This
allows us to then lift a `Reader[A, B]` into a `Kleisli[G, A, B]`. Note that lifting a `Reader[A, B]` into some `G[_]`
is equivalent to having a `Kleisli[G, A, B]` since `Reader[A, B]` is just a type alias for `Kleisli[Id, A, B]`, and
`type Id[A] = A` so `G[Id[A]]` is equivalent to `G[A]`.

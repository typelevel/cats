---
layout: default
title:  "Kleisli"
section: "data"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/data/Kleisli.scala"
scaladoc: "#cats.data.Kleisli"
---
# Kleisli
Kleisli is a data type that will come in handy often, especially if you are working with monadic functions.

## Functions
One of the most useful properties of functions is that functions **compose**. That is, given a function
`A => B` and a function `B => C`, we can compose them to create a new function `A => C`. It is through
this compositional property that we are able to write many small functions and compose them together
to create a larger one that suits our needs.

Often times, our functions will return monadic values. For instance, consider the following set of functions.

```tut
val parse: String => Option[Int] = s =>
  try {
    Some(s.toInt)
  } catch {
    case _: NumberFormatException => None
  }

val reciprocal: Int => Option[Double] =
  i => if (i != 0) Some(1.0 / i) else None
```

As it stands we cannot use `Function1.compose` (or `Function1.andThen`) to compose these two functions.
The output type of `parse` is `Option[Int]` whereas the input type of `reciprocal` is `Int`.

This is where `Kleisli` comes into play.

## Kleisli
At it's core, `Kleisli[F[_], A, B]` is just a wrapper around the function `A => F[B]`. Depending on the
properties of the `F[_]`, we can do different things with `Kleisli`s. For instance, if `F[_]` has a
`FlatMap[F]` instance (e.g. is equipped with a `flatMap: F[A] => (A => F[B]) => F[B]` function), we can
compose two `Kleisli`s much like we can two functions.

```tut
import cats.FlatMap

final case class Kleisli[F[_], A, B](run: A => F[B]) {
  def compose[Z](k: Kleisli[F, Z, A])(implicit F: FlatMap[F]): Kleisli[F, Z, B] =
    Kleisli[F, Z, B](z => F.flatMap(k.run(z))(run))
}
```

Returning to our earlier example:

```tut
// Bring in cats.FlatMap[Option] instance
import cats.std.option._

val parse = Kleisli((s: String) => try { Some(s.toInt) } catch { case _: NumberFormatException => None })

val reciprocal = Kleisli((i: Int) => if (i == 0) None else Some(1.0 / i))

val parseAndReciprocal = reciprocal.compose(parse)
```

`Kleisli#andThen` can be defined similarly.

It is important to note that the `F[_]` having a `FlatMap` (or a `Monad`) instance is not a hard requirement -
we can do useful things with weaker requirements. Such an example would be `Kleisli#map`, which only requires
that `F[_]` have a `Functor` instance (e.g. is equipped with `map: F[A] => (A => B) => F[B]`).

```tut
import cats.Functor

final case class Kleisli[F[_], A, B](run: A => F[B]) {
  def map[C](f: B => C)(implicit F: Functor[F]): Kleisli[F, A, C] =
    Kleisli[F, A, C](a => F.map(run(a))(f))
}
```

### Type class instances
The type class instances for `Kleisli`, like that for functions, fix the input type (and the `F[_]`) and leave
the output type free. What type class instances it has tends to depend on what instances the `F[_]` has. For
instance, `Kleisli[F, A, B]` has a `Functor` instance so long as the chosen `F[_]` does. It has a `Monad`
instance so long as the chosen `F[_]` does. The instances in Cats are laid out in a way such that implicit
resolution will pick up the most specific instance it can (depending on the `F[_]`).

An example of a `Monad` instance for `Kleisli` would be:

```tut
// We can define a FlatMap instance for Kleisli if the F[_] we chose has a FlatMap instance
// Note the input type and F are fixed, with the output type left free
implicit def kleisliFlatMap[F[_], Z](implicit F: FlatMap[F]): FlatMap[Kleisli[F, Z, ?]] =
  new FlatMap[Kleisli[F, Z, ?]] {
    def flatMap[A, B](fa: Kleisli[F, Z, A])(f: A => Kleisli[F, Z, B]): Kleisli[F, Z, B] =
      Kleisli(z => F.flatMap(fa.run(z))(a => f(a).run(z)))

    def map[A, B](fa: Kleisli[F, Z, A])(f: A => B): Kleisli[F, Z, B] =
      Kleisli(z => F.map(fa.run(z))(f))
  }
```

## Other uses
### Monad Transformer
Many data types have a monad transformer equivalent that allows us to compose the `Monad` instance of the data
type with any other `Monad` instance. For instance, `OptionT[F[_], A]` allows us to compose the monadic properties
of `Option` with any other `F[_]`, such as a `List`. This allows us to work with nested contexts/effects in a
nice way, say, in for comprehensions.

`Kleisli` can be viewed as the monad transformer for functions. Recall that at its essence, `Kleisli[F, A, B]`
is just a function `A => F[B]`, with niceties to make working with the value we actually care about, the `B`, easy.
`Kleisli` allows us to take the effects of functions and have them play nice with the effects of any other `F[_]`.

This may raise the question, what exactly is the "effect" of a function?

Well, if we take a look at any function, we can see it takes some input and produces some output with it, without
having touched the input (assuming a functional environment). That is, we take a read-only value, and produce some
value with it. For this reason, the type class instances for functions often refer to the function as a `Reader`.
For instance, it is common to hear about the `Reader` monad. In the same spirit, Cats defines a `Reader` type alias
along the lines of:

```tut
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
In functional programming it is often advocated to build many small modules, and then compose them together to
make larger and larger ones until you have the module you want (the overall program). This philosophy (not
accidentally) mirrors that of function composition - write many small functions, and compose them to build larger ones.
After all, our programs are just functions.

Let's look at some example modules, where each module has it's own configuration that is validated by a function.
If the configuration is good, we return a `Some` of the module, otherwise a `None` (you can/should use `Xor` or similar
to provide more detailed errors - `Option` was chosen for simplicity). Due to the nature of this, we use
`Kleisli` to define the function.

```tut
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

We have two independent modules, a `Db` and a `Service` (presumably this is a service that will read from a
database and expose the data at some endpoint). Both depend on their own configuration parameters. Neither know
or care about the other, as it should be. However our application needs both of these modules to work. It is
plausible we then have a more global application configuration.

```tut
case class AppConfig(dbConfig: DbConfig, serviceConfig: ServiceConfig)

class App(db: Db, service: Service)
```

As it stands, we cannot use both `Kleisli` validation functions together nicely - one takes a `DbConfig`, the
other a `ServiceConfig`. That means the `FlatMap` (and by extension, the `Monad`) instances differ (recall the
input type is fixed in the type class instances). However, there is a nice function on `Kleisli` called `local`.

```tut
final case class Kleisli[F[_], A, B](run: A => F[B]) {
  def local[AA](f: AA => A): Kleisli[F, AA, B] = Kleisli(f.andThen(run))
}
```

What `local` allows us to do is essentially "expand" our input type to a more "general" one. In our case, we
can take a `Kleisli` that expects a `DbConfig` or `ServiceConfig` and turn it into one that expects an `AppConfig`,
so long as we tell it how to go from an `AppConfig` to the other configs.

Now we can create our application config validator!

```tut
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

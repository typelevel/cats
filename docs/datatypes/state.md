# State

API Documentation: @:api(cats.data.IndexedStateT)

`State` is a structure that provides a functional approach to handling application state. `State[S, A]` is basically a function `S => (S, A)`, where `S` is the type that represents your state and `A` is the result the function produces. In addition to returning the result of type `A`, the function returns a new `S` value, which is the updated state.

## Robots

Let's try to make this more concrete with an example. We have this `Robot` model:

```scala mdoc:silent
final case class Robot(
  id: Long,
  sentient: Boolean,
  name: String,
  model: String)
```

We would like to generate some random `Robot` instances for test data.

## Pseudorandom values

Scala's standard library has a built-in `Random` class that provides a (pseudo)random number generator (RNG). Let's use it to write a method that creates robots.

```scala mdoc:silent
val rng = new scala.util.Random(0L)

def createRobot(): Robot = {
  val id = rng.nextLong()
  val sentient = rng.nextBoolean()
  val isCatherine = rng.nextBoolean()
  val name = if (isCatherine) "Catherine" else "Carlos"
  val isReplicant = rng.nextBoolean()
  val model = if (isReplicant) "replicant" else "borg"
  Robot(id, sentient, name, model)
}
```

```scala mdoc
val robot = createRobot()
```

We create a single `Random` instance, which is mutated as a side-effect each time that we call `nextLong` or `nextBoolean` on it. This mutation makes it more difficult to reason about our code. Someone might come along and see that we have `rng.nextBoolean` repeated three times within a single method. They might cleverly avoid repeated code and method invocations by extracting the common code into a variable:

```scala mdoc:silent:nest
val rng = new scala.util.Random(0L)

def createRobot(): Robot = {
  val id = rng.nextLong()
  val b = rng.nextBoolean()
  val sentient = b
  val isCatherine = b
  val name = if (isCatherine) "Catherine" else "Carlos"
  val isReplicant = b
  val model = if (isReplicant) "replicant" else "borg"
  Robot(id, sentient, name, model)
}
```

```scala mdoc
val robot = createRobot()
```

But now the output of our program has changed! We used to have a replicant robot named Catherine, but now we have a borg robot named Carlos. It might not have been obvious, but the `nextBoolean` calls we were making had the side effect of mutating internal RNG state, and we were depending on that behavior.

When we can't freely refactor identical code into a common variable, the code becomes harder to reason about. In functional programming lingo, one might say that such code lacks [referential transparency](https://en.wikipedia.org/wiki/Referential_transparency_(computer_science)).

## Purely functional pseudorandom values

Since mutating state caused us trouble, let's create an RNG that is immutable.

We'll use a simple RNG that can generate pseudorandom `Long` values based only on the previous "seed" value and some carefully chosen constants. You don't need to understand the details of this implementation for the purposes of this example, but if you'd like to know more, this is Knuth's 64-bit [linear congruential generator](https://en.wikipedia.org/wiki/Linear_congruential_generator).

```scala mdoc:silent
final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}
```

Instead of mutating the existing `long` value, calling `next` returns a _new_ `Seed` instance with an updated `long` value.

Since the RNG isn't updating state internally, we will need to keep track of state outside of the RNG. When we call `nextBoolean` we will want it to return a `Boolean` as it did before, but we will also want it to return an updated `Seed` that we can use to generate our next random value.

```scala mdoc:silent
def nextBoolean(seed: Seed): (Seed, Boolean) =
  (seed.next, seed.long >= 0L)
```

Similarly, `nextLong` will return an updated `Seed` along with a `Long` value.

```scala mdoc:silent
def nextLong(seed: Seed): (Seed, Long) =
  (seed.next, seed.long)
```

Now we need to explicitly pass in the updated state as we generate each new value.

```scala mdoc:silent:nest
def createRobot(seed: Seed): Robot = {
  val (seed1, id) = nextLong(seed)
  val (seed2, sentient) = nextBoolean(seed1)
  val (seed3, isCatherine) = nextBoolean(seed2)
  val name = if (isCatherine) "Catherine" else "Carlos"
  val (seed4, isReplicant) = nextBoolean(seed3)
  val model = if (isReplicant) "replicant" else "borg"
  Robot(id, sentient, name, model)
}

val initialSeed = Seed(13L)
```

```scala mdoc
val robot = createRobot(initialSeed)
```

Now it is a bit more obvious that we can't extract the three `nextBoolean` calls into a single variable, because we are passing each one a different seed value.

However, it is a bit cumbersome to explicitly pass around all of this intermediate state. It's also a bit error-prone. It would have been easy to accidentally call `nextBoolean(seed2)` for both the name generation and the model generation, instead of remembering to use `nextBoolean(seed3)` the second time.

## Cleaning it up with State

State's special power is keeping track of state and passing it along. Recall the description of `State` at the beginning of this document. It is basically a function `S` => `(S, A)`, where `S` is a type representing state.

Our `nextLong` function takes a `Seed` and returns an updated `Seed` and a `Long`. It can be represented as `Seed => (Seed, Long)`, and therefore matches the pattern `S => (S, A)` where `S` is `Seed` and `A` is `Long`.

Let's write a new version of `nextLong` using `State`:

```scala mdoc:invisible:reset
// re-define everything to make it easier to reset mdoc
// as things like nextLong are conflicting with each other

final case class Robot(
  id: Long,
  sentient: Boolean,
  name: String,
  model: String)

final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}

val initialSeed = Seed(13L)
```

```scala mdoc:silent
import cats.data.State

val nextLong: State[Seed, Long] = State(seed =>
  (seed.next, seed.long))
```

The `map` method on `State` allows us to transform the `A` value without affecting the `S` (state) value. This is perfect for implementing `nextBoolean` in terms of `nextLong`.

```scala mdoc:silent
val nextBoolean: State[Seed, Boolean] = nextLong.map(long =>
  long >= 0)
```

The `flatMap` method on `State[S, A]` lets you use the result of one `State` in a subsequent `State`. The updated state (`S`) after the first call is passed into the second call. These `flatMap` and `map` methods allow us to use `State` in for-comprehensions:

```scala mdoc:silent
val createRobot: State[Seed, Robot] =
  for {
    id <- nextLong
    sentient <- nextBoolean
    isCatherine <- nextBoolean
    name = if (isCatherine) "Catherine" else "Carlos"
    isReplicant <- nextBoolean
    model = if (isReplicant) "replicant" else "borg"
  } yield Robot(id, sentient, name, model)
```

At this point, we have not yet created a robot; we have written instructions for creating a robot. We need to pass in an initial seed value, and then we can call `value` to actually create the robot:

```scala mdoc:nest
val (finalState, robot) = createRobot.run(initialSeed).value
```

If we only care about the robot and not the final state, then we can use `runA`:

```scala mdoc:nest
val robot = createRobot.runA(initialSeed).value
```

The `createRobot` implementation reads much like the imperative code we initially wrote for the mutable RNG. However, this implementation is free of mutation and side-effects. Since this code is referentially transparent, we can perform the refactoring that we tried earlier without affecting the result:

```scala mdoc:silent:nest
val createRobot: State[Seed, Robot] = {
  val b = nextBoolean

  for {
    id <- nextLong
    sentient <- b
    isCatherine <- b
    name = if (isCatherine) "Catherine" else "Carlos"
    isReplicant <- b
    model = if (isReplicant) "replicant" else "borg"
  } yield Robot(id, sentient, name, model)
}
```

```scala mdoc:nest
val robot = createRobot.runA(initialSeed).value
```

This may seem surprising, but keep in mind that `b` isn't simply a `Boolean`. It is a function that takes a seed and _returns_ a `Boolean`, threading state along the way. Since the seed that is being passed into `b` changes from line to line, so do the returned `Boolean` values.

## Interleaving effects

Let's expand on the above example; assume that our random number generator works asynchronously by fetching results from a remote server:
```scala mdoc:silent
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

final case class AsyncSeed(long: Long) {
  def next = Future(AsyncSeed(long * 6364136223846793005L + 1442695040888963407L))
}
```

Using a proper `IO` data type would be ideal, but `Future` serves our purpose here.

We now need to redefine our `nextLong` action:
```scala mdoc:fail
val nextLong: State[AsyncSeed, Future[Long]] = State { seed =>
  (seed.next, seed.long)
}
```

Oops! That doesn't work: `State[S, A]` requires that we return a pure next `S` in the `State.apply` constructor. We could define `nextLong` as `State[Future[AsyncSeed], Future[Long]]`, but that would get us into even more trouble:
```scala mdoc:fail
val nextLong: State[Future[AsyncSeed], Future[Long]] = State { seedF =>
  seedF.map { seed =>
    (seed.next, seed.long)
  }
}
```

The `seed` that `State.apply` passes in is now a `Future`, so we must map it. But we can't return a `Future[(Future[S], [A])]`; so what do we do?

Luckily, `State[S, A]` is an alias for `StateT[Eval, S, A]` - a monad transformer defined as `StateT[F[_], S, A]`. This data type represents computations of the form `S => F[(S, A)]`.

If we plug in our concrete types, we get `AsyncSeed => Future[(AsyncSeed, A)]`, which is something we can work with:
```scala mdoc:silent
import cats.data.StateT
import cats.instances.future._
import scala.concurrent.ExecutionContext.Implicits.global

val nextLong: StateT[Future, AsyncSeed, Long] = StateT { seed =>
  seed.next zip Future.successful(seed.long)
}
```

Now, what do we get back if we invoke `run` on our `nextLong` action?
```scala mdoc
nextLong.run(AsyncSeed(0))
```

Since every intermediate computation returns a `Future`, the composite computation returns a `Future` as well. To summarize, `StateT[F[_], S, A]` allows us to interleave effects of type `F[_]` in the computations wrapped by it.

It should be noted that different combinators on `StateT` impose different constraints on `F`; for example, `map` only requires that `F` has a `Functor` instance, but `flatMap` naturally requires `F` to have a `FlatMap` instance. Have a look at the method signatures for the details.

## Changing States

More complex, stateful computations cannot be easily modeled by a single type. For example, let's try to model a door's state:

```scala mdoc:silent
sealed trait DoorState
case object Open extends DoorState
case object Closed extends DoorState

case class Door(state: DoorState)

def open: State[DoorState, Unit] = ???
def close: State[DoorState, Unit] = ???
```

We would naturally expect that `open` would only operate on doors that are `Closed`, and vice-versa. However, to implement these methods currently, we have to pattern-match on the state:

```scala mdoc:silent
val openImpl: State[DoorState, Unit] = State { doorState =>
  doorState match {
    case Closed => (Open, ())
    case Open   => ??? // What now?
  }
}
```

This puts us in the awkward situation of deciding how to handle invalid states; what's the appropriate behavior? Should we just leave the door open? Should we return a failure? That would mean that our return type needs to be modified.

The most elegant solution would be to model this requirement statically using types, and luckily, `StateT` is an alias for another type: `IndexedStateT[F[_], SA, SB, A]`.

This data type models a stateful computation of the form `SA => F[(SB, A)]`; that's a function that receives an initial state of type `SA` and results in a state of type `SB` and a result of type `A`, using an effect of `F`.

So, let's model our typed door:
```scala mdoc:silent:nest
import cats.Eval
import cats.data.IndexedStateT

def open: IndexedStateT[Eval, Closed.type, Open.type, Unit] = IndexedStateT.set(Open)
def close: IndexedStateT[Eval, Open.type, Closed.type, Unit] = IndexedStateT.set(Closed)
```

We can now reject, at compile time, sequences of `open` and `close` that are invalid:
```scala mdoc:fail
val invalid = for {
  _ <- open
  _ <- close
  _ <- close
} yield ()
```

While valid sequences will be accepted:
```scala mdoc
val valid = for {
  _ <- open
  _ <- close
  _ <- open
} yield ()
```

Note that the inferred type of `valid` correctly models that this computation can be executed only with an initial `Closed` state.

```scala mdoc:fail
valid.run(Open)
```

```scala mdoc
valid.run(Closed)
```

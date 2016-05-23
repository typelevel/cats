---
layout: default
title:  "State"
section: "data"
source: "core/src/main/scala/cats/data/StateT.scala"
scaladoc: "#cats.data.StateT"
---
# State

`State` is a structure that provides a functional approach to handling application state. `State[S, A]` is basically a function `S => (S, A)`, where `S` is the type that represents your state and `A` is the result the function produces. In addition to returning the result of type `A`, the function returns a new `S` value, which is the updated state.

## Robots

Let's try to make this more concrete with an example. We have this `Robot` model:

```tut:silent
final case class Robot(
  id: Long,
  sentient: Boolean,
  name: String,
  model: String)
```

We would like to generate some random `Robot` instances for test data.

## Pseudorandom values

Scala's standard library has a built-in `Random` class that provides a (pseudo)random number generator (RNG). Let's use it to write a method that creates robots.

```tut:silent
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

```tut:book
val robot = createRobot()
```

We create a single `Random` instance, which is mutated as a side-effect each time that we call `nextLong` or `nextBoolean` on it. This mutation makes it more difficult to reason about our code. Someone might come along and see that we have `rng.nextBoolean` repeated three times within a single method. They might cleverly avoid repeated code and method invocations by extracting the common code into a variable:

```tut:silent
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

```tut:book
val robot = createRobot()
```

But now the output of our program has changed! We used to have a replicant robot named Catherine, but now we have a borg robot named Carlos. It might not have been obvious, but the `nextBoolean` calls we were making had the side effect of mutating internal RNG state, and we were depending on that behavior.

When we can't freely refactor identical code into a common variable, the code becomes harder to reason about. In functional programming lingo, one might say that such code lacks [referential transparency](https://en.wikipedia.org/wiki/Referential_transparency_(computer_science)).

## Purely functional pseudorandom values

Since mutating state caused us trouble, let's create an RNG that is immutable.

We'll use a simple RNG that can generate pseudorandom `Long` values based only on the previous "seed" value and some carefully chosen constants. You don't need to understand the details of this implementation for the purposes of this example, but if you'd like to know more, this is Knuth's 64-bit [linear congruential generator](https://en.wikipedia.org/wiki/Linear_congruential_generator).

```tut:silent
final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}
```

Instead of mutating the existing `long` value, calling `next` returns a _new_ `Seed` instance with an updated `long` value.

Since the RNG isn't updating state internally, we will need to keep track of state outside of the RNG. When we call `nextBoolean` we will want it to return a `Boolean` as it did before, but we will also want it to return an updated `Seed` that we can use to generate our next random value.

```tut:silent
  def nextBoolean(seed: Seed): (Seed, Boolean) =
    (seed.next, seed.long >= 0L)
```

Similarly, `nextLong` will return an updated `Seed` along with a `Long` value.

```tut:silent
  def nextLong(seed: Seed): (Seed, Long) =
    (seed.next, seed.long)
```

Now we need to explicitly pass in the updated state as we generate each new value.

```tut:silent
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

```tut:book
val robot = createRobot(initialSeed)
```

Now it is a bit more obvious that we can't extract the three `nextBoolean` calls into a single variable, because we are passing each one a different seed value.

However, it is a bit cumbersome to explicitly pass around all of this intermediate state. It's also a bit error-prone. It would have been easy to accidentally call `nextBoolean(seed2)` for both the name generation and the model generation, instead of remembering to use `nextBoolean(seed3)` the second time.

## Cleaning it up with State

State's special power is keeping track of state and passing it along. Recall the description of `State` at the beginning of this document. It is basically a function `S` => `(S, A)`, where `S` is a type representing state.

Our `nextLong` function takes a `Seed` and returns an updated `Seed` and a `Long`. It can be represented as `Seed => (Seed, Long)`, and therefore matches the pattern `S => (S, A)` where `S` is `Seed` and `A` is `Long`.

Let's write a new version of `nextLong` using `State`:

```tut:silent
import cats.data.State

val nextLong: State[Seed, Long] = State(seed =>
  (seed.next, seed.long))
```

The `map` method on `State` allows us to transform the `A` value without affecting the `S` (state) value. This is perfect for implementing `nextBoolean` in terms of `nextLong`.

```tut:silent
val nextBoolean: State[Seed, Boolean] = nextLong.map(long =>
  long > 0)
```

The `flatMap` method on `State[S, A]` lets you use the result of one `State` in a subsequent `State`. The updated state (`S`) after the first call is passed into the second call. These `flatMap` and `map` methods allow us to use `State` in for-comprehensions:

```tut:silent
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

```tut:book
val (finalState, robot) = createRobot.run(initialSeed).value
```

If we only care about the robot and not the final state, then we can use `runA`:

```tut:book
val robot = createRobot.runA(initialSeed).value
```

The `createRobot` implementation reads much like the imperative code we initially wrote for the mutable RNG. However, this implementation is free of mutation and side-effects. Since this code is referentially transparent, we can perform the refactoring that we tried earlier without affecting the result:

```tut:silent
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

```tut:book
val robot = createRobot.runA(initialSeed).value
```

This may seem surprising, but keep in mind that `b` isn't simply a `Boolean`. It is a function that takes a seed and _returns_ a `Boolean`, threading state along the way. Since the seed that is being passed into `b` changes from line to line, so do the returned `Boolean` values.

## Fine print

TODO explain StateT and the fact that State is an alias for StateT with Eval.

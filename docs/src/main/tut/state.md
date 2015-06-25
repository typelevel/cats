---
layout: default
title:  "State"
section: "data"
source: "https://github.com/non/cats/blob/master/state/src/main/scala/cats/state/State.scala"
scaladoc: "#cats.state.StateT"
---
# State

`State` is a structure that provides a functional approach to handling application state. `State[S, A]` is basically a function `S => (S, A)`, where `S` is the type that represents your state and `A` is the result the function produces. In addition to returning the result of type `A`, the function returns a new `S` value, which is the updated state.

## Pseudorandom values

Let's try to make this more concrete with an example. We want to generate pseudorandom values.

Scala's standard library has a built-in `Random` class that provides this functionality:

```tut
val rng = new scala.util.Random(0L)

val x = rng.nextLong()

val y = rng.nextLong()

val b = rng.nextBoolean()

val z = if (b) x else y
```

We create a single `Random` instance, which is mutated as a side-effect each time that we call `nextX` on it. Because of this mutation, `x` and `y` have different values, even though they both are created with `rng.nextLong()`. This isn't ideal, because it's easier to test and reason about code if calling a function with the same input values (in this case no input values) always generates the same output.

## Purely functional pseudorandom values

We can achieve a similar effect without mutation and side-effects using `State`.

### Create a generator

First, let's create our own pseudorandom number generator. For simplicity, we will use a [linear congruential generator](https://en.wikipedia.org/wiki/Linear_congruential_generator). This generator is just meant for demo purposes, so please don't use it in the real world!

```tut:silent
import cats.state.State
import cats.std.function._
```

```tut:silent
/** our generator will need a seed value to initialize generation */
type Seed = Long

final case class Lcg(modulus: Double, multiplier: Long, increment: Int) {

  /**
   * The pseudorandom function at the heart of the LCG.
   */
  def nextSeed(currentSeed: Seed): Seed =
    ((multiplier * currentSeed + increment.toLong) % modulus).toLong

  /**
   * Transition to the next seed value and also return that value.
   */
  def nextLong: State[Seed, Long] = State { seed =>
    val n = nextSeed(seed)
    (n, n)
  }

  /**
   * Transition to the next seed value and return a Boolean derived from that
   * value
   */
  def nextBoolean: State[Seed, Boolean] = nextLong.map(_ > 0)
}

object Lcg {
  /**
   * An LCG instance with some reasonable default values.
   */
  val default: Lcg = new Lcg(math.pow(2, 32), 22695477L, 1)
}
```

### Use our generator

Let's call `nextLong` a couple times like we did with the mutable generator.

```tut
/** our "random" number generator */
val rng = Lcg.default

val x: State[Seed, Long] = rng.nextLong

val y: State[Seed, Long] = rng.nextLong

val b: State[Seed, Boolean] = rng.nextBoolean
```

As you can see, `x` and `y` aren't `Long` values. They are `State` values. We can provide them with an initial seed value and then run the calculation to obtain the result.

```tut
val seed: Seed = 13L

x.run(seed).run
y.run(seed).run
b.run(seed).run
```

There are a couple things to notice. The first is that for each of `x`, `y`, and `z`, we actually got two values back when we ran the computation. They represent the updated state value (`S`, or in this case `Seed`), and the requested value (`Long` for `x` and `y`, and `Boolean` for `b`).

The next thing to notice is that the updated state value is the same in all cases, and the generated `Long` values for `x` and `y` are the same! With the mutable generator, calling `rng.nextLong` multiple times generated different values, but with our functional generator, repeated calls will always return the same result. This makes it easier to reason about behavior and to write deterministic tests, but what's the use of a "random" number generator that always produces the same result?

### Update state

The secret is that you want to chain your calls such that each time you pass in the updated state from the previous call.

```tut
val (seed1, x) = rng.nextLong.run(seed).run
val (seed2, y) = rng.nextLong.run(seed1).run
val (seed3, b) = rng.nextBoolean.run(seed2).run
```

### Tidy it up (part 1: flatMap)

Now our values are changing on subsequent calls, as we'd like. However, this approach is pretty verbose and error-prone. It would have been really easy to accidentally write `val (seed3, b) = rng.nextBoolean.run(seed1).run` instead of passing in `seed2`. Luckily, the `flatMap` method on `State` provides an easy way to thread the output state of one computation into the next.

```tut
val z: State[Seed, Long] = rng.nextLong flatMap { x =>
  rng.nextLong flatMap { y =>
    rng.nextBoolean map { b =>
      if (b) x else y
    }
  }
}

z.run(seed).run
```

### Tidy it up (part 2: for-comprehensions)

Now we don't have to worry about explicitly passing through the updated state, but it's still not the prettiest code. We can make it look a little more tidy by using Scala's for-comprehensions, which are syntax sugar for `flatMap` and `map` calls.

```tut

val z: State[Seed, Long] = for {
  x <- rng.nextLong
  y <- rng.nextLong
  b <- rng.nextBoolean
} yield if (b) x else y

z.run(seed).run
```

This now looks quite a bit like the initial mutable approach, but it's completely free of mutation and side-effects.

### Convenient methods

If we are only interested in the final `A` value, we can grab it by itself:

```tut
z.runA(seed).run
```

Alternatively, if we just care about the final seed (`S`) value, we can just grab it:

```tut
z.runS(seed).run
```

## Fine print

TODO explain `StateT` and the fact that `State` is an alias for `StateT` with trampolining.

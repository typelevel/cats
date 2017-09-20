---
layout: docs
title:  "Arrow"
section: "typeclasses"
source: "core/src/main/scala/cats/arrow/Arrow.scala"
scaladoc: "#cats.arrow.Arrow"
---
# Arrow

`Arrow` is a useful type class for modeling something that behaves like functions, such as `Function1`: `A => B`, `Kleisli`: `A => F[B]` (also known as `ReaderT`), `Cokleisli`: `F[A] => B`, etc. So useful, that Haskell provides special syntax (the `proc` notation) for composing and combining Arrows, similar as the `do` notation for sequencing monadic operations.

To create an `Arrow` instance for a type `F[A, B]`, the following abstract methods need to be implemented:

```
def lift[A, B](f: A => B): F[A, B]

def id[A]: F[A, A]

def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]

def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]
```

Once a type `F` has an `Arrow` instance, it gets for free a number of methods for composing and combining with other `Arrow`s. You will be able to do things like:
- Composing `fab: F[A, B]` and `fbc: F[B, C]` into `fac: F[A, C]` with `fab >>> fbc`. If `F` is `Function1` then `>>>` becomes alias of `andThen`.
- Taking two arrows `fab: F[A, B]` and `fbc: F[B, C]` and combining them into `F[(A, C) => (B, D)]` with `fab *** fbc`. The resulting arrow takes two inputs and processes them with two arrows, one for each input.
- Taking two arrows `fab: F[A, B]` and `fac: F[A, C]` and combining them into `F[A => (B, C)]` with `fab &&& fbc`. The resulting arrow takes an input, duplicates it and processes each copy with a different arrow.
- Taking an arrow `fab: F[A, B]` and turning it into `F[(C, A), (C, B)]` with `fab.second`. The resulting arrow takes two inputs, processes the second input and leaves the first input as it is.

## Example

As an example, let's create a fancy version of `Function1` that is capable of maintaining states. Let's call it `FancyFunction`:

```tut:book:silent
case class FancyFunction[A, B](run: A => (FancyFunction[A, B], B))
```

That is, given an `A`, it not only returns a `B`, but also returns a new `FancyFunction[A, B]`. This sounds similar as the `State` monad (which returns a result and a new `State` from an initial `State`), and indeed, `FancyFunction` can be used to perform stateful transformations.

Here's an `Arrow` instance for `FancyFunction`:

```tut:book:silent
import cats.arrow.Arrow
import cats.implicits._

implicit val arrowInstance: Arrow[FancyFunction] = new Arrow[FancyFunction] {

  override def lift[A, B](f: A => B): FancyFunction[A, B] = FancyFunction(lift(f) -> f(_))

  override def first[A, B, C](fa: FancyFunction[A, B]): FancyFunction[(A, C), (B, C)] = FancyFunction {case (a, c) =>
    val (fa2, b) = fa.run(a)
    (first(fa2), (b, c))
  }

  override def id[A]: FancyFunction[A, A] = FancyFunction(id -> _)

  override def compose[A, B, C](f: FancyFunction[B, C], g: FancyFunction[A, B]): FancyFunction[A, C] = FancyFunction {a =>
    val (gg, b) = g.run(a)
    val (ff, c) = f.run(b)
    (compose(ff, gg), c)
  }
}

```

Then we can start to do interesting things with it. For example, we can create a method `accum` that returns a `FancyFunction`, which accumulates values fed to it using the accumulation function `f` and the starting value `b`:

```tut:book:silent
def accum[A, B](b: B)(f: (A, B) => B): FancyFunction[A, B] = FancyFunction {a =>
  val b2 = f(a, b)
  (accum(b2)(f), b2)
}
```

We then define a method `runList` that takes a `List[A]`, feeds the `A` values one by one into the `FancyFunction` and collects the result into a `List[B]`:

```tut:book:silent
def runList[A, B](ff: FancyFunction[A, B], as: List[A]): List[B] = as match {
  case h :: t =>
    val (ff2, b) = ff.run(h)
    b :: runList(ff2, t)
  case _ => List()
}
```

Now we can try it out:

```tut:book:silent
runList(accum[Int, Int](0)(_ + _), List(6, 5, 4, 3, 2, 1))
// res1: List[Int] = List(6, 11, 15, 18, 20, 21)
```

As another example, let's make the above `FancyFunction` collect the running average of a list of numbers. To do so it needs to keep track of the count in addition to the sum. In this example we will combine several `FancyFunction`s to get the `FancyFunction` we want.

We first define arrow `sum` in terms of `accum`, and define arrow `count` by composing `_ => 1` with `sum`:

```tut:book:silent
import cats.kernel.Monoid

def sum[A: Monoid]: FancyFunction[A, A] = accum(Monoid[A].empty)(_ |+| _)
def count[A]: FancyFunction[A, Int] = Arrow[FancyFunction].lift((_: A) => 1) >>> sum
```

Then we define a `combine` function that takes an input and processes two copies of it with two arrows, and finally, the `avg` arrow in terms of the arrows we have so far:

```tut:book:silent
def combine[F[_, _]: Arrow, A, B, C](fab: F[A, B], fac: F[A, C]): F[A, (B, C)] =
  Arrow[F].lift((a: A) => (a, a)) >>> (fab *** fac)

def avg: FancyFunction[Int, Double] =
  combine(sum[Int], count[Int]) >>> Arrow[FancyFunction].lift{case (x, y) => x.toDouble / y}
```

And try it out:

```tut:book:silent
runList(avg, List(1, 10, 100, 1000))
// res2: List[Int] = List(1.0, 5.5, 37.0, 277.75)
```

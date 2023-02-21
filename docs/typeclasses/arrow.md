# Arrow

API Documentation: @:api(cats.arrow.Arrow)

`Arrow` is a type class for modeling composable relationships between two types. One example of such a composable relationship is function `A => B`; other examples include `cats.data.Kleisli`(wrapping an `A => F[B]`, also known as `ReaderT`), and `cats.data.Cokleisli`(wrapping an `F[A] => B`). These type constructors all have `Arrow` instances. An arrow `F[A, B]` can be thought of as representing a computation from `A` to `B` with some context, just like a functor/applicative/monad `F[A]` represents a value `A` with some context.

Having an `Arrow` instance for a type constructor `F[_, _]` means that an `F[_, _]` can be composed and combined with other `F[_, _]`s. You will be able to do things like:

- Lifting a function `ab: A => B` into arrow `F[A, B]` with `Arrow[F].lift(ab)`. If `F` is `Function1` then `A => B` is the same as `F[A, B]` so `lift` is just the identity function.
- Composing `fab: F[A, B]` and `fbc: F[B, C]` into `fac: F[A, C]` with `Arrow[F].compose(fbc, fab)`, or `fab >>> fbc`. If `F` is `Function1` then `>>>` becomes an alias for `andThen`.
- Taking two arrows `fab: F[A, B]` and `fcd: F[C, D]` and combining them into `F[(A, C), (B, D)]` with `fab.split(fcd)` or `fab *** fcd`. The resulting arrow takes two inputs and processes them with two arrows, one for each input.
- Taking an arrow `fab: F[A, B]` and turning it into `F[(A, C), (B, C)]` with `fab.first`. The resulting arrow takes two inputs, processes the first input and leaves the second input as it is. A similar method, `fab.second`, turns `F[A, B]` into `F[(C, A), (C, B)]`.

## Examples

### `Function1`

`scala.Function1` has an `Arrow` instance, so you can use all the above methods on `Function1`. The Scala standard library has the `compose` and `andThen` methods for composing `Function1`s, but the `Arrow` instance offers more powerful options.

Suppose we want to write a function `meanAndVar`, that takes a `List[Int]` and returns the pair of mean and variance. To do so, we first define a `combine` function that combines two arrows into a single arrow, which takes an input and processes two copies of it with two arrows. `combine` can be defined in terms of `Arrow` operations `lift`, `>>>` and `***`:

```scala mdoc:silent
import cats.arrow.Arrow
import cats.syntax.all._

def combine[F[_, _]: Arrow, A, B, C](fab: F[A, B], fac: F[A, C]): F[A, (B, C)] =
  Arrow[F].lift((a: A) => (a, a)) >>> (fab *** fac)
```

We can then create functions `mean: List[Int] => Double`, `variance: List[Int] => Double` and `meanAndVar: List[Int] => (Double, Double)` using the `combine` method and `Arrow` operations:

```scala mdoc:silent
val mean: List[Int] => Double =
    combine((_: List[Int]).sum, (_: List[Int]).size) >>> {case (x, y) => x.toDouble / y}

val variance: List[Int] => Double =
  // Variance is mean of square minus square of mean
  combine(((_: List[Int]).map(x => x * x)) >>> mean, mean) >>> {case (x, y) => x - y * y}

val meanAndVar: List[Int] => (Double, Double) = combine(mean, variance)
```

```scala mdoc
meanAndVar(List(1, 2, 3, 4))
```

Of course, a more natural way to implement `mean` and `variance` would be:

```scala mdoc:silent
val mean2: List[Int] => Double = xs => xs.sum.toDouble / xs.size

val variance2: List[Int] => Double = xs => mean2(xs.map(x => x * x)) - scala.math.pow(mean2(xs), 2.0)
```

However, `Arrow` methods are more general and provide a common structure for type constructors that have `Arrow` instances. They are also a more abstract way of stitching computations together.


### `Kleisli`

A `Kleisli[F[_], A, B]` represents a function `A => F[B]`. You cannot directly compose an `A => F[B]` with a `B => F[C]` with functional composition, since the codomain of the first function is `F[B]` while the domain of the second function is `B`; however, since `Kleisli` is an arrow (as long as `F` is a monad), you can easily compose `Kleisli[F[_], A, B]` with `Kleisli[F[_], B, C]` using `Arrow` operations.


Suppose you want to take a `List[Int]`, and return the sum of the first and the last element (if exists). To do so, we can create two `Kleisli`s that find the `headOption` and `lastOption` of a `List[Int]`, respectively:

```scala mdoc:silent
import cats.data.Kleisli

val headK = Kleisli((_: List[Int]).headOption)
val lastK = Kleisli((_: List[Int]).lastOption)
```

With `headK` and `lastK`, we can obtain the `Kleisli` arrow we want by combining them, and composing it with `_ + _`:

```scala mdoc:silent
val headPlusLast = combine(headK, lastK) >>> Arrow[Kleisli[Option, *, *]].lift(((_: Int) + (_: Int)).tupled)
```

```scala mdoc
headPlusLast.run(List(2, 3, 5, 8))
headPlusLast.run(Nil)
```

### `FancyFunction`

In this example let's create our own `Arrow`. We shall create a fancy version of `Function1` called `FancyFunction`, that is capable of maintaining states. We then create an `Arrow` instance for `FancyFunction` and use it to compute the moving average of a list of numbers.

```scala mdoc:silent
case class FancyFunction[A, B](run: A => (FancyFunction[A, B], B))
```

That is, given an `A`, it not only returns a `B`, but also returns a new `FancyFunction[A, B]`. This sounds similar to the `State` monad (which returns a result and a new `State` from an initial `State`), and indeed, `FancyFunction` can be used to perform stateful transformations.

To run a stateful computation using a `FancyFunction` on a list of inputs, and collect the output into another list, we can define the following `runList` helper function:

```scala mdoc:silent
def runList[A, B](ff: FancyFunction[A, B], as: List[A]): List[B] = as match {
  case h :: t =>
    val (ff2, b) = ff.run(h)
    b :: runList(ff2, t)
  case _ => List()
}
```

In `runList`, the head element in `List[A]` is fed to `ff`, and each subsequent element is fed to a `FancyFunction` which is generated by running the previous `FancyFunction` on the previous element. If we have an `as: List[Int]`, and an `avg: FancyFunction[Int, Double]` which takes an integer and computes the average of all integers it has seen so far, we can call `runList(avg, as)` to get the list of moving average of `as`.

Next let's create an `Arrow` instance for `FancyFunction` and see how to implement the `avg` arrow. To create an `Arrow` instance for a type `F[A, B]`, the following abstract methods need to be implemented:

```
def lift[A, B](f: A => B): F[A, B]

def id[A]: F[A, A]

def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]

def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]
```

Thus the `Arrow` instance for `FancyFunction` would be:


```scala mdoc:silent
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

Once we have an `Arrow[FancyFunction]`, we can start to do interesting things with it. First, let's create a method `accum` that returns a `FancyFunction`, which accumulates values fed to it using the accumulation function `f` and the starting value `b`:

```scala mdoc:silent
def accum[A, B](b: B)(f: (A, B) => B): FancyFunction[A, B] = FancyFunction {a =>
  val b2 = f(a, b)
  (accum(b2)(f), b2)
}
```

```scala mdoc
runList(accum[Int, Int](0)(_ + _), List(6, 5, 4, 3, 2, 1))
```

To make the aformentioned `avg` arrow, we need to keep track of both the count and the sum of the numbers we have seen so far. To do so, we will combine several `FancyFunction`s to get the `avg` arrow we want.

We first define arrow `sum` in terms of `accum`, and define arrow `count` by composing `_ => 1` with `sum`:

```scala mdoc:silent
import cats.kernel.Monoid

def sum[A: Monoid]: FancyFunction[A, A] = accum(Monoid[A].empty)(_ |+| _)
def count[A]: FancyFunction[A, Int] = Arrow[FancyFunction].lift((_: A) => 1) >>> sum
```

Finally, we create the `avg` arrow in terms of the arrows we have so far:

```scala mdoc:silent
def avg: FancyFunction[Int, Double] =
  combine(sum[Int], count[Int]) >>> Arrow[FancyFunction].lift{case (x, y) => x.toDouble / y}
```

```scala mdoc
runList(avg, List(1, 10, 100, 1000))
```

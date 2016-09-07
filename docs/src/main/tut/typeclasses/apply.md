---
layout: docs
title:  "Apply"
section: "typeclasses"
source: "core/src/main/scala/cats/Apply.scala"
scaladoc: "#cats.Apply"
---
# Apply

`Apply` extends the [`Functor`](functor.html) type class (which features the familiar `map`
function) with a new function `ap`. The `ap` function is similar to `map`
in that we are transforming a value in a context (a context being the `F` in `F[A]`;
a context can be `Option`, `List` or `Future` for example).
However, the difference between `ap` and `map` is that for `ap` the function that
takes care of the transformation is of type `F[A => B]`, whereas for `map` it is `A => B`:

```tut:silent
import cats._

val intToString: Int => String = _.toString
val double: Int => Int = _ * 2
val addTwo: Int => Int = _ + 2

implicit val optionApply: Apply[Option] = new Apply[Option] {
  def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] =
    fa.flatMap (a => f.map (ff => ff(a)))

  def map[A,B](fa: Option[A])(f: A => B): Option[B] = fa map f
}

implicit val listApply: Apply[List] = new Apply[List] {
  def ap[A, B](f: List[A => B])(fa: List[A]): List[B] =
    fa.flatMap (a => f.map (ff => ff(a)))

  def map[A,B](fa: List[A])(f: A => B): List[B] = fa map f
}
```

### map

Since `Apply` extends `Functor`, we can use the `map` method from `Functor`:

```tut:book
Apply[Option].map(Some(1))(intToString)
Apply[Option].map(Some(1))(double)
Apply[Option].map(None)(double)
```

### compose

And like functors, `Apply` instances also compose (via the `Nested` data type):

```tut:book
import cats.data.Nested
val listOpt = Nested[List, Option, Int](List(Some(1), None, Some(3)))
val plusOne = (x:Int) => x + 1
val f = Nested[List, Option, Int => Int](List(Some(plusOne)))
Apply[Nested[List, Option, ?]].ap(f)(listOpt)
```

### ap
The `ap` method is a method that `Functor` does not have:

```tut:book
Apply[Option].ap(Some(intToString))(Some(1))
Apply[Option].ap(Some(double))(Some(1))
Apply[Option].ap(Some(double))(None)
Apply[Option].ap(None)(Some(1))
Apply[Option].ap(None)(None)
```

### ap2, ap3, etc

`Apply` also offers variants of `ap`. The functions `apN` (for `N` between `2` and `22`)
accept `N` arguments where `ap` accepts `1`:

For example:

```tut:book
val addArity2 = (a: Int, b: Int) => a + b
Apply[Option].ap2(Some(addArity2))(Some(1), Some(2))

val addArity3 = (a: Int, b: Int, c: Int) => a + b + c
Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3))
```

Note that if any of the arguments of this example is `None`, the
final result is `None` as well.  The effects of the context we are operating on
are carried through the entire computation:

```tut:book
Apply[Option].ap2(Some(addArity2))(Some(1), None)
Apply[Option].ap4(None)(Some(1), Some(2), Some(3), Some(4))
```

### map2, map3, etc

Similarly, `mapN` functions are available:

```tut:book
Apply[Option].map2(Some(1), Some(2))(addArity2)

Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3)
```

### tuple2, tuple3, etc

And `tupleN`:

```tut:book
Apply[Option].tuple2(Some(1), Some(2))

Apply[Option].tuple3(Some(1), Some(2), Some(3))
```

## apply builder syntax

The `|@|` operator offers an alternative syntax for the higher-arity `Apply`
functions (`apN`, `mapN` and `tupleN`).
In order to use it, first import `cats.syntax.all._` or `cats.syntax.cartesian._`.
Here we see that the following two functions, `f1` and `f2`, are equivalent:

```tut:book
import cats.implicits._

def f1(a: Option[Int], b: Option[Int], c: Option[Int]) =
  (a |@| b |@| c) map { _ * _ * _ }
def f2(a: Option[Int], b: Option[Int], c: Option[Int]) =
  Apply[Option].map3(a, b, c)(_ * _ * _)

f1(Some(1), Some(2), Some(3))

f2(Some(1), Some(2), Some(3))
```

All instances created by `|@|` have `map`, `ap`, and `tupled` methods of the appropriate arity:

```tut:book
val option2 = Option(1) |@| Option(2)
val option3 = option2 |@| Option.empty[Int]

option2 map addArity2
option3 map addArity3

option2 apWith Some(addArity2)
option3 apWith Some(addArity3)

option2.tupled
option3.tupled
```

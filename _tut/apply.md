---
layout: default
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

```scala
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

```scala
scala> Apply[Option].map(Some(1))(intToString)
res3: Option[String] = Some(1)

scala> Apply[Option].map(Some(1))(double)
res4: Option[Int] = Some(2)

scala> Apply[Option].map(None)(double)
res5: Option[Int] = None
```

### compose

And like functors, `Apply` instances also compose:

```scala
scala> val listOpt = Apply[List] compose Apply[Option]
listOpt: cats.Apply[[X]List[Option[X]]] = cats.Apply$$anon$1@13d2b5b6

scala> val plusOne = (x:Int) => x + 1
plusOne: Int => Int = <function1>

scala> listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3)))
res6: List[Option[Int]] = List(Some(2), None, Some(4))
```

### ap
The `ap` method is a method that `Functor` does not have:

```scala
scala> Apply[Option].ap(Some(intToString))(Some(1))
res7: Option[String] = Some(1)

scala> Apply[Option].ap(Some(double))(Some(1))
res8: Option[Int] = Some(2)

scala> Apply[Option].ap(Some(double))(None)
res9: Option[Int] = None

scala> Apply[Option].ap(None)(Some(1))
res10: Option[Nothing] = None

scala> Apply[Option].ap(None)(None)
res11: Option[Nothing] = None
```

### ap2, ap3, etc

`Apply` also offers variants of `ap`. The functions `apN` (for `N` between `2` and `22`) 
accept `N` arguments where `ap` accepts `1`:

For example:

```scala
scala> val addArity2 = (a: Int, b: Int) => a + b
addArity2: (Int, Int) => Int = <function2>

scala> Apply[Option].ap2(Some(addArity2))(Some(1), Some(2))
res12: Option[Int] = Some(3)

scala> val addArity3 = (a: Int, b: Int, c: Int) => a + b + c
addArity3: (Int, Int, Int) => Int = <function3>

scala> Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3))
res13: Option[Int] = Some(6)
```

Note that if any of the arguments of this example is `None`, the
final result is `None` as well.  The effects of the context we are operating on
are carried through the entire computation:

```scala
scala> Apply[Option].ap2(Some(addArity2))(Some(1), None)
res14: Option[Int] = None

scala> Apply[Option].ap4(None)(Some(1), Some(2), Some(3), Some(4))
res15: Option[Nothing] = None
```

### map2, map3, etc

Similarly, `mapN` functions are available:

```scala
scala> Apply[Option].map2(Some(1), Some(2))(addArity2)
res16: Option[Int] = Some(3)

scala> Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3)
res17: Option[Int] = Some(6)
```

### tuple2, tuple3, etc

And `tupleN`:

```scala
scala> Apply[Option].tuple2(Some(1), Some(2))
res18: Option[(Int, Int)] = Some((1,2))

scala> Apply[Option].tuple3(Some(1), Some(2), Some(3))
res19: Option[(Int, Int, Int)] = Some((1,2,3))
```

## apply builder syntax

The `|@|` operator offers an alternative syntax for the higher-arity `Apply`
functions (`apN`, `mapN` and `tupleN`).
In order to use it, first import `cats.syntax.all._` or `cats.syntax.cartesian._`.
Here we see that the following two functions, `f1` and `f2`, are equivalent:

```scala
scala> import cats.syntax.cartesian._
import cats.syntax.cartesian._

scala> def f1(a: Option[Int], b: Option[Int], c: Option[Int]) =
     |   (a |@| b |@| c) map { _ * _ * _ }
f1: (a: Option[Int], b: Option[Int], c: Option[Int])Option[Int]

scala> def f2(a: Option[Int], b: Option[Int], c: Option[Int]) =
     |   Apply[Option].map3(a, b, c)(_ * _ * _)
f2: (a: Option[Int], b: Option[Int], c: Option[Int])Option[Int]

scala> f1(Some(1), Some(2), Some(3))
res20: Option[Int] = Some(6)

scala> f2(Some(1), Some(2), Some(3))
res21: Option[Int] = Some(6)
```

All instances created by `|@|` have `map`, `ap`, and `tupled` methods of the appropriate arity:

```scala
scala> val option2 = Option(1) |@| Option(2)
option2: cats.syntax.CartesianBuilder[Option]#CartesianBuilder2[Int,Int] = cats.syntax.CartesianBuilder$CartesianBuilder2@53986986

scala> val option3 = option2 |@| Option.empty[Int]
option3: cats.syntax.CartesianBuilder[Option]#CartesianBuilder3[Int,Int,Int] = cats.syntax.CartesianBuilder$CartesianBuilder3@654e3b57

scala> option2 map addArity2
res22: Option[Int] = Some(3)

scala> option3 map addArity3
res23: Option[Int] = None

scala> option2 apWith Some(addArity2)
res24: Option[Int] = Some(3)

scala> option3 apWith Some(addArity3)
res25: Option[Int] = None

scala> option2.tupled
res26: Option[(Int, Int)] = Some((1,2))

scala> option3.tupled
res27: Option[(Int, Int, Int)] = None
```

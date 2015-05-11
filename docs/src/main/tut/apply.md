---
layout: default
title:  "Apply"
section: "typeclasses"
source: "https://github.com/non/cats/blob/master/core/src/main/scala/cats/Apply.scala"
scaladoc: "#cats.Apply"
---
# Apply

Apply extends the Functor typeclass (which features the familiar "map"
function) with a new function "ap".  The ap function is similar to map
in that we are transforming a value in a context, e.g. F[A] where F is
the context (e.g. Option, List, Future) and A is the type of the
value.  But the function A => B is now in the context itself,
e.g. F[A => B] such as Option[A => B] or List[A => B].

```tut
import cats._
val intToString: Int => String = _.toString
val double: Int => Int = _ * 2
val addTwo: Int => Int = _ + 2

implicit val optionApply: Apply[Option] = new Apply[Option] {
  def ap[A, B](fa: Option[A])(f: Option[A => B]): Option[B] =
    fa.flatMap (a => f.map (ff => ff(a)))

  def map[A,B](fa: Option[A])(f: A => B) = fa map f
}

implicit val listApply: Apply[List] = new Apply[List] {
  def ap[A, B](fa: List[A])(f: List[A => B]): List[B] =
    fa.flatMap (a => f.map (ff => ff(a)))

  def map[A,B](fa: List[A])(f: A => B) = fa map f
}
```

### map

Since Apply extends Functor, as we expect, we can use the map method
from Functor:

```tut
Apply[Option].map(Some(1))(intToString)
Apply[Option].map(Some(1))(double)
Apply[Option].map(None)(double)
```


### apply
But also the new apply method, which applies functions from the functor

```tut
Apply[Option].ap(Some(1))(Some(intToString))
Apply[Option].ap(Some(1))(Some(double))
Apply[Option].ap(None)(Some(double))
Apply[Option].ap(Some(1))(None)
Apply[Option].ap(None)(None)
```

### ap3, etc

Apply's ap function made it possible to build useful functions that
"lift" a function that takes multiple arguments into a context.

For example:

```tut
val add2 = (a: Int, b: Int) => a + b
Apply[Option].ap2(Some(1), Some(2))(Some(add2))
```

Interestingly, if any of the arguments of this example are None, the
final result is None.  The effects of the context we are operating on
are carried through the entire computation.

```tut
Apply[Option].ap2(Some(1), None)(Some(add2))
Apply[Option].ap2(Some(1), Some(2))(None)
```

## apply builder syntax

The `|@|` operator offers an alternative syntax for the higher-arity `Apply` functions (`apN`, `mapN`).
First, import `cats.syntax.all._` or `cats.syntax.apply._`. Here we see that following two functions, `f1` and `f2`, are equivalent:

```tut
import cats.syntax.apply._

def f1(a: Option[Int], b: Option[Int], c: Option[Int]) =
  (a |@| b |@| c) map { _ * _ * _ }
def f2(a: Option[Int], b: Option[Int], c: Option[Int]) =
  Apply[Option].map3(a, b, c)(_ * _ * _)

f1(Some(1), Some(2), Some(3))

f2(Some(1), Some(2), Some(3))
```

All instances created by `|@|` have `map`, `ap`, and `tupled` methods of the appropriate arity.

## composition

Like Functors, Apply instances also compose:

```tut
val listOpt = Apply[List] compose Apply[Option]
val plusOne = (x:Int) => x + 1
listOpt.ap(List(Some(1), None, Some(3)))(List(Some(plusOne)))
```

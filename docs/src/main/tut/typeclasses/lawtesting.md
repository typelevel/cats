---
layout: docs
title:  "Law Testing"
section: "typeclasses"
---

# Law testing

[Laws](https://typelevel.org/cats/typeclasses.html#laws) are an important part of cats.
Cats uses `catalysts` and `discipline` to help test instances with laws.
To make things easier, cats ships with `cats-testkit`, which makes use of `catalysts` and `discipline` and exposes `CatsSuite` based on ScalaTest.


## Getting started

First up, you will need to specify dependencies on `cats-laws` and `cats-testkit` in your `build.sbt` file.
To make things easier, we'll also include the `scalacheck-shapeless` library in this tutorial, so we don't have to manually write instances for ScalaCheck's `Arbitrary`.

```scala
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "1.0.1" % Test,
  "org.typelevel" %% "cats-testkit" % "1.0.1"% Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
)
```

## Example: Testing a Functor instance

We'll begin by creating a data type and its Functor instance.
```tut:book
import cats._

sealed trait Tree[+A]
case object Leaf extends Tree[Nothing]
case class Node[A](p: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  implicit val functorTree: Functor[Tree] = new Functor[Tree] {
    def map[A, B](tree: Tree[A])(f: A => B) = tree match {
      case Leaf => Leaf
      case Node(p, left, right) => Node(f(p), map(left)(f), map(right)(f))
    }
  }
}
```
We will also need to create an `Eq` instance, as most laws will need to compare values of a type to properly test for correctness.
For simplicity we'll just use `Eq.fromUniversalEquals`:

```tut:book
implicit def eqTree[A: Eq]: Eq[Tree[A]] = Eq.fromUniversalEquals
```

Then we can begin to write our law tests. Start by creating a new class in your `test` folder and inheriting from `cats.tests.CatsSuite`.
`CatsSuite` extends the standard ScalaTest `FunSuite` as well as `Matchers`.
Furthermore it also pulls in all of cats instances and syntax, so there's no need to import from `cats.implicits._`.

```tut:book
import cats.tests.CatsSuite

class TreeLawTests extends CatsSuite {

}
```

The key to testing laws is the `checkAll` function, which takes a name for your test and a Discipline ruleset.
Cats has defined rulesets for all type class laws in `cats.laws.discipline.*`.

So for our example we will want to import `cats.laws.discipline.FunctorTests` and call `checkAll` with it.
Before we do so, however,
we will have to bring our instances into scope as well as the derived `Arbitrary` instances from `scalacheck-shapeless`
(We have defined an Arbitrary instance for `Tree` here, but you won't need it if you import `org.scalacheck.ScalacheckShapeless._`).



```tut:silent

import org.scalacheck.{Arbitrary, Gen}

implicit def arbFoo[A: Arbitrary]: Arbitrary[Tree[A]] =
  Arbitrary(Gen.oneOf(Gen.const(Leaf), (for {
      e <- Arbitrary.arbitrary[A]
    } yield Node(e, Leaf, Leaf)))
  )
```

```tut:book
import Tree._

import cats.laws.discipline.FunctorTests

class TreeLawTests extends CatsSuite {
  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
}
```

Now when we run `test` in our sbt console, ScalaCheck will test if the `Functor` laws hold for our `Tree` type.
You should see something like this:

```
[info] TreeLawTests:
[info] - Tree.FunctorLaws.functor.covariant composition
[info] - Tree.FunctorLaws.functor.covariant identity
[info] - Tree.FunctorLaws.functor.invariant composition
[info] - Tree.FunctorLaws.functor.invariant identity
[info] ScalaTest
[info] Run completed in 537 milliseconds.
[info] Total number of tests run: 4
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 4, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[info] Passed: Total 4, Failed 0, Errors 0, Passed 4
[success] Total time: 1 s, completed Aug 31, 2017 2:19:22 PM
```

And voila, you've successfully proven that your data type upholds the Functor laws!

### Testing cats.kernel instances

For most of the type classes included in cats, the above will work great.
However, the law tests for the type classes inside `cats.kernel` are located in `cats.kernel.laws.discipline.*` instead.
So we have to import from there to test type classes like `Semigroup`, `Monoid`, `Group` or `Semilattice`.

Let's test it out, by defining a `Semigroup` instance for our `Tree` type.

```tut:book
import cats.implicits._

implicit def semigroupTree[A: Semigroup]: Semigroup[Tree[A]] = new Semigroup[Tree[A]] {
  def combine(x: Tree[A], y: Tree[A]) = (x, y) match {
    case (Leaf, _) => Leaf
    case (_, Leaf) => Leaf
    case (Node(xp, xLeft, xRight), Node(yp, yLeft, yRight)) =>
      Node(xp |+| yp, xLeft |+| yLeft, xRight |+| yRight)
  }
}
```

Then we can again test the instance inside our class extending `CatsSuite`:

```tut:book
import cats.laws.discipline.FunctorTests
import cats.kernel.laws.discipline.SemigroupTests

class TreeLawTests extends CatsSuite {
  checkAll("Tree[Int].SemigroupLaws", SemigroupTests[Tree[Int]].semigroup)
  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
}
```

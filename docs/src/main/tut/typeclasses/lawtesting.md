---
layout: docs
title:  "Law Testing"
section: "typeclasses"
---

# Law testing

[Laws](https://typelevel.org/cats/typeclasses.html#laws) are an important part of cats.
Cats uses [discipline](https://github.com/typelevel/discipline) to define type class laws and 
the [ScalaCheck](https://github.com/rickynils/scalacheck) tests based on them.

To test type class laws from Cats against your instances, you need to add a `cats-laws` dependency. 
If you are using `ScalaTest`, Cats also ships with optional `cats-testkit`, which provites a convenient
base test class `CatsSuite`.


## Getting started

First up, you will need to specify dependencies on `cats-laws` in your `build.sbt` file (or `cats-testkit` if you 
are using `ScalaTest`).
To make things easier, we'll also include the `scalacheck-shapeless` library in this tutorial, so we don't have to manually write instances for ScalaCheck's `Arbitrary`.

```scala
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "1.1.0" % Test, //or `cats-testkit` if you are using ScalaTest
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
```tut:invisible
import Tree._ //there is no real companion object is in REPL
```

Cats defines all type class laws tests in `cats.laws.discipline.*` 
as `discipline`'s `RuleSet`s. Each `RuleSet` provides a `ScalaCheck` `Properties` through
`ruleSet.all` to represent all the rules that it defines and inherits. For example, 
the `ScalaCheck` `Properties` for `Functor` can be retrieved using

```scala
cats.laws.discipline.FunctorTests[Tree].functor[Int, Int, String].all
```

We will also need to create an `Eq` instance, as most laws will need to compare values of a type to properly test for correctness.
For simplicity we'll just use `Eq.fromUniversalEquals`:

```tut:book
implicit def eqTree[A: Eq]: Eq[Tree[A]] = Eq.fromUniversalEquals
```
ScalaCheck requires `Arbitrary` instances for data types being tested. We have defined an `Arbitrary` instance for `Tree` here,
but you won't need it if you import `org.scalacheck.ScalacheckShapeless._`).

```tut:silent

import org.scalacheck.{Arbitrary, Gen}

implicit def arbFoo[A: Arbitrary]: Arbitrary[Tree[A]] =
  Arbitrary(Gen.oneOf(Gen.const(Leaf), (for {
      e <- Arbitrary.arbitrary[A]
    } yield Node(e, Leaf, Leaf)))
  )
```


Now we can convert these `ScalaCheck` `Properties` into tests that the test framework can run.

[discipline](https://github.com/typelevel/discipline) provides two helper `checkAll` functions that perform 
this conversion for two test frameworks: `ScalaTest` and `Spec2`. 

If you are using `Specs2`, let the test class extend `org.typelevel.discipline.specs2.Discipline`
which provides the `checkAll` function. 

If you are using `ScalaTest`, let the test class extend `org.typelevel.discipline.scalatest.Discipline`, or 
inherit from the more convenient `cats.tests.CatsSuite` from `cats-testkit`. 
`CatsSuite` extends the standard ScalaTest `FunSuite`, `Matchers` together with
`org.typelevel.discipline.scalatest.Discipline`. Furthermore it also pulls in all of cats instances and syntax,
so there's no need to import from `cats.implicits._`.

For other test frameworks, you need to resort to their integration with `ScalaCheck` to test
the `ScalaCheck` `Properties` provided by cats-laws. 

So here is the Scalatest test for our example, basically we import `cats.laws.discipline.FunctorTests` and 
call the `checkAll` helper with it.

```tut:book
import Tree._
import cats.tests.CatsSuite	
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

# Law testing

[Laws](../typeclasses.md#laws) are an important part of cats.
Cats uses [discipline](https://github.com/typelevel/discipline) to define type class laws and
the [ScalaCheck](https://github.com/rickynils/scalacheck) tests based on them.

To test type class laws from Cats against your instances, you need to add a `cats-laws` dependency.

## Getting started

First up, you will need to specify dependencies on `cats-laws` in your `build.sbt` file.
To make things easier, we'll also include the `scalacheck-shapeless` library in this tutorial, so we don't have to manually write instances for ScalaCheck's `Arbitrary`.

```scala
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "2.0.0" % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test
)
```


## Example: Testing a Functor instance

We'll begin by creating a data type and its Functor instance.
```scala mdoc
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
```scala mdoc:invisible
import Tree._ //there is no real companion object in REPL
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

```scala mdoc
implicit def eqTree[A: Eq]: Eq[Tree[A]] = Eq.fromUniversalEquals
```
ScalaCheck requires `Arbitrary` instances for data types being tested. We have defined an `Arbitrary` instance for `Tree` here,
but you won't need it if you import `org.scalacheck.ScalacheckShapeless._`).

```scala mdoc:silent

import org.scalacheck.{Arbitrary, Gen}

object arbitraries {
  implicit def arbTree[A: Arbitrary]: Arbitrary[Tree[A]] =
    Arbitrary(Gen.oneOf(Gen.const(Leaf), (for {
      e <- Arbitrary.arbitrary[A]
    } yield Node(e, Leaf, Leaf)))
  )
}
```

Now we can convert these `ScalaCheck` `Properties` into tests that the test framework can run.  [discipline](https://github.com/typelevel/discipline) provides a helper `checkAll` function that performs
this conversion for three test frameworks: `ScalaTest`, `Specs2` and `MUnit`.

* If you are using `Specs2`, extend your test class with `org.typelevel.discipline.specs2.Discipline` (provided by `discipline-specs2`).

* If you are using `ScalaTest`, extend your test class with `org.typelevel.discipline.scalatest.FunSuiteDiscipline` (provided by `discipline-scalatest`) and `org.scalatest.funsuite.AnyFunSuiteLike`.

* If you are using `MUnit`, extend your test class with `munit.DisciplineSuite` (provided by `discipline-munit`).

* For other test frameworks, you need to resort to their integration with `ScalaCheck` to test
the `ScalaCheck` `Properties` provided by `cats-laws`.

The following example is for MUnit.

```scala mdoc
import cats.syntax.all._
import cats.laws.discipline.FunctorTests
import munit.DisciplineSuite
import arbitraries._

class TreeLawTests extends DisciplineSuite {
  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
}
```

* `cats.implicits._` imports the instances we need for `Eq[Tree[Int]]`, which the laws use to compare trees.
* `FunctorTests` contains the functor laws.
* `AnyFunSuite` defines the style of ScalaTest.
* `FunSuiteDiscipline` provides `checkAll`, and must be mixed into `AnyFunSuite`
* `arbitraries._` imports the `Arbitrary[Tree[_]]` instances needed to check the laws.

Now when we run `test` in our sbt console, ScalaCheck will test if the `Functor` laws hold for our `Tree` type.
You should see something like this:

```
[info] TreeLawTests:
[info] - Tree.FunctorLaws.functor.covariant composition (58 milliseconds)
[info] - Tree.FunctorLaws.functor.covariant identity (3 milliseconds)
[info] - Tree.FunctorLaws.functor.invariant composition (19 milliseconds)
[info] - Tree.FunctorLaws.functor.invariant identity (3 milliseconds)
[info] Passed: Total 4, Failed 0, Errors 0, Passed 4
[success] Total time: 2 s, completed Aug 2, 2019 12:01:17 AM
```

And voila, you've successfully proven that your data type upholds the Functor laws!

### Testing `cats.kernel` instances

For most of the type classes included in Cats, the above will work great.
However, the law tests for the type classes inside `cats.kernel` are located in `cats.kernel.laws.discipline.*` instead.
So we have to import from there to test type classes like `Semigroup`, `Monoid`, `Group` or `Semilattice`.

Let's test it out by defining a `Semigroup` instance for our `Tree` type.

```scala mdoc
import cats.syntax.all._

implicit def semigroupTree[A: Semigroup]: Semigroup[Tree[A]] = new Semigroup[Tree[A]] {
  def combine(x: Tree[A], y: Tree[A]) = (x, y) match {
    case (Leaf, _) => Leaf
    case (_, Leaf) => Leaf
    case (Node(xp, xLeft, xRight), Node(yp, yLeft, yRight)) =>
      Node(xp |+| yp, combine(xLeft, yLeft), combine(xRight, yRight))
  }
}
```

Then we can add the Semigroup tests to our suite:

```scala mdoc:nest
import cats.syntax.all._
import cats.kernel.laws.discipline.SemigroupTests
import cats.laws.discipline.FunctorTests
import munit.DisciplineSuite
import arbitraries._

class TreeLawTests extends DisciplineSuite {
  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
  checkAll("Tree[Int].SemigroupLaws", SemigroupTests[Tree[Int]].semigroup)
}
```

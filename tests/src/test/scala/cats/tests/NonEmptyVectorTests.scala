package cats
package tests

import cats.kernel.laws.{GroupLaws, OrderLaws}

import cats.data.NonEmptyVector
import cats.laws.discipline.{ComonadTests, FunctorTests, SemigroupKTests, FoldableTests, MonadTests, SerializableTests, CartesianTests, TraverseTests, ReducibleTests}
import cats.laws.discipline.arbitrary._

class NonEmptyVectorTests extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 5, minSuccessful = 20)

  checkAll("NonEmptyVector[Int]", OrderLaws[NonEmptyVector[Int]].eqv)

  checkAll("NonEmptyVector[Int] with Option", TraverseTests[NonEmptyVector].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[NonEmptyVector[A]]", SerializableTests.serializable(Traverse[NonEmptyVector]))

  checkAll("NonEmptyVector[Int]", ReducibleTests[NonEmptyVector].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptyVector]", SerializableTests.serializable(Reducible[NonEmptyVector]))

  implicit val iso = CartesianTests.Isomorphisms.invariant[NonEmptyVector](NonEmptyVector.catsDataFunctorForNonEmptyVector)

  // Test instances that have more general constraints
  {
    checkAll("NonEmptyVector[Int]", CartesianTests[NonEmptyVector].cartesian[Int, Int, Int])
    checkAll("Cartesian[NonEmptyVector]", SerializableTests.serializable(Cartesian[NonEmptyVector]))
  }

  {
    checkAll("NonEmptyVector[Int]", FunctorTests[NonEmptyVector].functor[Int, Int, Int])
    checkAll("Functor[NonEmptyVector]", SerializableTests.serializable(Functor[NonEmptyVector]))
  }

  {
    checkAll("NonEmptyVector[Int]", SemigroupKTests[NonEmptyVector].semigroupK[Int])
    checkAll("NonEmptyVector[Int]", GroupLaws[NonEmptyVector[Int]].semigroup)
    checkAll("SemigroupK[NonEmptyVector]", SerializableTests.serializable(SemigroupK[NonEmptyVector]))
    checkAll("Semigroup[NonEmptyVector[Int]]", SerializableTests.serializable(Semigroup[NonEmptyVector[Int]]))
  }

  {
    checkAll("NonEmptyVector[Int]", FoldableTests[NonEmptyVector].foldable[Int, Int])
    checkAll("Foldable[NonEmptyVector]", SerializableTests.serializable(Foldable[NonEmptyVector]))
  }

  {
    // Test functor and subclasses don't have implicit conflicts
    implicitly[Functor[NonEmptyVector]]
    implicitly[Monad[NonEmptyVector]]
    implicitly[Comonad[NonEmptyVector]]
  }

  {
    checkAll("NonEmptyVector[Int]", MonadTests[NonEmptyVector].monad[Int, Int, Int])
    checkAll("Monad[NonEmptyVector]", SerializableTests.serializable(Monad[NonEmptyVector]))
  }

  {
    checkAll("NonEmptyVector[Int]", ComonadTests[NonEmptyVector].comonad[Int, Int, Int])
    checkAll("Comonad[NonEmptyVector]", SerializableTests.serializable(Comonad[NonEmptyVector]))
  }
 

  test("size is consistent with toList.size") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector.size should === (nonEmptyVector.toList.size.toLong)
    }
  }
 

  test("Show is not empty and is formatted as expected") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector.show.nonEmpty should === (true)
      nonEmptyVector.show.startsWith("NonEmptyVector(") should === (true)
      nonEmptyVector.show should === (implicitly[Show[NonEmptyVector[Int]]].show(nonEmptyVector))
      nonEmptyVector.show.contains(nonEmptyVector.head.show) should === (true)
    }
  }

  test("Show is formatted correctly") {
    val nonEmptyVector = NonEmptyVector("Test", Vector.empty)
    nonEmptyVector.show should === ("NonEmptyVector(Test, Vector())")
  }

  test("Creating NonEmptyVector + unwrap is identity") {
    forAll { (i: Int, tail: Vector[Int]) =>
      val vector = i +: tail
      val nonEmptyVector = NonEmptyVector(i, tail)
      vector should === (nonEmptyVector.unwrap)
    }
  }

  test("NonEmptyVector#filter is consistent with NonEmptyVector#filter") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.unwrap
      nonEmptyVector.filter(p) should === (vector.filter(p))
    }
  }

  test("NonEmptyVector#find is consistent with NonEmptyVector#find") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.unwrap
      nonEmptyVector.find(p) should === (vector.find(p))
    }
  }

  test("NonEmptyVector#exists is consistent with NonEmptyVector#exists") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.unwrap
      nonEmptyVector.exists(p) should === (vector.exists(p))
    }
  }

  test("NonEmptyVector#forall is consistent with NonEmptyVector#forall") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.unwrap
      nonEmptyVector.forall(p) should === (vector.forall(p))
    }
  }

  test("NonEmptyVector#map is consistent with NonEmptyVector#map") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => String) =>
      val vector = nonEmptyVector.unwrap
      nonEmptyVector.map(p).unwrap should === (vector.map(p))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: (Int, Int) => Int) =>
      nonEmptyVector.reduceLeft(f) should === (nonEmptyVector.tail.foldLeft(nonEmptyVector.head)(f))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      nonEmptyVector.reduceRight(f).value should === (nonEmptyVector.tail.foldRight(nonEmptyVector.head)((a, b) => f(a, Now(b)).value))
    }
  }

  test("reduce consistent with fold") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector.reduce should === (nonEmptyVector.fold)
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nonEmptyVector: NonEmptyVector[Option[Int]]) =>
      nonEmptyVector.reduce(SemigroupK[Option].algebra[Int]) should === (nonEmptyVector.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nonEmptyVector.tail.foldLeft(Option(f(nonEmptyVector.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      nonEmptyVector.reduceLeftToOption(f)(g) should === (expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val expected = nonEmptyVector.tail.foldRight(Option(f(nonEmptyVector.head))) { (i, opt) =>
        opt.map(s => g(i, Now(s)).value)
      }
      nonEmptyVector.reduceRightToOption(f)(g).value should === (expected)
    }
  }
}

package cats
package tests

//import catalysts.Platform

import cats.kernel.laws.{GroupLaws, OrderLaws}

import cats.data.NonEmptyVector
import cats.laws.discipline.{ComonadTests, SemigroupKTests, FoldableTests, SerializableTests, TraverseTests, ReducibleTests, MonadRecTests}
import cats.laws.discipline.arbitrary._

//import scala.util.Properties

class NonEmptyVectorTests extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 5, minSuccessful = 20)

  checkAll("NonEmptyVector[Int]", OrderLaws[NonEmptyVector[Int]].eqv)

  checkAll("NonEmptyVector[Int] with Option", TraverseTests[NonEmptyVector].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[NonEmptyVector[A]]", SerializableTests.serializable(Traverse[NonEmptyVector]))

  checkAll("NonEmptyVector[Int]", ReducibleTests[NonEmptyVector].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptyVector]", SerializableTests.serializable(Reducible[NonEmptyVector]))


  // Test instances that have more general constraints

  checkAll("NonEmptyVector[Int]", SemigroupKTests[NonEmptyVector].semigroupK[Int])
  checkAll("NonEmptyVector[Int]", GroupLaws[NonEmptyVector[Int]].semigroup)
  checkAll("SemigroupK[NonEmptyVector]", SerializableTests.serializable(SemigroupK[NonEmptyVector]))
  checkAll("Semigroup[NonEmptyVector[Int]]", SerializableTests.serializable(Semigroup[NonEmptyVector[Int]]))



  checkAll("NonEmptyVector[Int]", FoldableTests[NonEmptyVector].foldable[Int, Int])
  checkAll("Foldable[NonEmptyVector]", SerializableTests.serializable(Foldable[NonEmptyVector]))



  // Test functor and subclasses don't have implicit conflicts
  implicitly[Functor[NonEmptyVector]]
  implicitly[Monad[NonEmptyVector]]
  implicitly[Comonad[NonEmptyVector]]



  checkAll("NonEmptyVector[Int]", ComonadTests[NonEmptyVector].comonad[Int, Int, Int])
  checkAll("Comonad[NonEmptyVector]", SerializableTests.serializable(Comonad[NonEmptyVector]))


  checkAll("NonEmptyVector[Int]", MonadRecTests[NonEmptyVector].monadRec[Int, Int, Int])
  checkAll("MonadRec[NonEmptyVector]", SerializableTests.serializable(MonadRec[NonEmptyVector]))


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
    val v1 = NonEmptyVector("Test", Vector.empty)
    v1.show should === ("NonEmptyVector(Test)")

    val v2 = NonEmptyVector("foo", "bar", "baz")
    v2.show should === ("NonEmptyVector(foo, bar, baz)")
  }

  test("Creating NonEmptyVector + toVector is identity") {
    forAll { (i: Int, tail: Vector[Int]) =>
      val vector = i +: tail
      val nonEmptyVector = NonEmptyVector(i, tail)
      vector should === (nonEmptyVector.toVector)
    }
  }

  test("NonEmptyVector#filter is consistent with Vector#filter") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.filter(p) should === (vector.filter(p))
    }
  }

  test("NonEmptyVector#find is consistent with Vector#find") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.find(p) should === (vector.find(p))
    }
  }

  test("NonEmptyVector#exists is consistent with Vector#exists") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.exists(p) should === (vector.exists(p))
    }
  }

  test("NonEmptyVector#forall is consistent with Vector#forall") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.forall(p) should === (vector.forall(p))
    }
  }

  test("NonEmptyVector#map is consistent with Vector#map") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => String) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.map(p).toVector should === (vector.map(p))
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

  test("fromVector returns None when the input vector is empty") {
    NonEmptyVector.fromVector(Vector.empty[Int]) should === (Option.empty[NonEmptyVector[Int]])
  }

  test("fromVectorUnsafe throws an exception when the input vector is empty") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptyVector.fromVectorUnsafe(Vector.empty[Int])
    }
  }

  test("++ Vector is consistent with concatNEV") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], other: NonEmptyVector[Int]) =>
      nonEmptyVector ++ other.toVector should === (nonEmptyVector.concatNEV(other))
    }
  }

  test("++ Vector is consistent with concat") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], vector: Vector[Int]) =>
      nonEmptyVector ++ vector should === (nonEmptyVector.concat(vector))
    }
  }

  test("NonEmptyVector#apply on varargs is consistent with NonEmptyVector#apply on Vector") {
    forAll { (head: Int, tail: Vector[Int]) =>
      NonEmptyVector(head, tail:_*) should === (NonEmptyVector(head, tail))
    }
  }

  test("NonEmptyVector#get returns a None when the element does not exist") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      val size = nonEmptyVector.toVector.size
      nonEmptyVector.get(size) should === (None)
    }
  }

  test("NonEmptyVector#getUnsafe throws an exception when the element does not exist") {
    forAll{ (nonEmptyVector: NonEmptyVector[Int]) =>
      val size = nonEmptyVector.toVector.size
      val _ = intercept[IndexOutOfBoundsException] {
        nonEmptyVector.getUnsafe(size)
      }
    }
  }

  test("NonEmptyVector#updated returns a None when the element does not exist") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], element: Int) =>
      val size = nonEmptyVector.toVector.size
      nonEmptyVector.updated(size, element) should === (None)
    }
  }

  test("NonEmptyVector#updatedUnsafe throws an exception when the element does not exist") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], element: Int) =>
      val size = nonEmptyVector.toVector.size
      val _ = intercept[IndexOutOfBoundsException] {
        nonEmptyVector.updatedUnsafe(size, element)
      }
    }
  }

  test("NonEmptyVector#hashCode consistent with Vector#hashCode") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector.hashCode should === (nonEmptyVector.toVector.hashCode)
    }
  }

  test("NonEmptyVector#equals consistent with Vector#equals") {
    forAll { (lhs: NonEmptyVector[Int], rhs: NonEmptyVector[Int]) =>
      lhs.equals(rhs) should === (lhs.toVector.equals(rhs.toVector))
    }
  }

  test("NonEmptyVector#toString produces correct output") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector.toString should === (s"NonEmpty${nonEmptyVector.toVector.toString}")
    }
    NonEmptyVector(1, Vector.empty).toString should === ("NonEmptyVector(1)")
    NonEmptyVector(1, Vector.empty).toVector.toString should === ("Vector(1)")
  }

  //test("Cannot create a new NonEmptyVector from constructor") {
  //  if(Platform.isJvm) {
  //    if (!Properties.versionNumberString.startsWith("2.10")) {
  //      // A bug in scala 2.10 allows private constructors to be accessed.
  //      // We should still ensure that on scala 2.11 and up we cannot construct the
  //      // object directly. see: https://issues.scala-lang.org/browse/SI-6601
  //      "val bad: NonEmptyVector[Int] = new NonEmptyVector(Vector(1))" shouldNot compile
  //    }
  //  }
  //}

  test("Cannot create a new NonEmptyVector from apply with an empty vector") {
    "val bad: NonEmptyVector[Int] = NonEmptyVector(Vector(1))" shouldNot compile
  }

}

class ReducibleNonEmptyVectorCheck extends ReducibleCheck[NonEmptyVector]("NonEmptyVector") {
  def iterator[T](nel: NonEmptyVector[T]): Iterator[T] = nel.toVector.iterator

  def range(start: Long, endInclusive: Long): NonEmptyVector[Long] = {
    // if we inline this we get a bewildering implicit numeric widening
    // error message in Scala 2.10
    val tailStart: Long = start + 1L
    NonEmptyVector(start, (tailStart).to(endInclusive).toVector)
  }
}

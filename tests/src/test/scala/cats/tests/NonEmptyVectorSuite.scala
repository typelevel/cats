package cats
package tests

import cats.data.NonEmptyVector.ZipNonEmptyVector

import cats.kernel.laws.discipline.{EqTests, SemigroupTests}

import cats.data.NonEmptyVector
import cats.laws.discipline.{
  BimonadTests,
  CommutativeApplyTests,
  FoldableTests,
  NonEmptyTraverseTests,
  ReducibleTests,
  SemigroupKTests,
  SerializableTests
}
import cats.laws.discipline.arbitrary._
import cats.platform.Platform

import scala.util.Properties

class NonEmptyVectorSuite extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 20, sizeRange = 5)

  checkAll("NonEmptyVector[Int]", EqTests[NonEmptyVector[Int]].eqv)

  checkAll("NonEmptyVector[Int] with Option",
           NonEmptyTraverseTests[NonEmptyVector].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
  checkAll("NonEmptyTraverse[NonEmptyVector[A]]", SerializableTests.serializable(NonEmptyTraverse[NonEmptyVector]))

  checkAll("NonEmptyVector[Int]", ReducibleTests[NonEmptyVector].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptyVector]", SerializableTests.serializable(Reducible[NonEmptyVector]))

  // Test instances that have more general constraints

  checkAll("NonEmptyVector[Int]", SemigroupKTests[NonEmptyVector].semigroupK[Int])
  checkAll("NonEmptyVector[Int]", SemigroupTests[NonEmptyVector[Int]].semigroup)
  checkAll("SemigroupK[NonEmptyVector]", SerializableTests.serializable(SemigroupK[NonEmptyVector]))
  checkAll("Semigroup[NonEmptyVector[Int]]", SerializableTests.serializable(Semigroup[NonEmptyVector[Int]]))

  checkAll("NonEmptyVector[Int]", FoldableTests[NonEmptyVector].foldable[Int, Int])
  checkAll("Foldable[NonEmptyVector]", SerializableTests.serializable(Foldable[NonEmptyVector]))

  checkAll("ZipNonEmptyVector[Int]", CommutativeApplyTests[ZipNonEmptyVector].commutativeApply[Int, Int, Int])
  checkAll("CommutativeApply[ZipNonEmptyVector]", SerializableTests.serializable(CommutativeApply[ZipNonEmptyVector]))

  // Test functor and subclasses don't have implicit conflicts
  implicitly[Functor[NonEmptyVector]]
  implicitly[Monad[NonEmptyVector]]
  implicitly[Comonad[NonEmptyVector]]
  implicitly[Bimonad[NonEmptyVector]]

  checkAll("NonEmptyVector[Int]", BimonadTests[NonEmptyVector].bimonad[Int, Int, Int])
  checkAll("Bimonad[NonEmptyVector]", SerializableTests.serializable(Bimonad[NonEmptyVector]))

  test("size is consistent with toList.size") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector.size should ===(nonEmptyVector.toList.size.toLong)
    }
  }

  test("Show is not empty and is formatted as expected") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector.show.nonEmpty should ===(true)
      nonEmptyVector.show.startsWith("NonEmptyVector(") should ===(true)
      nonEmptyVector.show should ===(implicitly[Show[NonEmptyVector[Int]]].show(nonEmptyVector))
      nonEmptyVector.show.contains(nonEmptyVector.head.show) should ===(true)
    }
  }

  test("Show is formatted correctly") {
    val v1 = NonEmptyVector("Test", Vector.empty)
    v1.show should ===("NonEmptyVector(Test)")

    val v2 = NonEmptyVector.of("foo", "bar", "baz")
    v2.show should ===("NonEmptyVector(foo, bar, baz)")
  }

  test("Creating NonEmptyVector + toVector is identity") {
    forAll { (i: Int, tail: Vector[Int]) =>
      val vector = i +: tail
      val nonEmptyVector = NonEmptyVector(i, tail)
      vector should ===(nonEmptyVector.toVector)
    }
  }

  test("NonEmptyVector#filter is consistent with Vector#filter") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.filter(p) should ===(vector.filter(p))
    }
  }

  test("NonEmptyVector#filterNot is consistent with Vector#filterNot") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.filterNot(p) should ===(vector.filterNot(p))
    }
  }

  test("NonEmptyVector#find is consistent with Vector#find") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.find(p) should ===(vector.find(p))
    }
  }

  test("NonEmptyVector#exists is consistent with Vector#exists") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.exists(p) should ===(vector.exists(p))
    }
  }

  test("NonEmptyVector#forall is consistent with Vector#forall") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.forall(p) should ===(vector.forall(p))
    }
  }

  test("NonEmptyVector#map is consistent with Vector#map") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => String) =>
      val vector = nonEmptyVector.toVector
      nonEmptyVector.map(p).toVector should ===(vector.map(p))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: (Int, Int) => Int) =>
      nonEmptyVector.reduceLeft(f) should ===(nonEmptyVector.tail.foldLeft(nonEmptyVector.head)(f))
    }
  }

  def excise[A](as: Vector[A]): (Vector[A], A) =
    (as.slice(0, as.size - 1), as.last)

  test("reduceRight consistent with foldRight") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nonEmptyVector.reduceRight(f).value
      val (first, last) = excise(nonEmptyVector.toVector)
      val expected = first.foldRight(last)((a, b) => f(a, Now(b)).value)
      got should ===(expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector.reduce should ===(nonEmptyVector.fold)
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nonEmptyVector: NonEmptyVector[Option[Int]]) =>
      nonEmptyVector.reduce(SemigroupK[Option].algebra[Int]) should ===(nonEmptyVector.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nonEmptyVector.tail.foldLeft(Option(f(nonEmptyVector.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      nonEmptyVector.reduceLeftToOption(f)(g) should ===(expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val (first, last) = excise(nonEmptyVector.toVector)
      val expected = first.foldRight(Option(f(last))) { (i, opt) =>
        opt.map(s => g(i, Now(s)).value)
      }
      nonEmptyVector.reduceRightToOption(f)(g).value should ===(expected)
    }
  }

  test("fromVector returns None when the input vector is empty") {
    NonEmptyVector.fromVector(Vector.empty[Int]) should ===(Option.empty[NonEmptyVector[Int]])
  }

  test("fromVectorUnsafe throws an exception when the input vector is empty") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptyVector.fromVectorUnsafe(Vector.empty[Int])
    }
  }

  test("++ Vector is consistent with concatNev") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], other: NonEmptyVector[Int]) =>
      nonEmptyVector ++ other.toVector should ===(nonEmptyVector.concatNev(other))
    }
  }

  test("++ Vector is consistent with concat") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], vector: Vector[Int]) =>
      nonEmptyVector ++ vector should ===(nonEmptyVector.concat(vector))
    }
  }

  test(":+ is consistent with concat") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], i: Int) =>
      nonEmptyVector :+ i should ===(nonEmptyVector.concat(Vector(i)))
    }
  }
  test("append is consistent with :+") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], i: Int) =>
      nonEmptyVector.append(i) should ===(nonEmptyVector :+ i)
    }
  }

  test("+: is consistent with concatNev") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], i: Int) =>
      i +: nonEmptyVector should ===(NonEmptyVector.one(i).concatNev(nonEmptyVector))
    }
  }
  test("prepend is consistent with +:") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], i: Int) =>
      nonEmptyVector.prepend(i) should ===(i +: nonEmptyVector)
    }
  }

  test("NonEmptyVector#of on varargs is consistent with NonEmptyVector#apply on Vector") {
    forAll { (head: Int, tail: Vector[Int]) =>
      NonEmptyVector.of(head, tail: _*) should ===(NonEmptyVector(head, tail))
    }
  }

  test("NonEmptyVector#get returns a None when the element does not exist") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      val size = nonEmptyVector.toVector.size
      nonEmptyVector.get(size) should ===(None)
    }
  }

  test("NonEmptyVector#getUnsafe throws an exception when the element does not exist") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      val size = nonEmptyVector.toVector.size
      val _ = intercept[IndexOutOfBoundsException] {
        nonEmptyVector.getUnsafe(size)
      }
    }
  }

  test("NonEmptyVector#updated returns a None when the element does not exist") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], element: Int) =>
      val size = nonEmptyVector.toVector.size
      nonEmptyVector.updated(size, element) should ===(None)
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
      nonEmptyVector.hashCode should ===(nonEmptyVector.toVector.hashCode)
    }
  }

  test("NonEmptyVector#equals consistent with Vector#equals") {
    forAll { (lhs: NonEmptyVector[Int], rhs: NonEmptyVector[Int]) =>
      lhs.equals(rhs) should ===(lhs.toVector.equals(rhs.toVector))
    }
  }

  test("NonEmptyVector#toString produces correct output") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector.toString should ===(s"NonEmpty${nonEmptyVector.toVector.toString}")
    }
    NonEmptyVector(1, Vector.empty).toString should ===("NonEmptyVector(1)")
    NonEmptyVector(1, Vector.empty).toVector.toString should ===("Vector(1)")
  }

  test("NonEmptyVector.unapply supports pattern matching") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector match {
        case NonEmptyVector(head, tail) =>
          head should ===(nonEmptyVector.head)
          tail should ===(nonEmptyVector.tail)
      }
    }
  }

  test("Cannot create a new NonEmptyVector from constructor") {
    if (Platform.isJvm) {
      if (!Properties.versionNumberString.startsWith("2.10")) {
        // A bug in scala 2.10 allows private constructors to be accessed.
        // We should still ensure that on scala 2.11 and up we cannot construct the
        // object directly. see: https://issues.scala-lang.org/browse/SI-6601
        "val bad: NonEmptyVector[Int] = new NonEmptyVector(Vector(1))" shouldNot compile
      }
    }
  }

  test("Cannot create a new NonEmptyVector[Int] from apply with a Vector[Int]") {
    "val bad: NonEmptyVector[Int] = NonEmptyVector(Vector(1))" shouldNot compile
  }

  test("Cannot create a new NonEmptyVector[Int] from apply with a an empty Vector") {
    "val bad: NonEmptyVector[Int] = NonEmptyVector(Vector.empty[Int])" shouldNot compile
  }

  test("NonEmptyVector#distinct is consistent with Vector#distinct") {
    forAll { nonEmptyVector: NonEmptyVector[Int] =>
      nonEmptyVector.distinct.toVector should ===(nonEmptyVector.toVector.distinct)
    }
  }

  test("NonEmptyVector#zipWith is consistent with Vector#zip and then Vector#map") {
    forAll { (a: NonEmptyVector[Int], b: NonEmptyVector[Int], f: (Int, Int) => Int) =>
      a.zipWith(b)(f).toVector should ===(a.toVector.zip(b.toVector).map { case (x, y) => f(x, y) })
    }
  }

  test("NonEmptyVector#zipWith is consistent with #zipWithIndex") {
    forAll { nev: NonEmptyVector[Int] =>
      val zw = nev.zipWith(NonEmptyVector.fromVectorUnsafe((0 until nev.length).toVector))(Tuple2.apply)
      nev.zipWithIndex should ===(zw)
    }
  }

  test("NonEmptyVector#nonEmptyPartition remains sorted") {
    forAll { (nev: NonEmptyVector[Int], f: Int => Either[String, String]) =>
      val sorted = NonEmptyVector.fromVectorUnsafe(nev.map(f).toVector.sorted)
      val ior = Reducible[NonEmptyVector].nonEmptyPartition(sorted)(identity)

      ior.left.map(xs => xs.sorted should ===(xs))
      ior.right.map(xs => xs.sorted should ===(xs))
    }
  }

  test("NonEmptyVector#last is consistent with Vector#last") {
    forAll { nonEmptyVector: NonEmptyVector[Int] =>
      nonEmptyVector.last should ===(nonEmptyVector.toVector.last)
    }
  }

  test("NonEmptyVector#init is consistent with Vector#init") {
    forAll { nonEmptyVector: NonEmptyVector[Int] =>
      nonEmptyVector.init should ===(nonEmptyVector.toVector.init)
    }
  }

  test("NonEmptyVector#collect is consistent with Vector#collect") {
    val pf: PartialFunction[Int, Double] = {
      case i if (i % 2 == 0) => i.toDouble
    }
    forAll { nonEmptyVector: NonEmptyVector[Int] =>
      nonEmptyVector.collect(pf) should ===(nonEmptyVector.toVector.collect(pf))
    }
  }

  test("NonEmptyVector#length and size is consistent with Vector#length") {
    forAll { nonEmptyVector: NonEmptyVector[Int] =>
      nonEmptyVector.length should ===(nonEmptyVector.toVector.length)
      nonEmptyVector.size should ===(nonEmptyVector.toVector.length.toLong)
    }
  }

  test("NonEmptyVector#reverse is consistent with Vector#reverse") {
    forAll { nonEmptyVector: NonEmptyVector[Int] =>
      nonEmptyVector.reverse should ===(NonEmptyVector.fromVectorUnsafe(nonEmptyVector.toVector.reverse))
    }
  }

  test("NonEmptyVector#zipWithIndex is consistent with Vector#zipWithIndex") {
    forAll { nonEmptyVector: NonEmptyVector[Int] =>
      val expected = NonEmptyVector.fromVectorUnsafe(nonEmptyVector.toVector.zipWithIndex)
      nonEmptyVector.zipWithIndex should ===(expected)
      Traverse[NonEmptyVector].zipWithIndex(nonEmptyVector) should ===(expected)
    }
  }

  test("NonEmptyVector#sorted and sortBy is consistent with Vector#sorted and sortBy") {
    forAll { nonEmptyVector: NonEmptyVector[Int] =>
      nonEmptyVector.sorted should ===(NonEmptyVector.fromVectorUnsafe(nonEmptyVector.toVector.sorted))
      nonEmptyVector.sortBy(i => -i) should ===(
        NonEmptyVector.fromVectorUnsafe(nonEmptyVector.toVector.sortBy(i => -i))
      )
    }
  }
}

class ReducibleNonEmptyVectorSuite extends ReducibleSuite[NonEmptyVector]("NonEmptyVector") {
  def iterator[T](nel: NonEmptyVector[T]): Iterator[T] = nel.toVector.iterator

  def range(start: Long, endInclusive: Long): NonEmptyVector[Long] = {
    // if we inline this we get a bewildering implicit numeric widening
    // error message in Scala 2.10
    val tailStart: Long = start + 1L
    NonEmptyVector(start, (tailStart).to(endInclusive).toVector)
  }
}

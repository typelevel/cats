/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats._
import cats.data.NonEmptyVector
import cats.data.NonEmptyVector.ZipNonEmptyVector
import cats.kernel.instances.order.catsKernelOrderingForOrder
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.HashTests
import cats.kernel.laws.discipline.OrderTests
import cats.kernel.laws.discipline.SemigroupTests
import cats.kernel.laws.discipline.PartialOrderTests
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.platform.Platform
import cats.syntax.eq._
import cats.syntax.foldable._
import cats.syntax.reducible._
import cats.syntax.show._
import org.scalacheck.Prop._
import org.scalacheck.Test.Parameters

import scala.util.Properties

class NonEmptyVectorSuite extends NonEmptyCollectionSuite[Vector, NonEmptyVector, NonEmptyVector] {
  protected def toList[A](value: NonEmptyVector[A]): List[A] = value.toList
  protected def underlyingToList[A](underlying: Vector[A]): List[A] = underlying.toList
  protected def toNonEmptyCollection[A](nea: NonEmptyVector[A]): NonEmptyVector[A] = nea

  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val scalaCheckTestParameters: Parameters =
    Parameters.default.withMinSuccessfulTests(20).withMaxSize(Parameters.default.minSize + 5)

  checkAll("NonEmptyVector[Int]", EqTests[NonEmptyVector[Int]].eqv)
  checkAll("NonEmptyVector[Int]", HashTests[NonEmptyVector[Int]].hash)
  checkAll("NonEmptyVector[Int]", OrderTests[NonEmptyVector[Int]].order)
  checkAll("NonEmptyVector[Int]", PartialOrderTests[NonEmptyVector[Int]].partialOrder)

  checkAll("NonEmptyVector[Int] with Option",
           NonEmptyTraverseTests[NonEmptyVector].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll("NonEmptyTraverse[NonEmptyVector[A]]", SerializableTests.serializable(NonEmptyTraverse[NonEmptyVector]))

  checkAll("NonEmptyVector[Int]", ReducibleTests[NonEmptyVector].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptyVector]", SerializableTests.serializable(Reducible[NonEmptyVector]))

  // Test instances that have more general constraints

  checkAll("NonEmptyVector[Int]", NonEmptyAlternativeTests[NonEmptyVector].nonEmptyAlternative[Int, Int, Int])
  checkAll("Semigroup[NonEmptyVector[Int]]", SerializableTests.serializable(Semigroup[NonEmptyVector[Int]]))
  checkAll("NonEmptyVector[Int]", SemigroupTests[NonEmptyVector[Int]].semigroup)
  checkAll("NonEmptyAlternative[NonEmptyVector]", SerializableTests.serializable(NonEmptyAlternative[NonEmptyVector]))

  checkAll("NonEmptyVector[Int]", FoldableTests[NonEmptyVector].foldable[Int, Int])
  checkAll("Foldable[NonEmptyVector]", SerializableTests.serializable(Foldable[NonEmptyVector]))

  checkAll("NonEmptyVector[Int]", AlignTests[NonEmptyVector].align[Int, Int, Int, Int])
  checkAll("Align[NonEmptyVector]", SerializableTests.serializable(Align[NonEmptyVector]))

  checkAll("ZipNonEmptyVector[Int]", CommutativeApplyTests[ZipNonEmptyVector].commutativeApply[Int, Int, Int])
  checkAll("CommutativeApply[ZipNonEmptyVector]", SerializableTests.serializable(CommutativeApply[ZipNonEmptyVector]))

  // Test functor and subclasses don't have implicit conflicts
  implicitly[Functor[NonEmptyVector]]
  implicitly[Monad[NonEmptyVector]]
  implicitly[Comonad[NonEmptyVector]]
  implicitly[Bimonad[NonEmptyVector]]

  checkAll("NonEmptyVector[Int]", BimonadTests[NonEmptyVector].bimonad[Int, Int, Int])
  checkAll("Bimonad[NonEmptyVector]", SerializableTests.serializable(Bimonad[NonEmptyVector]))

  checkAll("NonEmptyVector[Int]", ShortCircuitingTests[NonEmptyVector].foldable[Int])
  checkAll("NonEmptyVector[Int]", ShortCircuitingTests[NonEmptyVector].traverse[Int])
  checkAll("NonEmptyVector[Int]", ShortCircuitingTests[NonEmptyVector].nonEmptyTraverse[Int])

  test("size is consistent with toList.size") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.size === (nonEmptyVector.toList.size.toLong))
    }
  }

  test("Show is not empty and is formatted as expected") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.show.nonEmpty === true)
      assert(nonEmptyVector.show.startsWith("NonEmptyVector(") === true)
      assert(nonEmptyVector.show === (implicitly[Show[NonEmptyVector[Int]]].show(nonEmptyVector)))
      assert(nonEmptyVector.show.contains(nonEmptyVector.head.show) === true)
    }
  }

  test("Show is formatted correctly") {
    val v1 = NonEmptyVector("Test", Vector.empty)
    assert(v1.show === "NonEmptyVector(Test)")

    val v2 = NonEmptyVector.of("foo", "bar", "baz")
    assert(v2.show === "NonEmptyVector(foo, bar, baz)")
  }

  test("Creating NonEmptyVector + toVector is identity") {
    forAll { (i: Int, tail: Vector[Int]) =>
      val vector = i +: tail
      val nonEmptyVector = NonEmptyVector(i, tail)
      assert(vector === (nonEmptyVector.toVector))
    }
  }

  test("NonEmptyVector#filter is consistent with Vector#filter") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      assert(nonEmptyVector.filter(p) === (vector.filter(p)))
    }
  }

  test("NonEmptyVector#filterNot is consistent with Vector#filterNot") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      assert(nonEmptyVector.filterNot(p) === (vector.filterNot(p)))
    }
  }

  test("NonEmptyVector#find is consistent with Vector#find") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      assert(nonEmptyVector.find(p) === (vector.find(p)))
    }
  }

  test("NonEmptyVector#exists is consistent with Vector#exists") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      assert(nonEmptyVector.exists(p) === (vector.exists(p)))
    }
  }

  test("NonEmptyVector#forall is consistent with Vector#forall") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => Boolean) =>
      val vector = nonEmptyVector.toVector
      assert(nonEmptyVector.forall(p) === (vector.forall(p)))
    }
  }

  test("NonEmptyVector#map is consistent with Vector#map") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], p: Int => String) =>
      val vector = nonEmptyVector.toVector
      assert(nonEmptyVector.map(p).toVector === (vector.map(p)))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: (Int, Int) => Int) =>
      assert(nonEmptyVector.reduceLeft(f) === (nonEmptyVector.tail.foldLeft(nonEmptyVector.head)(f)))
    }
  }

  def excise[A](as: Vector[A]): (Vector[A], A) =
    (as.slice(0, as.size - 1), as.last)

  test("reduceRight consistent with foldRight") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nonEmptyVector.reduceRight(f).value
      val (first, last) = excise(nonEmptyVector.toVector)
      val expected = first.foldRight(last)((a, b) => f(a, Now(b)).value)
      assert(got === expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.reduce === (nonEmptyVector.fold))
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nonEmptyVector: NonEmptyVector[Option[Int]]) =>
      assert(nonEmptyVector.reduce(SemigroupK[Option].algebra[Int]) === (nonEmptyVector.reduceK))
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nonEmptyVector.tail.foldLeft(Option(f(nonEmptyVector.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      assert(nonEmptyVector.reduceLeftToOption(f)(g) === expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val (first, last) = excise(nonEmptyVector.toVector)
      val expected = first.foldRight(Option(f(last))) { (i, opt) =>
        opt.map(s => g(i, Now(s)).value)
      }
      assert(nonEmptyVector.reduceRightToOption(f)(g).value === expected)
    }
  }

  test("fromVector returns None when the input vector is empty") {
    assert(NonEmptyVector.fromVector(Vector.empty[Int]) === (Option.empty[NonEmptyVector[Int]]))
  }

  test("fromVectorUnsafe throws an exception when the input vector is empty") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptyVector.fromVectorUnsafe(Vector.empty[Int])
    }
  }

  test("++ Vector is consistent with concatNev") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], other: NonEmptyVector[Int]) =>
      assert(nonEmptyVector ++ other.toVector === (nonEmptyVector.concatNev(other)))
    }
  }

  test("++ Vector is consistent with concat") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], vector: Vector[Int]) =>
      assert(nonEmptyVector ++ vector === (nonEmptyVector.concat(vector)))
    }
  }

  test("appendVector is consistent with concat") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], vector: Vector[Int]) =>
      assert(nonEmptyVector.appendVector(vector) === nonEmptyVector.concat(vector))
    }
  }

  test(":+ is consistent with concat") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], i: Int) =>
      assert(nonEmptyVector :+ i === (nonEmptyVector.concat(Vector(i))))
    }
  }
  test("append is consistent with :+") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], i: Int) =>
      assert(nonEmptyVector.append(i) === (nonEmptyVector :+ i))
    }
  }

  test("+: is consistent with concatNev") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], i: Int) =>
      assert(i +: nonEmptyVector === (NonEmptyVector.one(i).concatNev(nonEmptyVector)))
    }
  }
  test("prepend is consistent with +:") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], i: Int) =>
      assert(nonEmptyVector.prepend(i) === (i +: nonEmptyVector))
    }
  }

  test("prependVector with a NonEmptyVector is the same as concatNec") {
    forAll { (nonEmptyVector1: NonEmptyVector[Int], nonEmptyVector2: NonEmptyVector[Int]) =>
      assert(nonEmptyVector2.prependVector(nonEmptyVector1.toVector) === (nonEmptyVector1.concatNev(nonEmptyVector2)))
    }
  }

  test("prependVector with an empty Vector is the same as the original NonEmptyVector") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.prependVector(Vector.empty) === nonEmptyVector)
    }
  }

  test("NonEmptyVector#of on varargs is consistent with NonEmptyVector#apply on Vector") {
    forAll { (head: Int, tail: Vector[Int]) =>
      assert(NonEmptyVector.of(head, tail: _*) === (NonEmptyVector(head, tail)))
    }
  }

  test("NonEmptyVector#get returns a None when the element does not exist") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      val size = nonEmptyVector.toVector.size
      assert(nonEmptyVector.get(size) === None)
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
      assert(nonEmptyVector.updated(size, element) === None)
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
      assert(nonEmptyVector.hashCode === (nonEmptyVector.toVector.hashCode))
    }
  }

  test("NonEmptyVector#equals consistent with Vector#equals") {
    forAll { (lhs: NonEmptyVector[Int], rhs: NonEmptyVector[Int]) =>
      assert(lhs.equals(rhs) === (lhs.toVector.equals(rhs.toVector)))
    }
  }

  test("NonEmptyVector#toString produces correct output") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.toString === s"NonEmpty${nonEmptyVector.toVector.toString}")
    }
    assert(NonEmptyVector(1, Vector.empty).toString === "NonEmptyVector(1)")
    assert(NonEmptyVector(1, Vector.empty).toVector.toString === "Vector(1)")
  }

  test("NonEmptyVector.unapply supports pattern matching") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      nonEmptyVector match {
        case NonEmptyVector(head, tail) =>
          assert(head === (nonEmptyVector.head))
          assert(tail === (nonEmptyVector.tail))
      }
    }
  }

  test("Cannot create a new NonEmptyVector from constructor") {
    if (Platform.isJvm) {
      if (!Properties.versionNumberString.startsWith("2.10")) {
        // A bug in scala 2.10 allows private constructors to be accessed.
        // We should still ensure that on scala 2.11 and up we cannot construct the
        // object directly. see: https://issues.scala-lang.org/browse/SI-6601
        assert(compileErrors("val bad: NonEmptyVector[Int] = new NonEmptyVector(Vector(1))").nonEmpty)

      }
    }
  }

  test("Cannot create a new NonEmptyVector[Int] from apply with a Vector[Int]") {
    assert(compileErrors("val bad: NonEmptyVector[Int] = NonEmptyVector(Vector(1))").nonEmpty)
  }

  test("Cannot create a new NonEmptyVector[Int] from apply with a an empty Vector") {
    assert(compileErrors("val bad: NonEmptyVector[Int] = NonEmptyVector(Vector.empty[Int])").nonEmpty)
  }

  test("NonEmptyVector#distinctBy is consistent with Vector#distinctBy") {
    forAll { (nonEmptyVector: NonEmptyVector[Int], f: Int => String) =>
      assert(nonEmptyVector.distinctBy(f).toVector === (nonEmptyVector.toVector.distinctBy(f)))
    }
  }

  test("NonEmptyVector#distinct is consistent with Vector#distinct") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.distinct.toVector === (nonEmptyVector.toVector.distinct))
    }
  }

  test("NonEmptyVector#zipWith is consistent with Vector#zip and then Vector#map") {
    forAll { (a: NonEmptyVector[Int], b: NonEmptyVector[Int], f: (Int, Int) => Int) =>
      assert(a.zipWith(b)(f).toVector === a.toVector.zip(b.toVector).map { case (x, y) => f(x, y) })
    }
  }

  test("NonEmptyVector#zipWith is consistent with #zipWithIndex") {
    forAll { (nev: NonEmptyVector[Int]) =>
      val zw = nev.zipWith(NonEmptyVector.fromVectorUnsafe((0 until nev.length).toVector))(Tuple2.apply)
      assert(nev.zipWithIndex === zw)
    }
  }

  test("NonEmptyVector#nonEmptyPartition remains sorted") {
    forAll { (nev: NonEmptyVector[Int], f: Int => Either[String, String]) =>
      val sorted = NonEmptyVector.fromVectorUnsafe(nev.map(f).toVector.sorted)
      val ior = Reducible[NonEmptyVector].nonEmptyPartition(sorted)(identity)

      assert(ior.left.map(xs => xs.sorted === xs).getOrElse(true))
      assert(ior.right.map(xs => xs.sorted === xs).getOrElse(true))
    }
  }

  test("NonEmptyVector#last is consistent with Vector#last") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.last === (nonEmptyVector.toVector.last))
    }
  }

  test("NonEmptyVector#init is consistent with Vector#init") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.init === (nonEmptyVector.toVector.init))
    }
  }

  test("NonEmptyVector#collect is consistent with Vector#collect") {
    val pf: PartialFunction[Int, Double] = {
      case i if i % 2 == 0 => i.toDouble
    }
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.collect(pf) === (nonEmptyVector.toVector.collect(pf)))
    }
  }

  test("NonEmptyVector#length and size is consistent with Vector#length") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.length === (nonEmptyVector.toVector.length))
      assert(nonEmptyVector.size === (nonEmptyVector.toVector.length.toLong))
    }
  }

  test("NonEmptyVector#reverse is consistent with Vector#reverse") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.reverse === (NonEmptyVector.fromVectorUnsafe(nonEmptyVector.toVector.reverse)))
    }
  }

  test("NonEmptyVector#zipWithIndex is consistent with Vector#zipWithIndex") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      val expected = NonEmptyVector.fromVectorUnsafe(nonEmptyVector.toVector.zipWithIndex)
      assert(nonEmptyVector.zipWithIndex === expected)
      assert(Traverse[NonEmptyVector].zipWithIndex(nonEmptyVector) === expected)
    }
  }

  test("NonEmptyVector#sorted and sortBy is consistent with Vector#sorted and sortBy") {
    forAll { (nonEmptyVector: NonEmptyVector[Int]) =>
      assert(nonEmptyVector.sorted === (NonEmptyVector.fromVectorUnsafe(nonEmptyVector.toVector.sorted)))
      assert(
        nonEmptyVector.sortBy(i => -i) ===
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
    NonEmptyVector(start, tailStart.to(endInclusive).toVector)
  }

  def fromValues[A](el: A, els: A*): NonEmptyVector[A] = NonEmptyVector(el, Vector(els: _*))
}

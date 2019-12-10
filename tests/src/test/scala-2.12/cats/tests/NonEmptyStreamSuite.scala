package cats
package tests

import cats.data.{NonEmptyStream, OneAnd}
import cats.instances.stream._
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline._

class NonEmptyStreamSuite extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 20, sizeRange = 5)

  checkAll("NonEmptyStream[Int]", EqTests[NonEmptyStream[Int]].eqv)

  checkAll("NonEmptyStream[Int] with Option",
           NonEmptyTraverseTests[NonEmptyStream].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
  checkAll("NonEmptyTraverse[NonEmptyStream[A]]", SerializableTests.serializable(NonEmptyTraverse[NonEmptyStream[*]]))

  checkAll("NonEmptyStream[Int]", ReducibleTests[NonEmptyStream].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptyStream]", SerializableTests.serializable(Reducible[NonEmptyStream]))

  checkAll("NonEmptyStream[Int]", SemigroupTests[NonEmptyStream[Int]].semigroup)
  checkAll("Semigroup[NonEmptyStream[Int]]", SerializableTests.serializable(Semigroup[NonEmptyStream[Int]]))

  {
    // Test functor and subclasses don't have implicit conflicts
    implicitly[Functor[NonEmptyStream]]
    implicitly[Monad[NonEmptyStream]]
    implicitly[Comonad[NonEmptyStream]]
  }

  implicit val iso2 = SemigroupalTests.Isomorphisms.invariant[NonEmptyStream]

  checkAll("NonEmptyStream[Int]", MonadTests[NonEmptyStream].monad[Int, Int, Int])
  checkAll("Monad[NonEmptyStream[A]]", SerializableTests.serializable(Monad[NonEmptyStream]))

  checkAll("NonEmptyStream[Int]", ComonadTests[NonEmptyStream].comonad[Int, Int, Int])
  checkAll("Comonad[NonEmptyStream[A]]", SerializableTests.serializable(Comonad[NonEmptyStream]))

  test("Show is not empty and is formatted as expected") {
    forAll { (nel: NonEmptyStream[Int]) =>
      nel.show.nonEmpty should ===(true)
      nel.show.startsWith("OneAnd(") should ===(true)
      nel.show should ===(implicitly[Show[NonEmptyStream[Int]]].show(nel))
      nel.show.contains(nel.head.show) should ===(true)
    }
  }

  test("Show is formatted correctly") {
    val oneAnd = NonEmptyStream("Test")
    oneAnd.show should ===(s"OneAnd(Test, Stream())")
  }

  test("Creating OneAnd + unwrap is identity") {
    forAll { (i: Int, tail: Stream[Int]) =>
      val stream = i #:: tail
      val oneAnd = NonEmptyStream(i, tail: _*)
      stream should ===(oneAnd.unwrap)
    }
  }

  test("NonEmptyStream#find is consistent with Stream#find") {
    forAll { (nel: NonEmptyStream[Int], p: Int => Boolean) =>
      val stream = nel.unwrap
      nel.find(p) should ===(stream.find(p))
    }
  }

  test("NonEmptyStream#exists is consistent with Stream#exists") {
    forAll { (nel: NonEmptyStream[Int], p: Int => Boolean) =>
      val stream = nel.unwrap
      nel.exists(p) should ===(stream.exists(p))
    }
  }

  test("NonEmptyStream#forall is consistent with Stream#forall") {
    forAll { (nel: NonEmptyStream[Int], p: Int => Boolean) =>
      val stream = nel.unwrap
      nel.forall(p) should ===(stream.forall(p))
    }
  }

  test("NonEmptyStream#map is consistent with Stream#map") {
    forAll { (nel: NonEmptyStream[Int], p: Int => String) =>
      val stream = nel.unwrap
      nel.map(p).unwrap should ===(stream.map(p))
    }
  }

  test("NonEmptyStream#nonEmptyPartition remains sorted") {
    forAll { (nes: NonEmptyStream[Int], f: Int => Either[String, String]) =>
      val nesf = nes.map(f)
      val sortedStream = (nesf.head #:: nesf.tail).sorted
      val sortedNes = OneAnd(sortedStream.head, sortedStream.tail)
      val ior = Reducible[NonEmptyStream].nonEmptyPartition(sortedNes)(identity)

      ior.left.map(xs => xs.sorted should ===(xs))
      ior.right.map(xs => xs.sorted should ===(xs))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nel: NonEmptyStream[Int], f: (Int, Int) => Int) =>
      nel.reduceLeft(f) should ===(nel.tail.foldLeft(nel.head)(f))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nel: NonEmptyStream[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nel.reduceRight(f).value
      val last :: rev = nel.unwrap.toList.reverse
      val expected = rev.reverse.foldRight(last)((a, b) => f(a, Now(b)).value)
      got should ===(expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nel: NonEmptyStream[Int]) =>
      nel.reduce should ===(nel.fold)
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nel: NonEmptyStream[Option[Int]]) =>
      nel.reduce(SemigroupK[Option].algebra[Int]) should ===(nel.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nel: NonEmptyStream[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nel.tail.foldLeft(Option(f(nel.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      nel.reduceLeftToOption(f)(g) should ===(expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nel: NonEmptyStream[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val got = nel.reduceRightToOption(f)(g).value
      val last :: rev = nel.unwrap.toList.reverse
      val expected = rev.reverse.foldRight(Option(f(last))) { (i, opt) =>
        opt.map(s => g(i, Now(s)).value)
      }
      got should ===(expected)
    }
  }

  test("filter includes elements based on a predicate") {
    forAll { (nes: NonEmptyStream[Int], pred: Int => Boolean) =>
      nes.filter(pred) should ===(nes.unwrap.filter(pred))
    }
  }

}

class ReducibleNonEmptyStreamSuite extends ReducibleSuite[NonEmptyStream]("NonEmptyStream") {
  def iterator[T](nes: NonEmptyStream[T]): Iterator[T] =
    (nes.head #:: nes.tail).iterator

  def range(start: Long, endInclusive: Long): NonEmptyStream[Long] = {
    // if we inline this we get a bewildering implicit numeric widening
    // error message in Scala 2.10
    val tailStart: Long = start + 1L
    NonEmptyStream(start, tailStart.to(endInclusive).toStream)
  }

  def rangeE[L, R](el: Either[L, R], els: Either[L, R]*): NonEmptyStream[Either[L, R]] =
    NonEmptyStream(el, els: _*)
}

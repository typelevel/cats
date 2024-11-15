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

import cats.{Bitraverse, MonadError, Semigroupal, Show, Traverse}
import cats.data.{EitherT, Ior, NonEmptyChain, NonEmptyList, NonEmptySet, NonEmptyVector}
import cats.kernel.{Eq, Semigroup}
import cats.kernel.laws.discipline.{OrderTests, SemigroupTests}
import cats.laws.discipline.{
  BifunctorTests,
  BitraverseTests,
  MonadErrorTests,
  SemigroupalTests,
  SerializableTests,
  TraverseTests
}
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary.*
import org.scalacheck.Arbitrary.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class IorSuite extends CatsSuite {

  implicit val iso: Isomorphisms[Ior[String, *]] = Isomorphisms.invariant[Ior[String, *]]

  checkAll("Ior[String, Int]", SemigroupalTests[Ior[String, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Ior[String, *]]", SerializableTests.serializable(Semigroupal[Ior[String, *]]))

  implicit val eq0: Eq[EitherT[Ior[String, *], String, Int]] = EitherT.catsDataEqForEitherT[Ior[String, *], String, Int]

  checkAll("Ior[String, Int]", MonadErrorTests[Ior[String, *], String].monadError[Int, Int, Int])
  checkAll("MonadError[SIor[String, *]]", SerializableTests.serializable(MonadError[Ior[String, *], String]))

  checkAll("Ior[String, Int] with Option", TraverseTests[Ior[String, *]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Ior[String, *]]", SerializableTests.serializable(Traverse[Ior[String, *]]))
  checkAll("Ior[*, *]", BifunctorTests[Ior].bifunctor[Int, Int, Int, String, String, String])

  checkAll("BitraverseTests Ior[*, *]", BitraverseTests[Ior].bitraverse[Option, Int, Int, Int, String, String, String])
  checkAll("Bitraverse[Ior]", SerializableTests.serializable(Bitraverse[Ior]))

  checkAll("Order[Ior[A: Order, B: Order]]", OrderTests[Ior[Int, Int]].order)

  checkAll("Semigroup[Ior[A: Semigroup, B: Semigroup]]", SemigroupTests[Ior[List[Int], List[Int]]].semigroup)
  checkAll("SerializableTest Semigroup[Ior[A: Semigroup, B: Semigroup]]",
           SerializableTests.serializable(Semigroup[Ior[List[Int], List[Int]]])
  )

  test("left Option is defined left and both") {
    forAll { (i: Int Ior String) =>
      assert((i.isLeft || i.isBoth) === (i.left.isDefined))
    }
  }

  test("right Option is defined for right and both") {
    forAll { (i: Int Ior String) =>
      assert((i.isRight || i.isBoth) === (i.right.isDefined))
    }
  }

  test("onlyLeftOrRight") {
    forAll { (i: Int Ior String) =>
      assert(i.onlyLeft.map(Left(_)).orElse(i.onlyRight.map(Right(_))) === (i.onlyLeftOrRight))
    }
  }

  test("onlyBoth consistent with left and right") {
    forAll { (i: Int Ior String) =>
      assert(i.onlyBoth === (for {
        left <- i.left
        right <- i.right
      } yield (left, right)))
    }
  }

  test("pad") {
    forAll { (i: Int Ior String) =>
      assert(i.pad === ((i.left, i.right)))
    }
  }

  test("unwrap consistent with isBoth") {
    forAll { (i: Int Ior String) =>
      assert(i.unwrap.isRight === (i.isBoth))
    }
  }

  test("valueOr consistent with leftMap") {
    forAll { (i: Int Ior String, f: Int => String) =>
      assert(i.valueOr(f) === (i.leftMap(f).fold(identity, identity, _ + _)))
    }
  }

  test("isLeft consistent with toOption") {
    forAll { (i: Int Ior String) =>
      assert(i.isLeft === (i.toOption.isEmpty))
    }
  }

  test("isLeft consistent with toList") {
    forAll { (i: Int Ior String) =>
      assert(i.isLeft === (i.toList.isEmpty))
    }
  }

  test("isLeft consistent with forall and exists") {
    forAll { (i: Int Ior String, p: String => Boolean) =>
      if (i.isLeft) {
        assert((i.forall(p) && !i.exists(p)) === true)
      }
    }
  }

  test("leftMap then swap equivalent to swap then map") {
    forAll { (i: Int Ior String, f: Int => Double) =>
      assert(i.leftMap(f).swap === (i.swap.map(f)))
    }
  }

  test("foreach is noop for left") {
    forAll { (i: Int) =>
      Ior.left[Int, String](i).foreach { _ =>
        fail("should not be called")
      }
    }
  }

  test("foreach runs for right and both") {
    forAll { (i: Int Ior String) =>
      var count = 0
      i.foreach { _ =>
        count += 1
      }
      if (i.isRight || i.isBoth) assertEquals(count, 1)
      else assertEquals(count, 0)
    }
  }

  test("show isn't empty") {
    val iorShow = implicitly[Show[Int Ior String]]

    forAll { (i: Int Ior String) =>
      assert(iorShow.show(i).nonEmpty === true)
    }
  }

  test("merge") {
    forAll { (i: Int Ior Int) =>
      assert(i.merge === (i.left.getOrElse(0) + i.right.getOrElse(0)))
    }
  }

  test("mergeLeft") {
    forAll { (i: Int Ior Int) =>
      assert(i.mergeLeft === (i.left.orElse(i.right).get))
    }
  }

  test("mergeRight") {
    forAll { (i: Int Ior Int) =>
      assert(i.mergeRight === (i.right.orElse(i.left).get))
    }
  }

  test("mergeWith") {
    forAll { (i: Int Ior Int, f: (Int, Int) => Int) =>
      assert(i.mergeWith(f) === i.onlyBoth.map(f.tupled).orElse(i.left).orElse(i.right).get)
    }
  }

  test("putLeft") {
    forAll { (i: Int Ior Int) =>
      val expectedResult =
        if (i.isLeft)
          Ior.left(2)
        else
          Ior.both(2, i.right.get)
      assert(i.putLeft(2) === expectedResult)
    }
  }

  test("putRight") {
    forAll { (i: Int Ior Int) =>
      val expectedResult =
        if (i.isRight)
          Ior.right(2)
        else
          Ior.both(i.left.get, 2)
      assert(i.putRight(2) === expectedResult)
    }
  }

  test("addLeft") {
    forAll { (i: Int Ior Int, j: Int) =>
      val expectedResult =
        if (i.isLeft)
          Ior.left(i.left.get + j)
        else if (i.isRight)
          Ior.both(j, i.right.get)
        else
          Ior.both(i.left.get + j, i.right.get)
      assert(i.addLeft(j) === expectedResult)
    }
  }

  test("addRight") {
    forAll { (i: Int Ior Int, j: Int) =>
      val expectedResult =
        if (i.isLeft)
          Ior.both(i.left.get, j)
        else if (i.isRight)
          Ior.right(i.right.get + j)
        else
          Ior.both(i.left.get, i.right.get + j)
      assert(i.addRight(j) === expectedResult)
    }
  }

  test("combine left") {
    forAll { (i: Int Ior String, j: Int Ior String) =>
      assert(i.combine(j).left === (i.left.map(_ + j.left.getOrElse(0)).orElse(j.left)))
    }
  }

  test("combine right") {
    forAll { (i: Int Ior String, j: Int Ior String) =>
      assert(i.combine(j).right === (i.right.map(_ + j.right.getOrElse("")).orElse(j.right)))
    }
  }

  test("fromOptions left/right consistent with input options") {
    forAll { (oa: Option[String], ob: Option[Int]) =>
      val x = Ior.fromOptions(oa, ob)
      assert(x.flatMap(_.left) === oa)
      assert(x.flatMap(_.right) === ob)
    }
  }

  test("Option roundtrip") {
    forAll { (ior: String Ior Int) =>
      val iorMaybe = Ior.fromOptions(ior.left, ior.right)
      assert(iorMaybe === (Some(ior)))
    }
  }

  test("to consistent with toList") {
    forAll { (x: Int Ior String) =>
      assert(x.to[List, String] === (x.toList))
    }
  }

  test("to consistent with toOption") {
    forAll { (x: Int Ior String) =>
      assert(x.to[Option, String] === (x.toOption))
    }
  }

  test("toEither consistent with right") {
    forAll { (x: Int Ior String) =>
      assert(x.toEither.toOption === (x.right))
    }
  }

  test("toValidated consistent with right") {
    forAll { (x: Int Ior String) =>
      assert(x.toValidated.toOption === (x.right))
    }
  }

  test("toIorNec Left") {
    val ior = Ior.left[String, Int]("oops")
    assert(ior.toIorNec === (Ior.left[NonEmptyChain[String], Int](NonEmptyChain.one("oops"))))
  }
  test("toIorNec Right") {
    val ior = Ior.right[String, Int](42)
    assert(ior.toIorNec === (Ior.right[NonEmptyChain[String], Int](42)))
  }
  test("toIorNec Both") {
    val ior = Ior.both[String, Int]("oops", 42)
    assert(ior.toIorNec === (Ior.both[NonEmptyChain[String], Int](NonEmptyChain.one("oops"), 42)))
  }

  test("toIorNes Left") {
    val ior = Ior.left[String, Int]("oops")
    assert(ior.toIorNes === (Ior.left[NonEmptySet[String], Int](NonEmptySet.one("oops"))))
  }
  test("toIorNes Right") {
    val ior = Ior.right[String, Int](42)
    assert(ior.toIorNes === (Ior.right[NonEmptySet[String], Int](42)))
  }
  test("toIorNes Both") {
    val ior = Ior.both[String, Int]("oops", 42)
    assert(ior.toIorNes === (Ior.both[NonEmptySet[String], Int](NonEmptySet.one("oops"), 42)))
  }

  test("toIorNel Left") {
    val ior = Ior.left[String, Int]("oops")
    assert(ior.toIorNel === (Ior.left[NonEmptyList[String], Int](NonEmptyList.one("oops"))))
  }

  test("toIorNel Right") {
    val ior = Ior.right[String, Int](42)
    assert(ior.toIorNel === (Ior.right[NonEmptyList[String], Int](42)))
  }

  test("toIorNel Both") {
    forAll { (x: String, y: Int) =>
      assert(Ior.both(x, y).toIorNel === (Ior.both[NonEmptyList[String], Int](NonEmptyList.one(x), y)))
    }
  }

  test("leftNel") {
    forAll { (x: String) =>
      assert(Ior.leftNel(x).left === (Some(NonEmptyList.one(x))))
    }
  }

  test("leftNec") {
    forAll { (x: String) =>
      assert(Ior.leftNec(x).left === (Some(NonEmptyChain.one(x))))
    }
  }

  test("bothNel") {
    forAll { (x: Int, y: String) =>
      assert(Ior.bothNel(y, x).onlyBoth === (Some((NonEmptyList.one(y), x))))
    }
  }

  test("bothNec") {
    forAll { (x: Int, y: String) =>
      assert(Ior.bothNec(y, x).onlyBoth === (Some((NonEmptyChain.one(y), x))))
    }
  }

  test("getOrElse consistent with Option getOrElse") {
    forAll { (x: Int Ior String, default: String) =>
      assert(x.getOrElse(default) === (x.toOption.getOrElse(default)))
    }
  }
}

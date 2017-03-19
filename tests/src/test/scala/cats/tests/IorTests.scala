package cats
package tests

import cats.data.Ior
import cats.kernel.laws.GroupLaws
import cats.laws.discipline.{BifunctorTests, CartesianTests, MonadTests, SerializableTests, TraverseTests}
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary._

class IorTests extends CatsSuite {

  implicit val iso = CartesianTests.Isomorphisms.invariant[Ior[String, ?]]

  checkAll("Ior[String, Int]", CartesianTests[Ior[String, ?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[String Ior ?]]", SerializableTests.serializable(Cartesian[String Ior ?]))

  checkAll("Ior[String, Int]", MonadTests[String Ior ?].monad[Int, Int, Int])
  checkAll("MonadError[String Ior ?]", SerializableTests.serializable(MonadError[String Ior ?, String]))

  checkAll("Ior[String, Int] with Option", TraverseTests[String Ior ?].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[String Ior ?]", SerializableTests.serializable(Traverse[String Ior ?]))
  checkAll("? Ior ?", BifunctorTests[Ior].bifunctor[Int, Int, Int, String, String, String])

  checkAll("Semigroup[Ior[A: Semigroup, B: Semigroup]]", GroupLaws[Ior[List[Int], List[Int]]].semigroup)
  checkAll("SerializableTest Semigroup[Ior[A: Semigroup, B: Semigroup]]", SerializableTests.serializable(Semigroup[Ior[List[Int], List[Int]]]))

  test("left Option is defined left and both") {
    forAll { (i: Int Ior String) =>
      (i.isLeft || i.isBoth) should === (i.left.isDefined)
    }
  }

  test("right Option is defined for right and both") {
    forAll { (i: Int Ior String) =>
      (i.isRight || i.isBoth) should === (i.right.isDefined)
    }
  }

  test("onlyLeftOrRight") {
    forAll { (i: Int Ior String) =>
      i.onlyLeft.map(Left(_)).orElse(i.onlyRight.map(Right(_))) should === (i.onlyLeftOrRight)
    }
  }

  test("onlyBoth consistent with left and right") {
    forAll { (i: Int Ior String) =>
      i.onlyBoth should === (for {
        left <- i.left
        right <- i.right
      } yield (left, right))
    }
  }

  test("pad") {
    forAll { (i: Int Ior String) =>
      i.pad should === ((i.left, i.right))
    }
  }

  test("unwrap consistent with isBoth") {
    forAll { (i: Int Ior String) =>
      i.unwrap.isRight should === (i.isBoth)
    }
  }

  test("isLeft consistent with toOption") {
    forAll { (i: Int Ior String) =>
      i.isLeft should === (i.toOption.isEmpty)
    }
  }

  test("isLeft consistent with toList") {
    forAll { (i: Int Ior String) =>
      i.isLeft should === (i.toList.isEmpty)
    }
  }

  test("isLeft consistent with forall and exists") {
    forAll { (i: Int Ior String, p: String => Boolean) =>
      if (i.isLeft) {
        (i.forall(p) && !i.exists(p)) should === (true)
      }
    }
  }

  test("leftMap then swap equivalent to swap then map") {
    forAll { (i: Int Ior String, f: Int => Double) =>
      i.leftMap(f).swap should === (i.swap.map(f))
    }
  }

  test("foreach is noop for left") {
    forAll { (i: Int) =>
      Ior.left[Int, String](i).foreach { _ => fail("should not be called") }
    }
  }

  test("foreach runs for right and both") {
    forAll { (i: Int Ior String) =>
      var count = 0
      i.foreach { _ => count += 1 }
      if (i.isRight || i.isBoth) count should === (1)
      else count should === (0)
    }
  }

  test("show isn't empty") {
    val iorShow = implicitly[Show[Int Ior String]]

    forAll { (i: Int Ior String) =>
      iorShow.show(i).nonEmpty should === (true)
    }
  }


  test("merge") {
    forAll { (i: Int Ior Int) =>
        i.merge should === (i.left.getOrElse(0) + i.right.getOrElse(0))
    }
  }

  test("mergeLeft") {
    forAll { (i: Int Ior Int) =>
      i.mergeLeft should === (i.left.orElse(i.right).get)
    }
  }

  test("mergeRight") {
    forAll { (i: Int Ior Int) =>
      i.mergeRight should === (i.right.orElse(i.left).get)
    }
  }

  test("putLeft") {
    forAll { (i: Int Ior Int) =>
      val expectedResult =
      if (i.isLeft)
        Ior.left(2)
      else
        Ior.both(2, i.right.get)
      i.putLeft(2) should === (expectedResult)
    }
  }

  test("putRight") {
    forAll { (i: Int Ior Int) =>
      val expectedResult =
      if (i.isRight)
        Ior.right(2)
      else
        Ior.both(i.left.get, 2)
      i.putRight(2) should === (expectedResult)
    }
  }

  test("append left") {
    forAll { (i: Int Ior String, j: Int Ior String) =>
      i.append(j).left should === (i.left.map(_ + j.left.getOrElse(0)).orElse(j.left))
    }
  }

  test("append right") {
    forAll { (i: Int Ior String, j: Int Ior String) =>
      i.append(j).right should === (i.right.map(_ + j.right.getOrElse("")).orElse(j.right))
    }
  }

  test("fromOptions left/right consistent with input options"){
    forAll { (oa: Option[String], ob: Option[Int]) =>
      val x = Ior.fromOptions(oa, ob)
      x.flatMap(_.left) should === (oa)
      x.flatMap(_.right) should === (ob)
    }
  }

  test("Option roundtrip"){
    forAll { ior: String Ior Int =>
      val iorMaybe = Ior.fromOptions(ior.left, ior.right)
      iorMaybe should === (Some(ior))
    }
  }

  test("to consistent with toList") {
    forAll { (x: Int Ior String) =>
      x.to[List, String] should === (x.toList)
    }
  }

  test("to consistent with toOption") {
    forAll { (x: Int Ior String) =>
      x.to[Option, String] should === (x.toOption)
    }
  }

  test("toEither consistent with right") {
    forAll { (x: Int Ior String) =>
      x.toEither.toOption should === (x.right)
    }
  }

  test("getOrElse consistent with Option getOrElse") {
    forAll { (x: Int Ior String, default: String) =>
      x.getOrElse(default) should === (x.toOption.getOrElse(default))
    }
  }
}

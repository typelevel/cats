package cats
package tests

import cats.arrow.NaturalTransformation
import cats.data.Coproduct


class NaturalTransformationTests extends CatsSuite {
  val listToOption =
    new NaturalTransformation[List, Option] {
      def apply[A](fa: List[A]): Option[A] = fa.headOption
    }

  val optionToList =
    new NaturalTransformation[Option, List] {
      def apply[A](fa: Option[A]): List[A] = fa.toList
    }

  sealed trait Test1Algebra[A] {
    def v : A
  }

  case class Test1[A](v : A) extends Test1Algebra[A]

  sealed trait Test2Algebra[A] {
    def v : A
  }

  case class Test2[A](v : A) extends Test2Algebra[A]

  object Test1NT extends (Test1Algebra ~> Id) {
    override def apply[A](fa: Test1Algebra[A]): Id[A] = Id.pure(fa.v)
  }

  object Test2NT extends (Test2Algebra ~> Id) {
    override def apply[A](fa: Test2Algebra[A]): Id[A] = Id.pure(fa.v)
  }

  type T[A] = Coproduct[Test1Algebra, Test2Algebra, A]

  test("compose") {
    forAll { (list: List[Int]) =>
      val listToList = optionToList.compose(listToOption)
      listToList(list) should === (list.take(1))
    }
  }

  test("andThen") {
    forAll { (list: List[Int]) =>
      val listToList = listToOption.andThen(optionToList)
      listToList(list) should === (list.take(1))
    }
  }

  test("id is identity") {
    forAll { (list: List[Int]) =>
      NaturalTransformation.id[List].apply(list) should === (list)
    }
  }

  test("or") {
    val combinedInterpreter = Test1NT or Test2NT
    forAll { (a : Int, b : Int) =>
      combinedInterpreter(Coproduct.left(Test1(a))) should === (Id.pure(a))
      combinedInterpreter(Coproduct.right(Test2(b))) should === (Id.pure(b))
    }
  }
}

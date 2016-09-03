package cats
package tests

import cats.arrow.FunctionK
import cats.data.Coproduct
import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary._

class FunctionKTests extends CatsSuite {
  val listToOption =
    new FunctionK[List, Option] {
      def apply[A](fa: List[A]): Option[A] = fa.headOption
    }

  val optionToList =
    new FunctionK[Option, List] {
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

  object Test1NT extends FunctionK[Test1Algebra,Id] {
    override def apply[A](fa: Test1Algebra[A]): Id[A] = fa.v
  }

  object Test2NT extends FunctionK[Test2Algebra,Id] {
    override def apply[A](fa: Test2Algebra[A]): Id[A] = fa.v
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
      FunctionK.id[List].apply(list) should === (list)
    }
  }

  test("or") {
    val combinedInterpreter = Test1NT or Test2NT
    forAll { (a : Int, b : Int) =>
      combinedInterpreter(Coproduct.left(Test1(a))) should === (a)
      combinedInterpreter(Coproduct.right(Test2(b))) should === (b)
    }
  }

  test("lift simple unary") {
    def optionToList[A](option: Option[A]): List[A] = option.toList
    val fOptionToList = FunctionK.lift(optionToList _)
    forAll { (a: Option[Int]) =>
      fOptionToList(a) should === (optionToList(a))
    }

    val fO2I: FunctionK[Option, Iterable] = FunctionK.lift(Option.option2Iterable _)
    forAll { (a: Option[String]) =>
      fO2I(a).toList should === (Option.option2Iterable(a).toList)
    }

    val fNelFromListUnsafe = FunctionK.lift(NonEmptyList.fromListUnsafe _)
    forAll { (a: NonEmptyList[Int]) =>
      fNelFromListUnsafe(a.toList) should === (NonEmptyList.fromListUnsafe(a.toList))
    }
  }

  test("lift compound unary") {
    val fNelFromList = FunctionK.lift[List, λ[α ⇒ Option[NonEmptyList[α]]]](NonEmptyList.fromList _)
    forAll { (a: List[String]) =>
      fNelFromList(a) should === (NonEmptyList.fromList(a))
    }
  }

  { // lifting concrete types should fail to compile
    def sample[A](option: Option[A]): List[A] = option.toList
    assertTypeError("FunctionK.lift(sample[String])")
    assertTypeError("FunctionK.lift(sample[Nothing])")
  }

}

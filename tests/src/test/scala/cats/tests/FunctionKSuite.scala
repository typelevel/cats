package cats
package tests

import cats.arrow.FunctionK
import cats.data.EitherK
//import cats.data.NonEmptyList
//import cats.laws.discipline.arbitrary._

class FunctionKSuite extends CatsSuite {

  val listToOption = new FunctionK[List, Option] { def apply[A](a: List[A]): Option[A] = a.headOption }
  val listToVector = new FunctionK[List, Vector] { def apply[A](a: List[A]): Vector[A] = a.toVector }
  val optionToList = new FunctionK[Option, List] { def apply[A](a: Option[A]): List[A] = a.toList }

  sealed trait Test1Algebra[A] {
    def v: A
  }

  case class Test1[A](v: A) extends Test1Algebra[A]

  sealed trait Test2Algebra[A] {
    def v: A
  }

  case class Test2[A](v: A) extends Test2Algebra[A]

  val Test1FK = new FunctionK[Test1Algebra, Id] { def apply[A](a: Test1Algebra[A]): A = a.v }
  val Test2FK = new FunctionK[Test2Algebra, Id] { def apply[A](a: Test2Algebra[A]): A = a.v }

  test("compose") {
    forAll { (list: List[Int]) =>
      val listToList = optionToList.compose(listToOption)
      listToList(list) should ===(list.take(1))
    }
  }

  test("andThen") {
    forAll { (list: List[Int]) =>
      val listToList = listToOption.andThen(optionToList)
      listToList(list) should ===(list.take(1))
    }
  }

  test("id is identity") {
    forAll { (list: List[Int]) =>
      FunctionK.id[List].apply(list) should ===(list)
    }
  }

  test("or") {
    val combinedInterpreter = Test1FK.or(Test2FK)
    forAll { (a: Int, b: Int) =>
      combinedInterpreter(EitherK.left(Test1(a))) should ===(a)
      combinedInterpreter(EitherK.right(Test2(b))) should ===(b)
    }
  }

  test("and") {
    val combinedInterpreter = listToOption.and(listToVector)
    forAll { (list: List[Int]) =>
      val prod = combinedInterpreter(list)
      prod.first should ===(list.headOption)
      prod.second should ===(list.toVector)
    }
  }

  /*
  test("lift simple unary") {
    def optionToList[A](option: Option[A]): List[A] = option.toList
    val fOptionToList = FunctionK.lift(optionToList _)
    forAll { (a: Option[Int]) =>
      fOptionToList(a) should ===(optionToList(a))
    }

    val fO2I: FunctionK[Option, Iterable] = FunctionK.lift(Option.option2Iterable _)
    forAll { (a: Option[String]) =>
      fO2I(a).toList should ===(Option.option2Iterable(a).toList)
    }

    val fNelFromListUnsafe = FunctionK.lift(NonEmptyList.fromListUnsafe _)
    forAll { (a: NonEmptyList[Int]) =>
      fNelFromListUnsafe(a.toList) should ===(NonEmptyList.fromListUnsafe(a.toList))
    }
  }

  test("hygiene") {
    trait FunctionK
    def optionToList[A](option: Option[A]): List[A] = option.toList
    val fOptionToList = cats.arrow.FunctionK.lift(optionToList _)
    forAll { (a: Option[Int]) =>
      fOptionToList(a) should ===(optionToList(a))
    }
  }

  test("lift compound unary") {
    val fNelFromList = FunctionK.lift[List, λ[α => Option[NonEmptyList[α]]]](NonEmptyList.fromList _)
    forAll { (a: List[String]) =>
      fNelFromList(a) should ===(NonEmptyList.fromList(a))
    }
  }

  { // lifting concrete types should fail to compile
    def sample[A](option: Option[A]): List[A] = option.toList
    assertTypeError("FunctionK.lift(sample[String])")
    assertTypeError("FunctionK.lift(sample[Nothing])")
  }
 */

}

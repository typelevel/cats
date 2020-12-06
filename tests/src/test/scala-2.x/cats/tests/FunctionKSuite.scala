package cats.tests

import cats.arrow.FunctionK
import cats.data.{EitherK, NonEmptyList}
import cats.laws.discipline.arbitrary._
import cats.syntax.eq._
import cats.{Applicative, Id}
import org.scalacheck.Prop._

class FunctionKSuite extends CatsSuite {
  type OptionOfNel[+A] = Option[NonEmptyList[A]]

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
      assert(listToList(list) === list.take(1))
    }
  }

  test("andThen") {
    forAll { (list: List[Int]) =>
      val listToList = listToOption.andThen(optionToList)
      assert(listToList(list) === list.take(1))
    }
  }

  test("id is identity") {
    forAll { (list: List[Int]) =>
      assert(FunctionK.id[List].apply(list) === list)
    }
  }

  test("or") {
    val combinedInterpreter = Test1FK.or(Test2FK)
    forAll { (a: Int, b: Int) =>
      assert(combinedInterpreter(EitherK.left(Test1(a))) === a)
      assert(combinedInterpreter(EitherK.right(Test2(b))) === b)
    }
  }

  test("and") {
    val combinedInterpreter = listToOption.and(listToVector)
    forAll { (list: List[Int]) =>
      val prod = combinedInterpreter(list)
      assert(prod.first === list.headOption)
      assert(prod.second === list.toVector)
    }
  }

  test("lift simple unary") {
    def optionToList[A](option: Option[A]): List[A] = option.toList
    val fOptionToList = FunctionK.lift(optionToList _)
    forAll { (a: Option[Int]) =>
      assert(fOptionToList(a) === optionToList(a))
    }

    val fO2I: FunctionK[Option, Iterable] = FunctionK.lift(Option.option2Iterable _)
    forAll { (a: Option[String]) =>
      assert(fO2I(a).toList === Option.option2Iterable(a).toList)
    }

    val fNelFromListUnsafe = FunctionK.lift(NonEmptyList.fromListUnsafe _)
    forAll { (a: NonEmptyList[Int]) =>
      assert(fNelFromListUnsafe(a.toList) === NonEmptyList.fromListUnsafe(a.toList))
    }
  }

  test("hygiene") {
    trait FunctionK
    def optionToList[A](option: Option[A]): List[A] = option.toList
    val fOptionToList = cats.arrow.FunctionK.lift(optionToList _)
    forAll { (a: Option[Int]) =>
      assert(fOptionToList(a) === optionToList(a))
    }
  }

  test("lift compound unary") {
    val fNelFromList = FunctionK.lift[List, OptionOfNel](NonEmptyList.fromList)
    forAll { (a: List[String]) =>
      assert(fNelFromList(a) === NonEmptyList.fromList(a))
    }
  }

  test("lift eta-expanded function") {
    val fSomeNel = FunctionK.lift[NonEmptyList, OptionOfNel](Applicative[Option].pure)
    forAll { (a: NonEmptyList[Int]) =>
      assert(fSomeNel(a) === Some(a))
    }
  }

  test("lift a function directly") {
    def headOption[A](list: List[A]): Option[A] = list.headOption
    val fHeadOption = FunctionK.liftFunction[List, Option](headOption)
    forAll { (a: List[Int]) =>
      assert(fHeadOption(a) === a.headOption)
    }
  }

  { // lifting concrete types should fail to compile
    def sample[A](option: Option[A]): List[A] = option.toList
    assert(compileErrors("FunctionK.lift(sample[String])").nonEmpty)
    assert(compileErrors("FunctionK.lift(sample[Nothing])").nonEmpty)
  }
}

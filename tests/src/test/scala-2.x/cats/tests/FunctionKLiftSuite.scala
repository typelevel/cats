package cats.tests

import cats.data.NonEmptyList
import cats.arrow.FunctionK
import cats.implicits._
import org.scalacheck.Prop._
import cats.laws.discipline.arbitrary._

class FunctionKLiftSuite extends CatsSuite {

  test("lift simple unary") {
    def optionToList[A](option: Option[A]): List[A] = option.toList
    val fOptionToList = FunctionK.lift(optionToList _)
    forAll { (a: Option[Int]) =>
      assert(fOptionToList(a) === (optionToList(a)))
    }

    val fO2I: FunctionK[Option, Iterable] = FunctionK.lift(Option.option2Iterable _)
    forAll { (a: Option[String]) =>
      assert(fO2I(a).toList === (Option.option2Iterable(a).toList))
    }

    val fNelFromListUnsafe = FunctionK.lift(NonEmptyList.fromListUnsafe _)
    forAll { (a: NonEmptyList[Int]) =>
      assert(fNelFromListUnsafe(a.toList) === (NonEmptyList.fromListUnsafe(a.toList)))
    }
  }

  test("hygiene") {
    trait FunctionK
    def optionToList[A](option: Option[A]): List[A] = option.toList
    val fOptionToList = cats.arrow.FunctionK.lift(optionToList _)
    forAll { (a: Option[Int]) =>
      assert(fOptionToList(a) === (optionToList(a)))
    }
  }

  test("lift compound unary") {
    val fNelFromList = FunctionK.lift[List, λ[α => Option[NonEmptyList[α]]]](NonEmptyList.fromList _)
    forAll { (a: List[String]) =>
      assert(fNelFromList(a) === (NonEmptyList.fromList(a)))
    }
  }

  { // lifting concrete types should fail to compile
    def sample[A](option: Option[A]): List[A] = option.toList
    assert(compileErrors("FunctionK.lift(sample[String])").nonEmpty)
    assert(compileErrors("FunctionK.lift(sample[Nothing])").nonEmpty)
  }
}
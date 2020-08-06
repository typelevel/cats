package cats.tests

import cats.Id
import cats.arrow.FunctionK
import cats.data.EitherK
import cats.syntax.eq._
import org.scalacheck.Prop._

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
      assert(listToList(list) === (list.take(1)))
    }
  }

  test("andThen") {
    forAll { (list: List[Int]) =>
      val listToList = listToOption.andThen(optionToList)
      assert(listToList(list) === (list.take(1)))
    }
  }

  test("id is identity") {
    forAll { (list: List[Int]) =>
      assert(FunctionK.id[List].apply(list) === (list))
    }
  }

  test("or") {
    val combinedInterpreter = Test1FK.or(Test2FK)
    forAll { (a: Int, b: Int) =>
      assert(combinedInterpreter(EitherK.left(Test1(a))) === (a))
      assert(combinedInterpreter(EitherK.right(Test2(b))) === (b))
    }
  }

  test("and") {
    val combinedInterpreter = listToOption.and(listToVector)
    forAll { (list: List[Int]) =>
      val prod = combinedInterpreter(list)
      assert(prod.first === (list.headOption))
      assert(prod.second === (list.toVector))
    }
  }

}

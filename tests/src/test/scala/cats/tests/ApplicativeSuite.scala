package cats
package tests

import cats.Applicative
import cats.data.{Validated, Const}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.CoflatMapTests


class ApplicativeSuite extends CatsSuite {

  test("replicateA creates a List of 'n' copies of given Applicative 'fa'") {
    val A = Applicative[Option]
    val fa = A.pure(1)
    fa.replicateA(5) should === (Some(List(1,1,1,1,1)))
  }

  test("whenA return given argument when cond is true") {
    forAll { (l: List[Int]) =>
      l.whenA(true) should === (List.fill(l.length)(()))
    }
  }

  test("whenA lift Unit to F when cond is false") {
    forAll { (l: List[Int]) =>
      l.whenA(false) should === (List(()))
    }
  }

  test("unlessA return given argument when cond is false") {
    forAll { (l: List[Int]) =>
      l.unlessA(false) should === (List.fill(l.length)(()))
    }
  }

  test("unlessA lift Unit to F when cond is true") {
    forAll { (l: List[Int]) =>
      l.unlessA(true) should === (List(()))
    }
  }

  implicit val listwrapperApplicative = ListWrapper.applicative
  implicit val listwrapperCoflatMap = Applicative.coflatMap[ListWrapper]
  checkAll("Applicative[ListWrapper].coflatMap", CoflatMapTests[ListWrapper].coflatMap[String, String, String])

  implicit val validatedCoflatMap = Applicative.coflatMap[Validated[String, ?]]
  checkAll("Applicative[Validated].coflatMap", CoflatMapTests[Validated[String, ?]].coflatMap[String, String, String])

  implicit val constCoflatMap = Applicative.coflatMap[Const[String, ?]]
  checkAll("Applicative[Const].coflatMap", CoflatMapTests[Const[String, ?]].coflatMap[String, String, String])

}

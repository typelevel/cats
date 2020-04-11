package cats.tests

import cats.{Align, Applicative, Apply, CoflatMap}
import cats.data.{Const, Validated}
import cats.kernel.Monoid
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{AlignTests, CoflatMapTests}
import cats.syntax.applicative._

class ApplicativeSuite extends CatsSuite {

  test("replicateA creates a List of 'n' copies of given Applicative 'fa'") {
    val A = Applicative[Option]
    val fa = A.pure(1)
    fa.replicateA(5) should ===(Some(List(1, 1, 1, 1, 1)))
  }

  test("whenA return given argument when cond is true") {
    forAll { (l: List[Int]) =>
      l.whenA(true) should ===(List.fill(l.length)(()))
    }
  }

  test("whenA lift Unit to F when cond is false") {
    forAll { (l: List[Int]) =>
      l.whenA(false) should ===(List(()))
    }
  }

  test("unlessA return given argument when cond is false") {
    forAll { (l: List[Int]) =>
      l.unlessA(false) should ===(List.fill(l.length)(()))
    }
  }

  test("unlessA lift Unit to F when cond is true") {
    forAll { (l: List[Int]) =>
      l.unlessA(true) should ===(List(()))
    }
  }

  {
    implicit val optionMonoid: Monoid[Option[Int]] = Applicative.monoid[Option, Int]
    checkAll("Applicative[Option].monoid", MonoidTests[Option[Int]](optionMonoid).monoid)
  }

  {
    val optionSemigroupFromApply = Apply.semigroup[Option, Int]
    checkAll("Apply[Option].semigroup", SemigroupTests[Option[Int]](optionSemigroupFromApply).semigroup)
  }

  {
    implicit val listwrapperApplicative: Applicative[ListWrapper] = ListWrapper.applicative
    implicit val listwrapperCoflatMap: CoflatMap[ListWrapper] = Applicative.coflatMap[ListWrapper]
    checkAll("Applicative[ListWrapper].coflatMap", CoflatMapTests[ListWrapper].coflatMap[String, String, String])

    implicit val validatedCoflatMap: CoflatMap[Validated[String, *]] = Applicative.coflatMap[Validated[String, *]]
    checkAll("Applicative[Validated].coflatMap", CoflatMapTests[Validated[String, *]].coflatMap[String, String, String])

    implicit val constCoflatMap: CoflatMap[Const[String, *]] = Applicative.coflatMap[Const[String, *]]
    checkAll("Applicative[Const].coflatMap", CoflatMapTests[Const[String, *]].coflatMap[String, String, String])

    implicit val listwrapperAlign: Align[ListWrapper] = Apply.align[ListWrapper]
    checkAll("Apply[ListWrapper].align", AlignTests[ListWrapper].align[Int, Int, Int, Int])

    implicit val validatedAlign: Align[Validated[String, *]] = Apply.align[Validated[String, *]]
    checkAll("Apply[Validated].align", AlignTests[Validated[String, *]].align[Int, Int, Int, Int])

    implicit val constAlign: Align[Const[String, *]] = Apply.align[Const[String, *]]
    checkAll("Apply[Const].align", AlignTests[Const[String, *]].align[Int, Int, Int, Int])
  }

}

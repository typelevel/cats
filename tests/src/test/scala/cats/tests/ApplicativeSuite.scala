package cats
package tests

import cats.Applicative
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}


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

  {
    implicit val listwrapperApplicative = ListWrapper.applicative
    implicit val listwrapperMonoid = Applicative.monoid[ListWrapper, Int]
    checkAll("Applicative[ListWrapper].monoid", MonoidTests[ListWrapper[Int]].monoid)
  }

  {
    implicit val listwrapperApplicative = ListWrapper.applyInstance
    implicit val listwrapperSemigroup = Apply.semigroup[ListWrapper, Int]
    checkAll("Apply[ListWrapper].semigroup", SemigroupTests[ListWrapper[Int]].semigroup)
  }

}

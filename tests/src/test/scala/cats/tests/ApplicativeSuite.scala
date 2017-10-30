package cats
package tests

import cats.Applicative


class ApplicativeSuite extends CatsSuite {

  test("Applicative#traverse is equivalent to Traverse#traverse") {
    val f: (Int) => Option[Int] = x => Some(x + 1)
    Applicative[Option].traverse(List(1, 2))(f) should ===(Traverse[List].traverse(List(1, 2))(f))
  }

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

}

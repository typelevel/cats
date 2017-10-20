package cats.tests

import cats.evidence._

class EvidenceSuite extends CatsSuite {

  test("Is / Leibniz") {

    def cast1[A, B](as: List[A])(implicit ev: A Is B): List[B] =
      ev.substitute(as)
    cast1[Int, Int](1 :: 2 :: 3 :: Nil)

    def cast2[A, B](as: List[A])(implicit ev: Leibniz[A, B]): List[B] =
      ev.substitute(as)
    cast2[Int, Int](1 :: 2 :: 3 :: Nil)

  }

}

package cats.tests

import cats.laws.FunctorLaws
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import cats.free._
import cats.Monad

class CodensityTests extends FunSuite with Discipline {

  test("codensity on list") {
    def list3[A](as: List[A], bs: List[A], cs: List[A]) = (as ++ bs) ++ cs

    type DList[A] = Codensity[List, A]

    // implicitly[Monad[List]]
    def append[A](as: DList[A], bs: DList[A]) = new Codensity[List, A] {
      def apply[B](f: A => List[B]): List[B] = as.apply(f) ++ bs.apply(f)
    }

    def dlist3[A](as: List[A], bs: List[A], cs: List[A]) = append(Codensity.rep(as), Codensity.rep(bs))
  }

}

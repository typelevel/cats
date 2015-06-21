package cats
package tests

import cats.arrow.Arrow
import cats.data.Kleisli
import Kleisli.kleisli
import cats.functor.Strong
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary

class KleisliTests extends CatsSuite {
  implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
    Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

  checkAll("Kleisli[Option, Int, Int]", ApplicativeTests[Kleisli[Option, Int, ?]].applicative[Int, Int, Int])
  checkAll("Applicative[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Applicative[Kleisli[Option, Int, ?]]))

  checkAll("Kleisli[Option, Int, Int]", StrongTests[Kleisli[Option, ?, ?]].strong[Int, Int, Int, Int, Int, Int])
  checkAll("Strong[Kleisli[Option, ?, ?]]", SerializableTests.serializable(Strong[Kleisli[Option, ?, ?]]))

  checkAll("Kleisli[Option, Int, Int]", ArrowTests[Kleisli[Option, ?, ?]].arrow[Int, Int, Int, Int, Int, Int])
  checkAll("Arrow[Kleisli[Option, ?, ?]]", SerializableTests.serializable(Arrow[Kleisli[Option, ?, ?]]))

  test("lift") {
    val f = kleisli { (x: Int) => (Some(x + 1): Option[Int]) }
    val l = f.lift[List]
    assert((List(1, 2, 3) >>= l.run) == List(Some(2), Some(3), Some(4)))
  }
}

package cats.tests

import algebra.Eq
import cats.data.Kleisli
import cats.laws.discipline.ApplicativeTests
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary

class KleisliTests extends CatsSuite {

  implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
    Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

  checkAll("Kleisli[Option,Int, Int]", ApplicativeTests[Kleisli[Option, Int, ?]].applicative[Int, Int, Int])
}

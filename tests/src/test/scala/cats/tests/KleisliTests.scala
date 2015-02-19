package cats.tests

import algebra.Eq
import cats.data.Kleisli
import cats.laws.discipline.ApplicativeTests
import cats.laws.discipline.eq._
import cats.std.int._
import cats.std.option._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline


class KleisliTests extends FunSuite with Discipline {

  implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
    Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

  checkAll("Kleisli[Option,Int, Int]", ApplicativeTests[Kleisli[Option, Int, ?]].applicative[Int, Int, Int])
}

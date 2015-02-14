package cats.tests

import algebra.Eq
import cats.data.Cokleisli
import cats.laws.discipline.FunctorTests
import cats.laws.discipline.eq._
import cats.std.int._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline


class CokleisliTests extends FunSuite with Discipline {

  implicit def cokleisliEq[F[_], A, B](implicit A: Arbitrary[F[A]], FB: Eq[B]): Eq[Cokleisli[F, A, B]] =
    Eq.by[Cokleisli[F, A, B], F[A] => B](_.run)

  checkAll("Cokleisli[Option,Int, Int]", FunctorTests[Cokleisli[Option, Int, ?], Int].applicative[Int, Int])
}

package cats.tests

import algebra.Eq
import cats.data.Kleisli
import cats.laws.FunctorLaws
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import cats.Traverse
import cats.std.list._
import cats.std.option._
import cats.std.int._


class KleisliTests extends FunSuite with Discipline {

  // create an approximation of Eq[Kleisli[F, A, B]] by generating 100 values for A
  // and compare the result of applying these A to two kleisli
  implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] = new Eq[Kleisli[F, A, B]] {
    def eqv(x: Kleisli[F, A, B], y: Kleisli[F, A, B]): Boolean =
      Traverse[List].traverse(List.fill(100)(0))(_ => A.arbitrary.sample).map( samples =>
        samples.forall(s => FB.eqv(x.runKleisli(s), y.runKleisli(s)) )
      ).getOrElse(sys.error("Could not generate 100 values"))

  }

  checkAll("Kleisli[Option,Int, Int]", FunctorLaws[Kleisli[Option, Int, ?], Int].applicative[Int, Int])
}
package cats.free

import cats.Functor
import cats.kernel.laws.discipline.EqTests
import cats.tests.CatsSuite

import org.scalacheck.Cogen

// this functionality doesn't exist on Scala 2.12
class FreeStructuralSuite extends CatsSuite {
  import FreeSuite.freeArbitrary

  implicit def freeCogen[S[_]: Functor, A](implicit S: => Cogen[S[Free[S, A]]], A: Cogen[A]): Cogen[Free[S, A]] =
    Cogen { (seed, f) =>
      f.resume match {
        case Left(sf) =>
          S.perturb(seed, sf)

        case Right(a) =>
          A.perturb(seed, a)
      }
    }

  checkAll("Free[Option, Int]", EqTests[Free[Option, Int]].eqv)
}

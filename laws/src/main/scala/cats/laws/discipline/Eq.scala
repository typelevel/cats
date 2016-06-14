package cats
package laws
package discipline

import catalysts.Platform
import cats.std.string._
import org.scalacheck.Arbitrary

object eq {

  /**
   * Create an approximation of Eq[A => B] by generating 100 values for A
   * and comparing the application of the two functions.
   */
  implicit def catsLawsEqForFn1[A, B](implicit A: Arbitrary[A], B: Eq[B]): Eq[A => B] = new Eq[A => B] {
    val sampleCnt: Int = if (Platform.isJvm) 50 else 5

    def eqv(f: A => B, g: A => B): Boolean = {
      val samples = List.fill(sampleCnt)(A.arbitrary.sample).collect{
        case Some(a) => a
        case None => sys.error("Could not generate arbitrary values to compare two functions")
      }
      samples.forall(s => B.eqv(f(s), g(s)) )
    }
  }

  /** Create an approximation of Eq[Show[A]] by using catsLawsEqForFn1[A, String] */
  implicit def catsLawsEqForShow[A: Arbitrary]: Eq[Show[A]] = {
    val xyz = catsLawsEqForFn1[A, String]
    Eq.by[Show[A], A => String] { showInstance =>
      (a: A) => showInstance.show(a)
    }
  }

  /**
   * Create an approximation of Eq[Semigroup[A]] by generating values for A
   * and comparing the application of the two combine functions.
   */
  implicit def catsLawsEqForSemigroup[A](implicit arbAA: Arbitrary[(A, A)], eqA: Eq[A]): Eq[Semigroup[A]] =
    catsLawsEqForFn1[(A, A), A].on(f =>
      Function.tupled((x, y) => f.combine(x, y))
    )

  implicit def catsLawsEqForMonoid[A](implicit eqSA: Eq[Semigroup[A]], eqA: Eq[A]): Eq[Monoid[A]] = new Eq[Monoid[A]] {
    def eqv(f: Monoid[A], g: Monoid[A]): Boolean = {
      eqSA.eqv(f, g) && eqA.eqv(f.empty, g.empty)
    }
  }
}

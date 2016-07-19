package cats
package laws
package discipline

import catalysts.Platform
import cats.instances.string._
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
    Eq.by[Show[A], A => String] { showInstance =>
      (a: A) => showInstance.show(a)
    }
  }

  /**
   * Create an approximation of Eq[Eq[A]] by generating 100 values for A
   * and comparing the application of the two eqv functions
   */
  implicit def catsLawsEqForEq[A](implicit arbA: Arbitrary[(A, A)]): Eq[Eq[A]] = new Eq[Eq[A]] {
    def eqv(f: Eq[A], g: Eq[A]): Boolean = {
      val samples = List.fill(100)(arbA.arbitrary.sample).collect {
        case Some(a) => a
        case None => sys.error("Could not generate arbitrary values to compare two Eq[A]")
      }
      samples.forall {
        case (l, r) => f.eqv(l, r) == g.eqv(l, r)
      }
    }
  }

  /**
   * Create an approximation of Eq[PartialOrder[A]] by generating 100 values for A
   * and comparing the application of the two compare functions
   */
  implicit def catsLawsEqForPartialOrder[A](implicit arbA: Arbitrary[(A, A)], optIntEq: Eq[Option[Int]]): Eq[PartialOrder[A]] = new Eq[PartialOrder[A]] {
    def eqv(f: PartialOrder[A], g: PartialOrder[A]): Boolean = {
      val samples = List.fill(100)(arbA.arbitrary.sample).collect {
        case Some(a) => a
        case None => sys.error("Could not generate arbitrary values to compare two PartialOrder[A]")
      }
      samples.forall {
        case (l, r) => optIntEq.eqv(f.tryCompare(l, r), g.tryCompare(l, r))
      }
    }
  }

  /**
   * Create an approximation of Eq[Order[A]] by generating 100 values for A
   * and comparing the application of the two compare functions
   */
  implicit def catsLawsEqForOrder[A](implicit arbA: Arbitrary[(A, A)]): Eq[Order[A]] = new Eq[Order[A]] {
    def eqv(f: Order[A], g: Order[A]): Boolean = {
      val samples = List.fill(100)(arbA.arbitrary.sample).collect {
        case Some(a) => a
        case None => sys.error("Could not generate arbitrary values to compare two Order[A]")
      }
      samples.forall {
        case (l, r) => f.compare(l, r) == g.compare(l, r)
      }
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

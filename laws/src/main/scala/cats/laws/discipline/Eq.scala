package cats
package laws
package discipline

import catalysts.Platform
import cats.instances.boolean._
import cats.instances.int._
import cats.instances.string._
import cats.syntax.eq._
import org.scalacheck.Arbitrary

object eq {

  /**
   * Create an approximation of Eq[A => B] by generating 100 values for A
   * and comparing the application of the two functions.
   */
  implicit def catsLawsEqForFn1[A, B](implicit A: Arbitrary[A], B: Eq[B]): Eq[A => B] = new Eq[A => B] {
    val sampleCnt: Int = if (Platform.isJvm) 50 else 30

    def eqv(f: A => B, g: A => B): Boolean = {
      val samples = List.fill(sampleCnt)(A.arbitrary.sample).collect{
        case Some(a) => a
        case None => sys.error("Could not generate arbitrary values to compare two functions")
      }
      samples.forall(s => B.eqv(f(s), g(s)) )
    }
  }

  /**
   * Create an approximation of Eq[(A, B) => C] by generating 100 values for A and B
   * and comparing the application of the two functions.
   */
  implicit def catsLawsEqForFn2[A, B, C](implicit A: Arbitrary[A], B: Arbitrary[B], C: Eq[C]): Eq[(A, B) => C] = new Eq[(A, B) => C] {
    val sampleCnt: Int = if (Platform.isJvm) 50 else 5

    def eqv(f: (A, B) => C, g: (A, B) => C): Boolean = {
      val samples = List.fill(sampleCnt)((A.arbitrary.sample, B.arbitrary.sample)).collect{
        case (Some(a), Some(b)) => (a, b)
        case _ => sys.error("Could not generate arbitrary values to compare two functions")
      }
      samples.forall { case (a, b) => C.eqv(f(a, b), g(a, b)) }
    }
  }

  /** Create an approximation of Eq[Show[A]] by using catsLawsEqForFn1[A, String] */
  implicit def catsLawsEqForShow[A: Arbitrary]: Eq[Show[A]] = {
    Eq.by[Show[A], A => String] { showInstance =>
      (a: A) => showInstance.show(a)
    }
  }

  /**
   * Create an approximate Eq instance for some type A, by comparing
   * the behavior of `f(x, b)` and `f(y, b)` across many `b` samples.
   */
  def sampledEq[A, B: Arbitrary, C: Eq](samples: Int)(f: (A, B) => C): Eq[A] =
    new Eq[A] {
      val gen = Arbitrary.arbitrary[B]
      def eqv(x: A, y: A): Boolean =
        Iterator.range(1, samples)
          .map(_ => gen.sample)
          .map(_.getOrElse(sys.error(s"generator $gen failed")))
          .forall { b => f(x, b) === f(y, b) }
    }

  implicit def catsLawsEqForEq[A](implicit arbA: Arbitrary[(A, A)]): Eq[Eq[A]] =
    sampledEq[Eq[A], (A, A), Boolean](100) { case (e, (l, r)) => e.eqv(l, r) }

  implicit def catsLawsEqForEquiv[A](implicit arbA: Arbitrary[(A, A)]): Eq[Equiv[A]] =
    sampledEq[Equiv[A], (A, A), Boolean](100) { case (e, (l, r)) => e.equiv(l, r) }

  implicit def catsLawsEqForPartialOrder[A](implicit arbA: Arbitrary[(A, A)], optIntEq: Eq[Option[Int]]): Eq[PartialOrder[A]] =
    sampledEq[PartialOrder[A], (A, A), Option[Int]](100) { case (p, (l, r)) => p.tryCompare(l, r) }

  implicit def catsLawsEqForPartialOrdering[A](implicit arbA: Arbitrary[(A, A)], optIntEq: Eq[Option[Int]]): Eq[PartialOrdering[A]] =
    sampledEq[PartialOrdering[A], (A, A), Option[Int]](100) { case (p, (l, r)) => p.tryCompare(l, r) }

  implicit def catsLawsEqForOrder[A](implicit arbA: Arbitrary[(A, A)]): Eq[Order[A]] =
    sampledEq[Order[A], (A, A), Int](100) { case (p, (l, r)) => p.compare(l, r) }

  implicit def catsLawsEqForOrdering[A](implicit arbA: Arbitrary[(A, A)]): Eq[Ordering[A]] =
    sampledEq[Ordering[A], (A, A), Int](100) { case (p, (l, r)) => p.compare(l, r) }

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

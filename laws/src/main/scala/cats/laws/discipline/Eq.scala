package cats
package laws
package discipline

import catalysts.Platform

import cats.data.RepresentableStore
import cats.Eq
import cats.data.AndThen
import cats.instances.boolean._
import cats.instances.int._
import cats.instances.string._
import cats.kernel._
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

  /** `Eq[AndThen]` instance, built by piggybacking on [[catsLawsEqForFn1]]. */
  implicit def catsLawsEqForAndThen[A, B](implicit A: Arbitrary[A], B: Eq[B]): Eq[AndThen[A, B]] =
    Eq.instance(catsLawsEqForFn1[A, B].eqv(_, _))

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
   * Creates an approximation of Eq[Hash[A]] by generating 100 values for A
   * and comparing the application of the two hash functions.
   */
  implicit def catsLawsEqForHash[A](implicit arbA: Arbitrary[A]): Eq[Hash[A]] = new Eq[Hash[A]] {
    def eqv(f: Hash[A], g: Hash[A]): Boolean = {
      val samples = List.fill(100)(arbA.arbitrary.sample).collect {
        case Some(a) => a
        case None => sys.error("Could not generate arbitrary values to compare two Hash[A]")
      }
      samples.forall { x => f.hash(x) == g.hash(x) }
    }
  }

  /**
   * Create an approximation of Eq[Semigroup[A]] by generating values for A
   * and comparing the application of the two combine functions.
   */
  implicit def catsLawsEqForSemigroup[A](implicit arbAA: Arbitrary[(A, A)], eqA: Eq[A]): Eq[Semigroup[A]] = {
    val instance: Eq[((A, A)) => A] = catsLawsEqForFn1[(A, A), A]
    Eq.by[Semigroup[A], ((A, A)) => A]( f => Function.tupled((x, y) => f.combine(x, y)))(instance)
  }

  implicit def catsLawsEqForCommutativeSemigroup[A](implicit arbAA: Arbitrary[(A, A)], eqA: Eq[A]): Eq[CommutativeSemigroup[A]] = {
    implicit val eqABool: Eq[(A, Boolean)] = Eq.instance {
      case ((x, boolX), (y, boolY)) => x === y && boolX === boolY
    }

    Eq.by[CommutativeSemigroup[A], ((A, A)) => (A, Boolean)](f =>
      Function.tupled((x, y) => (f.combine(x, y), f.combine(x, y) === f.combine(y, x)))
    )(catsLawsEqForFn1[(A, A), (A, Boolean)])
  }

  implicit def catsLawsEqForBand[A](implicit arbAA: Arbitrary[(A, A)], eqSA: Eq[Semigroup[A]], eqA: Eq[A]): Eq[Band[A]] = {
    Eq.by[Band[A], ((A, A)) => Boolean](f =>
      Function.tupled((x, y) => f.combine(x, y) === f.combine(f.combine(x, y), y))
    )(catsLawsEqForFn1[(A, A), Boolean])
  }

  implicit def catsLawsEqForMonoid[A](implicit eqSA: Eq[Semigroup[A]], eqA: Eq[A]): Eq[Monoid[A]] = new Eq[Monoid[A]] {
    def eqv(f: Monoid[A], g: Monoid[A]): Boolean = {
      eqSA.eqv(f, g) && eqA.eqv(f.empty, g.empty)
    }
  }

  implicit def catsLawsEqForSemilattice[A](implicit eqBA: Eq[Band[A]], eqCA: Eq[CommutativeSemigroup[A]], eqA: Eq[A]): Eq[Semilattice[A]] =
    Eq.instance((f, g) => eqBA.eqv(f, g) && eqCA.eqv(f, g))

  implicit def catsLawsEqForCommutativeMonoid[A](implicit eqSA: Eq[CommutativeSemigroup[A]], eqMA: Eq[Monoid[A]], eqA: Eq[A]): Eq[CommutativeMonoid[A]] =
    Eq.instance((f, g) => eqSA.eqv(f, g) && eqMA.eqv(f, g))

  implicit def catsLawsEqForBoundedSemilattice[A](implicit eqSA: Eq[Semilattice[A]], eqCA: Eq[CommutativeMonoid[A]], eqA: Eq[A]): Eq[BoundedSemilattice[A]] =
    Eq.instance((f, g) => eqSA.eqv(f, g) && eqCA.eqv(f, g))

  implicit def catsLawsEqForGroup[A](implicit arbAA: Arbitrary[(A, A)], eqMA: Eq[Monoid[A]], eqA: Eq[A]): Eq[Group[A]] = {
    implicit val eqABool: Eq[(A, Boolean)] = Eq.instance {
      case ((x, boolX), (y, boolY)) => x === y && boolX === boolY
    }

    val inverseEq = Eq.by[Group[A], ((A, A)) => (A, Boolean)](f =>
      Function.tupled((x, y) => (
        f.combine(x, y),
        f.combine(f.inverse(x), x) === f.empty && f.combine(x, f.inverse(x)) === f.empty &&
          f.combine(f.inverse(y), y) === f.empty && f.combine(y, f.inverse(y)) === f.empty &&
          f.inverse(f.empty) == f.empty
      )
    ))(catsLawsEqForFn1[(A, A), (A, Boolean)])

    Eq.instance((f, g) => eqMA.eqv(f, g) && inverseEq.eqv(f, g))
  }

  implicit def catsLawsEqForCommutativeGroup[A](implicit eqMA: Eq[CommutativeMonoid[A]], eqGA: Eq[Group[A]], eqA: Eq[A]): Eq[CommutativeGroup[A]] =
    Eq.instance((f, g) => eqMA.eqv(f, g) && eqGA.eqv(f, g))

  implicit def catsLawsEqForRepresentableStore[F[_]: Representable, S, A](implicit eqFA: Eq[F[A]], eqS: Eq[S]): Eq[RepresentableStore[F, S, A]] = {
    Eq.instance((s1, s2) => eqFA.eqv(s1.fa, s2.fa) && eqS.eqv(s1.index, s2.index))
  }
}

package cats
package laws
package discipline

import cats.data.RepresentableStore
import cats.Eq
import cats.data.AndThen
import cats.instances.boolean._
import cats.instances.int._
import cats.instances.string._
import cats.instances.tuple._
import cats.kernel._
import cats.platform.Platform
import cats.syntax.eq._
import org.scalacheck.Arbitrary

object eq extends DisciplineBinCompatEqInstances {

  implicit def catsLawsEqForFn1Exhaustive[A, B](implicit A: ExhaustiveCheck[A], B: Eq[B]): Eq[A => B] =
    Eq.instance((f, g) => A.allValues.forall(a => B.eqv(f(a), g(a))))

  implicit def catsLawsEqForFn2BinCompat[A, B, C](implicit ev: Eq[((A, B)) => C]): Eq[(A, B) => C] =
    Eq.by((_: (A, B) => C).tupled)

  implicit def catsLawsEqForAndThenBinCompat[A, B](implicit eqAB: Eq[A => B]): Eq[AndThen[A, B]] =
    Eq.by[AndThen[A, B], A => B](identity)

  implicit def catsLawsEqForShowBinCompat[A](implicit ev: Eq[A => String]): Eq[Show[A]] =
    Eq.by[Show[A], A => String](showA => a => showA.show(a))

  implicit def catsLawsEqForEqBinCompt[A](implicit ev: Eq[(A, A) => Boolean]): Eq[Eq[A]] =
    Eq.by[Eq[A], (A, A) => Boolean](e => (a1, a2) => e.eqv(a1, a2))

  implicit def catsLawsEqForEquivBinCompat[A](implicit ev: Eq[(A, A) => Boolean]): Eq[Equiv[A]] =
    Eq.by[Equiv[A], (A, A) => Boolean](e => (a1, a2) => e.equiv(a1, a2))

  implicit def catsLawsEqForPartialOrderBinCompat[A](implicit ev: Eq[(A, A) => Option[Int]]): Eq[PartialOrder[A]] =
    Eq.by[PartialOrder[A], (A, A) => Option[Int]](o => (a1, a2) => o.tryCompare(a1, a2))

  implicit def catsLawsEqForPartialOrderingBinCompat[A](
    implicit ev: Eq[(A, A) => Option[Int]]
  ): Eq[PartialOrdering[A]] =
    Eq.by[PartialOrdering[A], (A, A) => Option[Int]]((o: PartialOrdering[A]) => (a1, a2) => o.tryCompare(a1, a2))

  implicit def catsLawsEqForOrderBinCompat[A](implicit ev: Eq[(A, A) => Int]): Eq[Order[A]] =
    Eq.by[Order[A], (A, A) => Int](o => (a1, a2) => o.compare(a1, a2))

  implicit def catsLawsEqForOrderingBinCompat[A](implicit ev: Eq[(A, A) => Int]): Eq[Ordering[A]] =
    Eq.by[Ordering[A], (A, A) => Int](o => (a1, a2) => o.compare(a1, a2))

  implicit def catsLawsEqForHashBinCompat[A](implicit ev: Eq[A => Int]): Eq[Hash[A]] =
    Eq.by[Hash[A], A => Int](h => a => h.hash(a))

  implicit def catsLawsEqForSemigroupBinCompat[A](implicit ev: Eq[(A, A) => A]): Eq[Semigroup[A]] =
    Eq.by[Semigroup[A], (A, A) => A](s => (a1, a2) => s.combine(a1, a2))

  implicit def catsLawsEqForCommutativeSemigroupBinCompat[A](implicit eqA: Eq[A],
                                                             ev: Eq[(A, A) => (A, A)]): Eq[CommutativeSemigroup[A]] =
    Eq.by[CommutativeSemigroup[A], (A, A) => (A, A)](s => (x, y) => (s.combine(x, y), s.combine(y, x)))

  implicit def catsLawsEqForBandBinCompat[A](implicit ev: Eq[(A, A) => (A, A)]): Eq[Band[A]] =
    Eq.by[Band[A], (A, A) => (A, A)](
      f => (x, y) => (f.combine(x, y), f.combine(f.combine(x, y), y))
    )

  implicit def catsLawsEqForGroupBinCompat[A](implicit ev1: Eq[(A, A) => (A, Boolean)], eqA: Eq[A]): Eq[Group[A]] =
    Eq.by[Group[A], (A, A) => (A, Boolean)](
      f =>
        (x, y) =>
          (
            f.combine(x, y),
            f.combine(f.inverse(x), x) === f.empty && f.combine(x, f.inverse(x)) === f.empty &&
              f.combine(f.inverse(y), y) === f.empty && f.combine(y, f.inverse(y)) === f.empty &&
              f.inverse(f.empty) == f.empty
      )
    )

  implicit def catsLawsEqForMonoid[A](implicit eqSA: Eq[Semigroup[A]], eqA: Eq[A]): Eq[Monoid[A]] = new Eq[Monoid[A]] {
    def eqv(f: Monoid[A], g: Monoid[A]): Boolean =
      eqSA.eqv(f, g) && eqA.eqv(f.empty, g.empty)
  }

  implicit def catsLawsEqForSemilattice[A](implicit eqBA: Eq[Band[A]],
                                           eqCA: Eq[CommutativeSemigroup[A]],
                                           eqA: Eq[A]): Eq[Semilattice[A]] =
    Eq.instance((f, g) => eqBA.eqv(f, g) && eqCA.eqv(f, g))

  implicit def catsLawsEqForCommutativeMonoid[A](implicit eqSA: Eq[CommutativeSemigroup[A]],
                                                 eqMA: Eq[Monoid[A]],
                                                 eqA: Eq[A]): Eq[CommutativeMonoid[A]] =
    Eq.instance((f, g) => eqSA.eqv(f, g) && eqMA.eqv(f, g))

  implicit def catsLawsEqForBoundedSemilattice[A](implicit eqSA: Eq[Semilattice[A]],
                                                  eqCA: Eq[CommutativeMonoid[A]],
                                                  eqA: Eq[A]): Eq[BoundedSemilattice[A]] =
    Eq.instance((f, g) => eqSA.eqv(f, g) && eqCA.eqv(f, g))

  implicit def catsLawsEqForCommutativeGroup[A](implicit eqMA: Eq[CommutativeMonoid[A]],
                                                eqGA: Eq[Group[A]],
                                                eqA: Eq[A]): Eq[CommutativeGroup[A]] =
    Eq.instance((f, g) => eqMA.eqv(f, g) && eqGA.eqv(f, g))

  implicit def catsLawsEqForRepresentableStore[F[_]: Representable, S, A](implicit eqFA: Eq[F[A]],
                                                                          eqS: Eq[S]): Eq[RepresentableStore[F, S, A]] =
    Eq.instance((s1, s2) => eqFA.eqv(s1.fa, s2.fa) && eqS.eqv(s1.index, s2.index))
}

/**
 * These instances are questionable and can lead to false positives. For the sake of compatibility,
 * they haven't been removed, but they should be considered to be deprecated, and we put them in a
 * lower implicit scope priority.
 */
private[discipline] trait DisciplineBinCompatEqInstances {

  /**
   * Create an approximation of Eq[A => B] by generating random values for A
   * and comparing the application of the two functions.
   */
  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForFn1Exhaustive instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForFn1[A, B](implicit A: Arbitrary[A], B: Eq[B]): Eq[A => B] = new Eq[A => B] {
    val sampleCnt: Int = if (Platform.isJvm) 50 else 30

    def eqv(f: A => B, g: A => B): Boolean = {
      val samples = List.fill(sampleCnt)(A.arbitrary.sample).collect {
        case Some(a) => a
        case None    => sys.error("Could not generate arbitrary values to compare two functions")
      }
      samples.forall(s => B.eqv(f(s), g(s)))
    }
  }

  /**
   * Create an approximation of Eq[(A, B) => C] by generating random values for A and B
   * and comparing the application of the two functions.
   */
  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForFn2BinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForFn2[A, B, C](implicit A: Arbitrary[A], B: Arbitrary[B], C: Eq[C]): Eq[(A, B) => C] =
    Eq.by((_: (A, B) => C).tupled)(catsLawsEqForFn1)

  /** `Eq[AndThen]` instance, built by piggybacking on [[catsLawsEqForFn1]]. */
  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForAndThenBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForAndThen[A, B](implicit A: Arbitrary[A], B: Eq[B]): Eq[AndThen[A, B]] =
    Eq.instance(catsLawsEqForFn1[A, B].eqv(_, _))

  /** Create an approximation of Eq[Show[A]] by using catsLawsEqForFn1[A, String] */
  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForShowBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForShow[A: Arbitrary]: Eq[Show[A]] =
    Eq.by[Show[A], A => String] { showInstance => (a: A) =>
      showInstance.show(a)
    }(catsLawsEqForFn1)

  /**
   * Create an approximate Eq instance for some type A, by comparing
   * the behavior of `f(x, b)` and `f(y, b)` across many `b` samples.
   */
  @deprecated(
    "This method is problematic and will most likely be removed in a future version of Cats. You may want to use an approach based on catsLawsEqForFn1Exhaustive instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  def sampledEq[A, B: Arbitrary, C: Eq](samples: Int)(f: (A, B) => C): Eq[A] =
    new Eq[A] {
      val gen = Arbitrary.arbitrary[B]
      def eqv(x: A, y: A): Boolean =
        Iterator
          .range(1, samples)
          .map(_ => gen.sample)
          .map(_.getOrElse(sys.error(s"generator $gen failed")))
          .forall { b =>
            f(x, b) === f(y, b)
          }
    }

  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForEqBinCompt instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForEq[A](implicit arbA: Arbitrary[(A, A)]): Eq[Eq[A]] =
    sampledEq[Eq[A], (A, A), Boolean](100) { case (e, (l, r)) => e.eqv(l, r) }

  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForEquivBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForEquiv[A](implicit arbA: Arbitrary[(A, A)]): Eq[Equiv[A]] =
    sampledEq[Equiv[A], (A, A), Boolean](100) { case (e, (l, r)) => e.equiv(l, r) }

  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForPartialOrderBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForPartialOrder[A](implicit arbA: Arbitrary[(A, A)],
                                            optIntEq: Eq[Option[Int]]): Eq[PartialOrder[A]] =
    sampledEq[PartialOrder[A], (A, A), Option[Int]](100) { case (p, (l, r)) => p.tryCompare(l, r) }

  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForPartialOrderingBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForPartialOrdering[A](implicit arbA: Arbitrary[(A, A)],
                                               optIntEq: Eq[Option[Int]]): Eq[PartialOrdering[A]] =
    sampledEq[PartialOrdering[A], (A, A), Option[Int]](100) { case (p, (l, r)) => p.tryCompare(l, r) }

  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForOrderBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForOrder[A](implicit arbA: Arbitrary[(A, A)]): Eq[Order[A]] =
    sampledEq[Order[A], (A, A), Int](100) { case (p, (l, r)) => p.compare(l, r) }

  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForOrderingBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForOrdering[A](implicit arbA: Arbitrary[(A, A)]): Eq[Ordering[A]] =
    sampledEq[Ordering[A], (A, A), Int](100) { case (p, (l, r)) => p.compare(l, r) }

  /**
   * Creates an approximation of Eq[Hash[A]] by generating 100 values for A
   * and comparing the application of the two hash functions.
   */
  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForHashBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForHash[A](implicit arbA: Arbitrary[A]): Eq[Hash[A]] = new Eq[Hash[A]] {
    def eqv(f: Hash[A], g: Hash[A]): Boolean = {
      val samples = List.fill(100)(arbA.arbitrary.sample).collect {
        case Some(a) => a
        case None    => sys.error("Could not generate arbitrary values to compare two Hash[A]")
      }
      samples.forall { x =>
        f.hash(x) == g.hash(x)
      }
    }
  }

  /**
   * Create an approximation of Eq[Semigroup[A]] by generating values for A
   * and comparing the application of the two combine functions.
   */
  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForSemigroupBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForSemigroup[A](implicit arbAA: Arbitrary[(A, A)], eqA: Eq[A]): Eq[Semigroup[A]] = {
    val instance: Eq[((A, A)) => A] = catsLawsEqForFn1[(A, A), A]
    Eq.by[Semigroup[A], ((A, A)) => A](f => Function.tupled((x, y) => f.combine(x, y)))(instance)
  }

  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForCommutativeSemigroupBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForCommutativeSemigroup[A](implicit arbAA: Arbitrary[(A, A)],
                                                    eqA: Eq[A]): Eq[CommutativeSemigroup[A]] = {
    implicit val eqABool: Eq[(A, Boolean)] = Eq.instance {
      case ((x, boolX), (y, boolY)) => x === y && boolX === boolY
    }

    Eq.by[CommutativeSemigroup[A], ((A, A)) => (A, Boolean)](
      f => Function.tupled((x, y) => (f.combine(x, y), f.combine(x, y) === f.combine(y, x)))
    )(catsLawsEqForFn1[(A, A), (A, Boolean)])
  }

  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForBandBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForBand[A](implicit arbAA: Arbitrary[(A, A)],
                                    eqSA: Eq[Semigroup[A]],
                                    eqA: Eq[A]): Eq[Band[A]] =
    Eq.by[Band[A], ((A, A)) => Boolean](
      f => Function.tupled((x, y) => f.combine(x, y) === f.combine(f.combine(x, y), y))
    )(catsLawsEqForFn1[(A, A), Boolean])

  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForGroupBinCompat instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForGroup[A](implicit arbAA: Arbitrary[(A, A)],
                                     eqMA: Eq[Monoid[A]],
                                     eqA: Eq[A]): Eq[Group[A]] = {
    implicit val eqABool: Eq[(A, Boolean)] = Eq.instance {
      case ((x, boolX), (y, boolY)) => x === y && boolX === boolY
    }

    val inverseEq = Eq.by[Group[A], ((A, A)) => (A, Boolean)](
      f =>
        Function.tupled(
          (x, y) =>
            (
              f.combine(x, y),
              f.combine(f.inverse(x), x) === f.empty && f.combine(x, f.inverse(x)) === f.empty &&
                f.combine(f.inverse(y), y) === f.empty && f.combine(y, f.inverse(y)) === f.empty &&
                f.inverse(f.empty) == f.empty
          )
      )
    )(catsLawsEqForFn1[(A, A), (A, Boolean)])

    Eq.instance((f, g) => eqMA.eqv(f, g) && inverseEq.eqv(f, g))
  }
}

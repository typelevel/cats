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

object eq {

  implicit def catsLawsEqForFn1Exhaustive[A, B](implicit A: ExhaustiveCheck[A], B: Eq[B]): Eq[A => B] =
    Eq.instance((f, g) => A.allValues.forall(a => B.eqv(f(a), g(a))))

  implicit def catsLawsEqForFn2[A, B, C](implicit ev: Eq[((A, B)) => C]): Eq[(A, B) => C] =
    Eq.by((_: (A, B) => C).tupled)

  implicit def catsLawsEqForAndThen[A, B](implicit eqAB: Eq[A => B]): Eq[AndThen[A, B]] =
    Eq.by[AndThen[A, B], A => B](identity)

  implicit def catsLawsEqForShow[A](implicit ev: Eq[A => String]): Eq[Show[A]] =
    Eq.by[Show[A], A => String](showA => a => showA.show(a))

  implicit def catsLawsEqForEq[A](implicit ev: Eq[(A, A) => Boolean]): Eq[Eq[A]] =
    Eq.by[Eq[A], (A, A) => Boolean](e => (a1, a2) => e.eqv(a1, a2))

  implicit def catsLawsEqForEquiv[A](implicit ev: Eq[(A, A) => Boolean]): Eq[Equiv[A]] =
    Eq.by[Equiv[A], (A, A) => Boolean](e => (a1, a2) => e.equiv(a1, a2))

  implicit def catsLawsEqForPartialOrder[A](implicit ev: Eq[(A, A) => Option[Int]]): Eq[PartialOrder[A]] =
    Eq.by[PartialOrder[A], (A, A) => Option[Int]](o => (a1, a2) => o.tryCompare(a1, a2))

  implicit def catsLawsEqForPartialOrdering[A](
    implicit ev: Eq[(A, A) => Option[Int]]
  ): Eq[PartialOrdering[A]] =
    Eq.by[PartialOrdering[A], (A, A) => Option[Int]]((o: PartialOrdering[A]) => (a1, a2) => o.tryCompare(a1, a2))

  implicit def catsLawsEqForOrder[A](implicit ev: Eq[(A, A) => Int]): Eq[Order[A]] =
    Eq.by[Order[A], (A, A) => Int](o => (a1, a2) => o.compare(a1, a2))

  implicit def catsLawsEqForOrdering[A](implicit ev: Eq[(A, A) => Int]): Eq[Ordering[A]] =
    Eq.by[Ordering[A], (A, A) => Int](o => (a1, a2) => o.compare(a1, a2))

  implicit def catsLawsEqForHash[A](implicit ev: Eq[A => Int]): Eq[Hash[A]] =
    Eq.by[Hash[A], A => Int](h => a => h.hash(a))

  implicit def catsLawsEqForSemigroup[A](implicit ev: Eq[(A, A) => A]): Eq[Semigroup[A]] =
    Eq.by[Semigroup[A], (A, A) => A](s => (a1, a2) => s.combine(a1, a2))

  implicit def catsLawsEqForCommutativeSemigroup[A](implicit eqA: Eq[A],
                                                             ev: Eq[(A, A) => (A, A)]): Eq[CommutativeSemigroup[A]] =
    Eq.by[CommutativeSemigroup[A], (A, A) => (A, A)](s => (x, y) => (s.combine(x, y), s.combine(y, x)))

  implicit def catsLawsEqForBand[A](implicit ev: Eq[(A, A) => (A, A)]): Eq[Band[A]] =
    Eq.by[Band[A], (A, A) => (A, A)](
      f => (x, y) => (f.combine(x, y), f.combine(f.combine(x, y), y))
    )

  implicit def catsLawsEqForGroup[A](implicit ev1: Eq[(A, A) => (A, Boolean)], eqA: Eq[A]): Eq[Group[A]] =
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

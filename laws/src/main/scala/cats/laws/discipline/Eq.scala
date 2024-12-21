/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats
package laws
package discipline

import cats.data.{AndThen, RepresentableStore, StoreT}
import cats.instances.boolean.*
import cats.instances.int.*
import cats.instances.string.*
import cats.kernel.*
import cats.platform.Platform
import cats.syntax.eq.*
import org.scalacheck.Arbitrary

object eq {

  implicit def catsLawsEqForFn1Exhaustive[A, B](implicit A: ExhaustiveCheck[A], B: Eq[B]): Eq[A => B] =
    (f, g) => A.allValues.forall(a => B.eqv(f(a), g(a)))

  implicit def catsLawsEqForFn2[A, B, C](implicit ev: Eq[((A, B)) => C]): Eq[(A, B) => C] =
    Eq.by(_.tupled)

  implicit def catsLawsEqForPartialFunctionExhaustive[A: ExhaustiveCheck, B: Eq]: Eq[PartialFunction[A, B]] =
    (f, g) =>
      ExhaustiveCheck[A].allValues
        .filter(a => f.isDefinedAt(a) || g.isDefinedAt(a))
        .forall(a => f.isDefinedAt(a) && g.isDefinedAt(a) && Eq[B].eqv(f(a), g(a)))

  implicit def catsLawsEqForAndThen[A, B](implicit eqAB: Eq[A => B]): Eq[AndThen[A, B]] =
    Eq.by(identity[A => B])

  implicit def catsLawsEqForShow[A](implicit ev: Eq[A => String]): Eq[Show[A]] =
    Eq.by(showA => showA.show _)

  implicit def catsLawsEqForEq[A](implicit ev: Eq[(A, A) => Boolean]): Eq[Eq[A]] =
    Eq.by(e => e.eqv _)

  implicit def catsLawsEqForEquiv[A](implicit ev: Eq[(A, A) => Boolean]): Eq[Equiv[A]] =
    Eq.by(e => e.equiv _)

  implicit def catsLawsEqForPartialOrder[A](implicit ev: Eq[(A, A) => Option[Int]]): Eq[PartialOrder[A]] =
    Eq.by(o => o.tryCompare _)

  implicit def catsLawsEqForPartialOrdering[A](implicit ev: Eq[(A, A) => Option[Int]]): Eq[PartialOrdering[A]] =
    Eq.by(o => o.tryCompare _)

  implicit def catsLawsEqForOrder[A](implicit ev: Eq[(A, A) => Int]): Eq[Order[A]] =
    Eq.by(o => o.compare _)

  implicit def catsLawsEqForOrdering[A](implicit ev: Eq[(A, A) => Int]): Eq[Ordering[A]] =
    Eq.by(o => o.compare _)

  implicit def catsLawsEqForHash[A](implicit ev: Eq[A => Int]): Eq[Hash[A]] =
    Eq.by(h => h.hash _)

  implicit def catsLawsEqForSemigroup[A](implicit ev: Eq[(A, A) => A]): Eq[Semigroup[A]] =
    Eq.by(s => s.combine _)

  implicit def catsLawsEqForCommutativeSemigroup[A](implicit
    eqA: Eq[A],
    ev: Eq[(A, A) => (A, A)]
  ): Eq[CommutativeSemigroup[A]] =
    Eq.by(s => (x: A, y: A) => (s.combine(x, y), s.combine(y, x)))

  implicit def catsLawsEqForBand[A](implicit ev: Eq[(A, A) => (A, A)]): Eq[Band[A]] =
    Eq.by(f => (x: A, y: A) => (f.combine(x, y), f.combine(f.combine(x, y), y)))

  implicit def catsLawsEqForGroup[A](implicit ev1: Eq[(A, A) => (A, Boolean)], eqA: Eq[A]): Eq[Group[A]] =
    Eq.by(f => { (x: A, y: A) =>
      val xy = f.combine(x, y)
      val p1 = f.combine(f.inverse(x), x) === f.empty && f.combine(x, f.inverse(x)) === f.empty
      val p2 = f.combine(f.inverse(y), y) === f.empty && f.combine(y, f.inverse(y)) === f.empty
      val p3 = f.inverse(f.empty) == f.empty
      (xy, p1 && p2 && p3)
    })

  implicit def catsLawsEqForMonoid[A](implicit eqSA: Eq[Semigroup[A]], eqA: Eq[A]): Eq[Monoid[A]] =
    (f, g) => eqSA.eqv(f, g) && eqA.eqv(f.empty, g.empty)

  implicit def catsLawsEqForSemilattice[A](implicit
    eqBA: Eq[Band[A]],
    eqCA: Eq[CommutativeSemigroup[A]],
    eqA: Eq[A]
  ): Eq[Semilattice[A]] =
    (f, g) => eqBA.eqv(f, g) && eqCA.eqv(f, g)

  implicit def catsLawsEqForCommutativeMonoid[A](implicit
    eqSA: Eq[CommutativeSemigroup[A]],
    eqMA: Eq[Monoid[A]],
    eqA: Eq[A]
  ): Eq[CommutativeMonoid[A]] =
    (f, g) => eqSA.eqv(f, g) && eqMA.eqv(f, g)

  implicit def catsLawsEqForBoundedSemilattice[A](implicit
    eqSA: Eq[Semilattice[A]],
    eqCA: Eq[CommutativeMonoid[A]],
    eqA: Eq[A]
  ): Eq[BoundedSemilattice[A]] =
    (f, g) => eqSA.eqv(f, g) && eqCA.eqv(f, g)

  implicit def catsLawsEqForCommutativeGroup[A](implicit
    eqMA: Eq[CommutativeMonoid[A]],
    eqGA: Eq[Group[A]],
    eqA: Eq[A]
  ): Eq[CommutativeGroup[A]] =
    (f, g) => eqMA.eqv(f, g) && eqGA.eqv(f, g)

  implicit def catsLawsEqForRepresentableStore[F[_]: Representable, S, A](implicit
    eqFA: Eq[F[A]],
    eqS: Eq[S]
  ): Eq[RepresentableStore[F, S, A]] =
    (s1, s2) => eqFA.eqv(s1.fa, s2.fa) && eqS.eqv(s1.index, s2.index)

  implicit def catsLawsEqForStoreT[F[_], S, A](implicit eqF: Eq[F[S => A]], eqS: Eq[S]): Eq[StoreT[F, S, A]] =
    (s1, s2) => eqF.eqv(s1.runF, s2.runF) && eqS.eqv(s1.index, s2.index)
}

@deprecated(
  "These instances are questionable and can lead to false positives. For the sake of compatibility, they haven't been removed, but they should be considered to be deprecated, and we put them in a lower implicit scope priority.",
  "2.0"
) object DeprecatedEqInstances {

  /**
   * Create an approximation of Eq[A => B] by generating random values for A
   * and comparing the application of the two functions.
   */
  @deprecated(
    "This instance is problematic and will most likely be removed in a future version of Cats. Use catsLawsEqForFn1Exhaustive instead. See https://github.com/typelevel/cats/pull/2577 for more information.",
    "1.7"
  )
  implicit def catsLawsEqForFn1[A, B](implicit A: Arbitrary[A], B: Eq[B]): Eq[A => B] = { (f, g) =>
    val sampleCnt = if (Platform.isJvm) 50 else 30
    val samples = List.fill(sampleCnt)(A.arbitrary.sample).collect {
      case Some(a) => a
      case None    => sys.error("Could not generate arbitrary values to compare two functions")
    }
    samples.forall(s => B.eqv(f(s), g(s)))
  }

  /**
   * Create an approximation of Eq[(A, B) => C] by generating random values for A and B
   * and comparing the application of the two functions.
   */
  implicit def catsLawsEqForFn2[A, B, C](implicit A: Arbitrary[A], B: Arbitrary[B], C: Eq[C]): Eq[(A, B) => C] =
    Eq.by(_.tupled)

  /**
   * `Eq[AndThen]` instance, built by piggybacking on [[catsLawsEqForFn1]].
   */
  implicit def catsLawsEqForAndThen[A, B](implicit A: Arbitrary[A], B: Eq[B]): Eq[AndThen[A, B]] =
    Eq.by(identity[A => B])

  /**
   * Create an approximation of `Eq[Show[A]]` by using catsLawsEqForFn1[A, String]
   */
  implicit def catsLawsEqForShow[A: Arbitrary]: Eq[Show[A]] =
    Eq.by(showA => showA.show _)

  /**
   * Create an approximate Eq instance for some type A, by comparing
   * the behavior of `f(x, b)` and `f(y, b)` across many `b` samples.
   */
  def sampledEq[A, B: Arbitrary, C: Eq](samples: Int)(f: (A, B) => C): Eq[A] = {
    val gen = Arbitrary.arbitrary[B]
    (x, y) =>
      Iterator
        .range(1, samples)
        .map(_ => gen.sample)
        .map(_.getOrElse(sys.error(s"generator $gen failed")))
        .forall(b => f(x, b) === f(y, b))
  }

  implicit def catsLawsEqForEq[A](implicit arbA: Arbitrary[(A, A)]): Eq[Eq[A]] =
    sampledEq[Eq[A], (A, A), Boolean](100) { case (e, (l, r)) => e.eqv(l, r) }

  implicit def catsLawsEqForEquiv[A](implicit arbA: Arbitrary[(A, A)]): Eq[Equiv[A]] =
    sampledEq[Equiv[A], (A, A), Boolean](100) { case (e, (l, r)) => e.equiv(l, r) }

  implicit def catsLawsEqForPartialOrder[A](implicit
    arbA: Arbitrary[(A, A)],
    optIntEq: Eq[Option[Int]]
  ): Eq[PartialOrder[A]] =
    sampledEq[PartialOrder[A], (A, A), Option[Int]](100) { case (p, (l, r)) => p.tryCompare(l, r) }

  implicit def catsLawsEqForPartialOrdering[A](implicit
    arbA: Arbitrary[(A, A)],
    optIntEq: Eq[Option[Int]]
  ): Eq[PartialOrdering[A]] =
    sampledEq[PartialOrdering[A], (A, A), Option[Int]](100) { case (p, (l, r)) => p.tryCompare(l, r) }

  implicit def catsLawsEqForOrder[A](implicit arbA: Arbitrary[(A, A)]): Eq[Order[A]] =
    sampledEq[Order[A], (A, A), Int](100) { case (p, (l, r)) => p.compare(l, r) }

  implicit def catsLawsEqForOrdering[A](implicit arbA: Arbitrary[(A, A)]): Eq[Ordering[A]] =
    sampledEq[Ordering[A], (A, A), Int](100) { case (p, (l, r)) => p.compare(l, r) }

  /**
   * Creates an approximation of `Eq[Hash[A]]` by generating 100 values for A
   * and comparing the application of the two hash functions.
   */
  implicit def catsLawsEqForHash[A](implicit arbA: Arbitrary[A]): Eq[Hash[A]] = { (f, g) =>
    val samples = List.fill(100)(arbA.arbitrary.sample).collect {
      case Some(a) => a
      case None    => sys.error("Could not generate arbitrary values to compare two Hash[A]")
    }
    samples.forall(x => f.hash(x) == g.hash(x))
  }

  /**
   * Create an approximation of `Eq[Semigroup[A]]` by generating values for A
   * and comparing the application of the two combine functions.
   */
  implicit def catsLawsEqForSemigroup[A](implicit arbAA: Arbitrary[(A, A)], eqA: Eq[A]): Eq[Semigroup[A]] =
    Eq.by[Semigroup[A], ((A, A)) => A](f => { case (x, y) => f.combine(x, y) })

  implicit def catsLawsEqForCommutativeSemigroup[A](implicit
    arbAA: Arbitrary[(A, A)],
    eqA: Eq[A]
  ): Eq[CommutativeSemigroup[A]] = {
    implicit val eqABool: Eq[(A, Boolean)] = { case ((x, boolX), (y, boolY)) =>
      x === y && boolX === boolY
    }

    Eq.by[CommutativeSemigroup[A], ((A, A)) => (A, Boolean)](f => { case (x, y) =>
      (f.combine(x, y), f.combine(x, y) === f.combine(y, x))
    })
  }

  implicit def catsLawsEqForBand[A](implicit
    arbAA: Arbitrary[(A, A)],
    eqSA: Eq[Semigroup[A]],
    eqA: Eq[A]
  ): Eq[Band[A]] =
    Eq.by[Band[A], ((A, A)) => Boolean](f => { case (x, y) =>
      f.combine(x, y) === f.combine(f.combine(x, y), y)
    })

  implicit def catsLawsEqForGroup[A](implicit
    arbAA: Arbitrary[(A, A)],
    eqMA: Eq[Monoid[A]],
    eqA: Eq[A]
  ): Eq[Group[A]] = {
    implicit val eqABool: Eq[(A, Boolean)] = { case ((x, boolX), (y, boolY)) =>
      x === y && boolX === boolY
    }

    val inverseEq = Eq.by[Group[A], ((A, A)) => (A, Boolean)](f => { case (x, y) =>
      val xy = f.combine(x, y)
      val p1 = f.combine(f.inverse(x), x) === f.empty && f.combine(x, f.inverse(x)) === f.empty
      val p2 = f.combine(f.inverse(y), y) === f.empty && f.combine(y, f.inverse(y)) === f.empty
      val p3 = f.inverse(f.empty) == f.empty
      (xy, p1 && p2 && p3)
    })

    (f, g) => eqMA.eqv(f, g) && inverseEq.eqv(f, g)
  }
}

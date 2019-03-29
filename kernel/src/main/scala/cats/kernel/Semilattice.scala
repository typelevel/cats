package cats.kernel

import scala.{specialized => sp}

/**
 * Semilattices are commutative semigroups whose operation
 * (i.e. combine) is also idempotent.
 */
trait Semilattice[@sp(Int, Long, Float, Double) A] extends Any with Band[A] with CommutativeSemigroup[A] { self =>

  /**
   * Given Eq[A], return a PartialOrder[A] using the `combine`
   * operator to determine the partial ordering. This method assumes
   * `combine` functions as `meet` (that is, as a lower bound).
   *
   * This method returns:
   *
   *    0.0 if x = y
   *   -1.0 if x = combine(x, y)
   *    1.0 if y = combine(x, y)
   *    NaN otherwise
   */
  def asMeetPartialOrder(implicit ev: Eq[A]): PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A): Double =
        if (ev.eqv(x, y)) 0.0
        else {
          val z = self.combine(x, y)
          if (ev.eqv(x, z)) -1.0 else if (ev.eqv(y, z)) 1.0 else Double.NaN
        }
    }

  /**
   * Given Eq[A], return a PartialOrder[A] using the `combine`
   * operator to determine the partial ordering. This method assumes
   * `combine` functions as `join` (that is, as an upper bound).
   *
   * This method returns:
   *
   *    0.0 if x = y
   *   -1.0 if y = combine(x, y)
   *    1.0 if x = combine(x, y)
   *    NaN otherwise
   */
  def asJoinPartialOrder(implicit ev: Eq[A]): PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A): Double =
        if (ev.eqv(x, y)) 0.0
        else {
          val z = self.combine(x, y)
          if (ev.eqv(y, z)) -1.0 else if (ev.eqv(x, z)) 1.0 else Double.NaN
        }
    }
}

abstract class SemilatticeFunctions[S[T] <: Semilattice[T]] extends SemigroupFunctions[S] {
  def asMeetPartialOrder[A](implicit s: S[A], ev: Eq[A]): PartialOrder[A] =
    s.asMeetPartialOrder(ev)
  def asJoinPartialOrder[A](implicit s: S[A], ev: Eq[A]): PartialOrder[A] =
    s.asJoinPartialOrder(ev)
}

object Semilattice extends SemilatticeFunctions[Semilattice] {

  /**
   * Access an implicit `Semilattice[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: Semilattice[A]): Semilattice[A] = ev
}

package cats

import simulacrum.typeclass
import scala.{ specialized => sp }

/**
 * A monoid is a semigroup with an identity. A monoid is a specialization of a
 * semigroup, so its operation must be associative. Additionally,
 * `combine(x, empty) == combine(empty, x) == x`. For example, if we have `Monoid[String]`,
 * with `combine` as string concatenation, then `empty = ""`.
 */
@typeclass trait Monoid[@sp(Int, Long, Float, Double) A] extends Semigroup[A] {

  /**
   * Return the identity element for this monoid.
   */
  def empty: A

  /**
   * Tests if `a` is the identity.
   */
  def isEmpty(a: A)(implicit ev: Eq[A]) = ev.eqv(a, empty)

  /**
   * Return `a` appended to itself `n` times.
   */
  override def combineN(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated combining for monoids must have n >= 0")
    else if (n == 0) empty
    else repeatedCombineN(a, n)

  /**
   * Given a sequence of `as`, sum them using the monoid and return the total.
   */
  def combineAll(as: TraversableOnce[A]): A =
    as.foldLeft(empty)(combine)
}

trait MonoidFunctions[M[T] <: Monoid[T]] extends SemigroupFunctions[M] {
  def empty[@sp(Int, Long, Float, Double) A](implicit ev: M[A]): A =
    ev.empty

  def combineAll[@sp(Int, Long, Float, Double) A](as: TraversableOnce[A])(implicit ev: M[A]): A =
    ev.combineAll(as)
}

object Monoid extends MonoidFunctions[Monoid]

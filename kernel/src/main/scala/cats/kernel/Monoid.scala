package cats.kernel

import scala.{specialized => sp}

/**
 * A monoid is a semigroup with an identity. A monoid is a specialization of a
 * semigroup, so its operation must be associative. Additionally,
 * `combine(x, empty) == combine(empty, x) == x`. For example, if we have `Monoid[String]`,
 * with `combine` as string concatenation, then `empty = ""`.
 */
trait Monoid[@sp(Int, Long, Float, Double) A] extends Any with Semigroup[A] {

  /**
   * Return the identity element for this monoid.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].empty
   * res0: String = ""
   *
   * scala> Monoid[Int].empty
   * res1: Int = 0
   * }}}
   */
  def empty: A

  /**
   * Tests if `a` is the identity.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].isEmpty("")
   * res0: Boolean = true
   *
   * scala> Monoid[String].isEmpty("something")
   * res1: Boolean = false
   * }}}
   */
  def isEmpty(a: A)(implicit ev: Eq[A]): Boolean =
    ev.eqv(a, empty)

  /**
   * Return `a` appended to itself `n` times.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].combineN("ha", 3)
   * res0: String = hahaha
   *
   * scala> Monoid[String].combineN("ha", 0)
   * res1: String = ""
   * }}}
   */
  override def combineN(a: A, n: Int): A =
    if (n < 0) throw new IllegalArgumentException("Repeated combining for monoids must have n >= 0")
    else if (n == 0) empty
    else repeatedCombineN(a, n)

  /**
   * Given a sequence of `as`, sum them using the monoid and return the total.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Monoid[String].combineAll(List("One ", "Two ", "Three"))
   * res0: String = One Two Three
   *
   * scala> Monoid[String].combineAll(List.empty)
   * res1: String = ""
   * }}}
   */
  def combineAll(as: TraversableOnce[A]): A =
    as.foldLeft(empty)(combine)

  override def combineAllOption(as: TraversableOnce[A]): Option[A] =
    if (as.isEmpty) None else Some(combineAll(as))
}

abstract class MonoidFunctions[M[T] <: Monoid[T]] extends SemigroupFunctions[M] {
  def empty[@sp(Int, Long, Float, Double) A](implicit ev: M[A]): A =
    ev.empty

  def isEmpty[@sp(Int, Long, Float, Double) A](a: A)(implicit m: M[A], ev: Eq[A]): Boolean =
    m.isEmpty(a)

  def combineAll[@sp(Int, Long, Float, Double) A](as: TraversableOnce[A])(implicit ev: M[A]): A =
    ev.combineAll(as)
}

object Monoid extends MonoidFunctions[Monoid] {

  /**
   * Access an implicit `Monoid[A]`.
   */
  @inline final def apply[A](implicit ev: Monoid[A]): Monoid[A] = ev
}

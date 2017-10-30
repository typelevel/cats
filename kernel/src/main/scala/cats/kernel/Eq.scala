package cats.kernel

import scala.{specialized => sp}

import scala.math.Equiv

/**
 * A type class used to determine equality between 2 instances of the same
 * type. Any 2 instances `x` and `y` are equal if `eqv(x, y)` is `true`.
 * Moreover, `eqv` should form an equivalence relation.
 */
trait Eq[@sp A] extends Any with Serializable { self =>

  /**
   * Returns `true` if `x` and `y` are equivalent, `false` otherwise.
   */
  def eqv(x: A, y: A): Boolean

  /**
   * Returns `false` if `x` and `y` are equivalent, `true` otherwise.
   */
  def neqv(x: A, y: A): Boolean = !eqv(x, y)

  /**
   * Constructs a new `Eq` instance for type `B` where 2 elements are
   * equivalent iff `eqv(f(x), f(y))`.
   */
  def on[@sp B](f: B => A): Eq[B] = Eq.by[B, A](f)(self)

  /**
   * Return an Eq that gives the result of the and of this and that
   * note this is idempotent
   */
  def and(that: Eq[A]): Eq[A] = Eq.and(self, that)

  /**
   * Return an Eq that gives the result of the or of this and that
   * Note this is idempotent
   */
  def or(that: Eq[A]): Eq[A] = Eq.or(self, that)
}

abstract class EqFunctions[E[T] <: Eq[T]] {

  def eqv[@sp A](x: A, y: A)(implicit ev: E[A]): Boolean =
    ev.eqv(x, y)

  def neqv[@sp A](x: A, y: A)(implicit ev: E[A]): Boolean =
    ev.neqv(x, y)

}

object Eq extends EqFunctions[Eq] {

  /**
   * Access an implicit `Eq[A]`.
   */
  @inline final def apply[A](implicit ev: Eq[A]): Eq[A] = ev

  /**
   * Convert an implicit `Eq[B]` to an `Eq[A]` using the given
   * function `f`.
   */
  def by[@sp A, @sp B](f: A => B)(implicit ev: Eq[B]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = ev.eqv(f(x), f(y))
    }

  /**
    * Return an Eq that gives the result of the and of eq1 and eq2
    * note this is idempotent
    */
  def and[@sp A](eq1: Eq[A], eq2: Eq[A]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = eq1.eqv(x, y) && eq2.eqv(x, y)
    }

  /**
    * Return an Eq that gives the result of the or of this and that
    * Note this is idempotent
    */
  def or[@sp A](eq1: Eq[A], eq2: Eq[A]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = eq1.eqv(x, y) || eq2.eqv(x, y)
    }

  /**
   * This gives compatibility with scala's Equiv trait
   */
  implicit def catsKernelEquivForEq[A](implicit ev: Eq[A]): Equiv[A] =
    new Equiv[A] {
      def equiv(a: A, b: A) = ev.eqv(a, b)
    }

  /**
   * Create an `Eq` instance from an `eqv` implementation.
   */
  def instance[A](f: (A, A) => Boolean): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = f(x, y)
    }

  /**
   * An `Eq[A]` that delegates to universal equality (`==`).
   *
   * This can be useful for case classes, which have reasonable `equals`
   * implementations
   */
  def fromUniversalEquals[A]: Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = x == y
    }

  /**
   * Everything is the same
   */
  def allEqual[A]: Eq[A] = new Eq[A] {
    def eqv(x: A, y: A) = true
  }

  /**
   * This is a monoid that creates an Eq that
   * checks that all equality checks pass
   */
  def allEqualBoundedSemilattice[A]: BoundedSemilattice[Eq[A]] = new BoundedSemilattice[Eq[A]] {
    def empty = allEqual[A]
    def combine(e1: Eq[A], e2: Eq[A]): Eq[A] = e1.and(e2)
    override def combineAllOption(es: TraversableOnce[Eq[A]]): Option[Eq[A]] =
      if (es.isEmpty) None
      else {
        val materialized = es.toVector
        Some(new Eq[A] {
          def eqv(x: A, y: A) = materialized.forall(_.eqv(x, y))
        })
      }
  }

  /**
   * This is a monoid that creates an Eq that
   * checks that at least one equality check passes
   */
  def anyEqualSemilattice[A]: Semilattice[Eq[A]] = new Semilattice[Eq[A]] {
    def combine(e1: Eq[A], e2: Eq[A]): Eq[A] = e1.or(e2)
    override def combineAllOption(es: TraversableOnce[Eq[A]]): Option[Eq[A]] =
      if (es.isEmpty) None
      else {
        val materialized = es.toVector
        Some(new Eq[A] {
          def eqv(x: A, y: A) = materialized.exists(_.eqv(x, y))
        })
      }
  }
}

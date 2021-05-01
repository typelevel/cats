package algebra
package lattice

import ring.BoolRng
import scala.{specialized => sp}

/**
 * Generalized Boolean algebra, that is, a Boolean algebra without
 * the top element. Generalized Boolean algebras do not (in general)
 * have (absolute) complements, but they have ''relative complements''
 * (see [[GenBool.without]]).
 */
trait GenBool[@sp(Int, Long) A] extends Any with DistributiveLattice[A] with BoundedJoinSemilattice[A] { self =>
  def and(a: A, b: A): A
  override def meet(a: A, b: A): A = and(a, b)

  def or(a: A, b: A): A
  override def join(a: A, b: A): A = or(a, b)

  /**
   * The operation of ''relative complement'', symbolically often denoted
   * `a\b` (the symbol for set-theoretic difference, which is the
   * meaning of relative complement in the lattice of sets).
   */
  def without(a: A, b: A): A

  /**
   * Logical exclusive or, set-theoretic symmetric difference.
   * Defined as `a\b ∨ b\a`.
   */
  def xor(a: A, b: A): A = or(without(a, b), without(b, a))

  /**
   * Every generalized Boolean algebra is also a `BoolRng`, with
   * multiplication defined as `and` and addition defined as `xor`.
   */
  def asBoolRing: BoolRng[A] = new BoolRngFromGenBool(self)
}

/**
 * Every Boolean rng gives rise to a Boolean algebra without top:
 *  - 0 is preserved;
 *  - ring multiplication (`times`) corresponds to `and`;
 *  - ring addition (`plus`) corresponds to `xor`;
 *  - `a or b` is then defined as `a xor b xor (a and b)`;
 *  - relative complement `a\b` is defined as `a xor (a and b)`.
 *
 * `BoolRng.asBool.asBoolRing` gives back the original `BoolRng`.
 *
 * @see [[algebra.lattice.GenBool.asBoolRing]]
 */
class GenBoolFromBoolRng[A](orig: BoolRng[A]) extends GenBool[A] {
  def zero: A = orig.zero
  def and(a: A, b: A): A = orig.times(a, b)
  def or(a: A, b: A): A = orig.plus(orig.plus(a, b), orig.times(a, b))
  def without(a: A, b: A): A = orig.plus(a, orig.times(a, b))
  override def asBoolRing: BoolRng[A] = orig
}

private[lattice] class BoolRngFromGenBool[@sp(Int, Long) A](orig: GenBool[A]) extends BoolRng[A] {
  def zero: A = orig.zero
  def plus(x: A, y: A): A = orig.xor(x, y)
  def times(x: A, y: A): A = orig.and(x, y)
}

trait GenBoolFunctions[G[A] <: GenBool[A]] extends BoundedJoinSemilatticeFunctions[G] with MeetSemilatticeFunctions[G] {
  def and[@sp(Int, Long) A](x: A, y: A)(implicit ev: G[A]): A = ev.and(x, y)
  def or[@sp(Int, Long) A](x: A, y: A)(implicit ev: G[A]): A = ev.or(x, y)
  def without[@sp(Int, Long) A](x: A, y: A)(implicit ev: G[A]): A = ev.without(x, y)
  def xor[@sp(Int, Long) A](x: A, y: A)(implicit ev: G[A]): A = ev.xor(x, y)
}

object GenBool extends GenBoolFunctions[GenBool] {
  @inline final def apply[@sp(Int, Long) A](implicit ev: GenBool[A]): GenBool[A] = ev
}

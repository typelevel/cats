package algebra
package lattice

import ring.BoolRing
import scala.{specialized => sp}

/**
 * Boolean algebras are Heyting algebras with the additional
 * constraint that the law of the excluded middle is true
 * (equivalently, double-negation is true).
 *
 * This means that in addition to the laws Heyting algebras obey,
 * boolean algebras also obey the following:
 *
 *  - (a ∨ ¬a) = 1
 *  - ¬¬a = a
 *
 * Boolean algebras generalize classical logic: one is equivalent to
 * "true" and zero is equivalent to "false". Boolean algebras provide
 * additional logical operators such as `xor`, `nand`, `nor`, and
 * `nxor` which are commonly used.
 *
 * Every boolean algebras has a dual algebra, which involves reversing
 * true/false as well as and/or.
 */
trait Bool[@sp(Int, Long) A] extends Any with Heyting[A] with GenBool[A] { self =>
  def imp(a: A, b: A): A = or(complement(a), b)
  def without(a: A, b: A): A = and(a, complement(b))

  // xor is already defined in both Heyting and GenBool.
  // In Bool, the definitions coincide, so we just use one of them.
  override def xor(a: A, b: A): A =
    or(without(a, b), without(b, a))

  override def dual: Bool[A] = new DualBool(this)

  /**
   * Every Boolean algebra is a BoolRing, with multiplication defined as
   * `and` and addition defined as `xor`. Bool does not extend BoolRing
   * because, e.g. we might want a Bool[Int] and CommutativeRing[Int] to
   * refer to different structures, by default.
   *
   * Note that the ring returned by this method is not an extension of
   * the `Rig` returned from `BoundedDistributiveLattice.asCommutativeRig`.
   */
  override def asBoolRing: BoolRing[A] = new BoolRingFromBool(self)
}

class DualBool[@sp(Int, Long) A](orig: Bool[A]) extends Bool[A] {
  def one: A = orig.zero
  def zero: A = orig.one
  def and(a: A, b: A): A = orig.or(a, b)
  def or(a: A, b: A): A = orig.and(a, b)
  def complement(a: A): A = orig.complement(a)
  override def xor(a: A, b: A): A = orig.complement(orig.xor(a, b))

  override def imp(a: A, b: A): A = orig.and(orig.complement(a), b)
  override def nand(a: A, b: A): A = orig.nor(a, b)
  override def nor(a: A, b: A): A = orig.nand(a, b)
  override def nxor(a: A, b: A): A = orig.xor(a, b)

  override def dual: Bool[A] = orig
}

private[lattice] class BoolRingFromBool[A](orig: Bool[A]) extends BoolRngFromGenBool(orig) with BoolRing[A] {
  def one: A = orig.one
}

/**
 * Every Boolean ring gives rise to a Boolean algebra:
 *  - 0 and 1 are preserved;
 *  - ring multiplication (`times`) corresponds to `and`;
 *  - ring addition (`plus`) corresponds to `xor`;
 *  - `a or b` is then defined as `a xor b xor (a and b)`;
 *  - complement (`¬a`) is defined as `a xor 1`.
 */
class BoolFromBoolRing[A](orig: BoolRing[A]) extends GenBoolFromBoolRng(orig) with Bool[A] {
  def one: A = orig.one
  def complement(a: A): A = orig.plus(orig.one, a)
  override def without(a: A, b: A): A = super[GenBoolFromBoolRng].without(a, b)
  override def asBoolRing: BoolRing[A] = orig

  override def meet(a: A, b: A): A = super[GenBoolFromBoolRng].meet(a, b)
  override def join(a: A, b: A): A = super[GenBoolFromBoolRng].join(a, b)
}

object Bool extends HeytingFunctions[Bool] with GenBoolFunctions[Bool] {

  /**
   * Access an implicit `Bool[A]`.
   */
  @inline final def apply[@sp(Int, Long) A](implicit ev: Bool[A]): Bool[A] = ev
}

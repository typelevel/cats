package cats
package algebra
package instances

import cats.algebra.lattice.GenBool
import cats.algebra.ring.{BoolRng, Semiring}

package object set extends SetInstances

trait SetInstances extends cats.kernel.instances.SetInstances {

  implicit def setLattice[A]: GenBool[Set[A]] = new SetLattice[A]
  implicit def setSemiring[A]: Semiring[Set[A]] = new SetSemiring[A]

  // this instance is not compatible with setSemiring, so it is not
  // marked as implicit to avoid an ambiguity.
  def setBoolRng[A]: BoolRng[Set[A]] = new SetBoolRng[A]
}

class SetLattice[A] extends GenBool[Set[A]] {
  def zero: Set[A] = Set.empty
  def or(lhs: Set[A], rhs: Set[A]): Set[A] = lhs | rhs
  def and(lhs: Set[A], rhs: Set[A]): Set[A] = lhs & rhs
  def without(lhs: Set[A], rhs: Set[A]): Set[A] = lhs -- rhs
}

class SetSemiring[A] extends Semiring[Set[A]] {
  def zero: Set[A] = Set.empty
  def plus(x: Set[A], y: Set[A]): Set[A] = x | y
  def times(x: Set[A], y: Set[A]): Set[A] = x & y
}

class SetBoolRng[A] extends BoolRng[Set[A]] {
  def zero: Set[A] = Set.empty
  def plus(x: Set[A], y: Set[A]): Set[A] = (x -- y) | (y -- x) // this is xor
  def times(x: Set[A], y: Set[A]): Set[A] = x & y
}

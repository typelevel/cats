package cats.kernel
package instances

package object set extends SetInstances

trait SetInstances {
  implicit def catsKernelStdPartialOrderForSet[A]: PartialOrder[Set[A]] =
    new SetPartialOrder[A]

  implicit def catsKernelStdSemilatticeForSet[A]: BoundedSemilattice[Set[A]] =
    new SetSemilattice[A]
}

class SetPartialOrder[A] extends PartialOrder[Set[A]] {
  def partialCompare(x: Set[A], y: Set[A]): Double =
    if (x eq y) 0.0
    else if (x.size < y.size) if (x.subsetOf(y)) -1.0 else Double.NaN
    else if (y.size < x.size) if (y.subsetOf(x)) 1.0 else Double.NaN
    else if (x == y) 0.0
    else Double.NaN

  override def eqv(x: Set[A], y: Set[A]): Boolean =
    x == y
}

class SetSemilattice[A] extends BoundedSemilattice[Set[A]] {
  def empty: Set[A] = Set.empty
  def combine(x: Set[A], y: Set[A]): Set[A] = x | y
}

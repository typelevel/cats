package cats.kernel
package instances

trait SetInstances extends SetInstances1 {
  implicit def catsKernelStdHashForSet[A]: Hash[Set[A]] =
    new SetHash[A]
  implicit def catsKernelStdLowerBoundedForSet[A]: LowerBounded[Set[A]] =
    new SetLowerBounded[A] {
      override val partialOrder: PartialOrder[Set[A]] = catsKernelStdPartialOrderForSet
    }
}

trait SetInstances1 {
  implicit def catsKernelStdPartialOrderForSet[A]: PartialOrder[Set[A]] =
    new SetPartialOrder[A]

  implicit def catsKernelStdSemilatticeForSet[A]: BoundedSemilattice[Set[A]] =
    new SetSemilattice[A]
}

trait SetLowerBounded[A] extends LowerBounded[Set[A]] {
  override def minBound: Set[A] = Set.empty
}

class SetPartialOrder[A] extends PartialOrder[Set[A]] {
  def partialCompare(x: Set[A], y: Set[A]): Double =
    if (x eq y) 0.0
    else if (x.size < y.size) if (x.subsetOf(y)) -1.0 else Double.NaN
    else if (y.size < x.size) if (y.subsetOf(x)) 1.0 else Double.NaN
    else if (x == y) 0.0
    else Double.NaN

  // Does not require an Eq on elements: Scala sets must use the universal `equals`.
  override def eqv(x: Set[A], y: Set[A]): Boolean = x == y
}

class SetHash[A] extends Hash[Set[A]] {
  // Does not require a Hash on elements: Scala sets must use the universal `hashCode`.
  def hash(x: Set[A]): Int = x.hashCode()

  // Does not require an Eq on elements: Scala sets must use the universal `equals`.
  def eqv(x: Set[A], y: Set[A]): Boolean = x == y
}

class SetSemilattice[A] extends BoundedSemilattice[Set[A]] {
  def empty: Set[A] = Set.empty
  def combine(x: Set[A], y: Set[A]): Set[A] = x | y
}

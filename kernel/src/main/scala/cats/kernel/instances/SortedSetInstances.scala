package cats.kernel
package instances

import cats.kernel.OrderingFromOrder
import cats.kernel.OrderFromOrdering

import scala.collection.immutable.SortedSet

trait SortedSetInstances extends SortedSetInstances1 {
  @deprecated("Use catsKernelStdHashForSortedSet override without Order", "2.1.0")
  def catsKernelStdHashForSortedSet[A: Order: Hash]: Hash[SortedSet[A]] =
    new SortedSetHash[A]

  implicit def catsKernelStdHashForSortedSet[A: Hash]: Hash[SortedSet[A]] =
    new SortedSetHash[A]
}

private[instances] trait SortedSetInstances1 {
  implicit def catsKernelStdOrderForSortedSet[A: Order]: Order[SortedSet[A]] =
    new SortedSetOrder[A]

  implicit def catsKernelStdBoundedSemilatticeForSortedSet[A: Order]: BoundedSemilattice[SortedSet[A]] =
    new SortedSetSemilattice[A]
}

class SortedSetOrder[A: Order] extends Order[SortedSet[A]] {

  /** Attempt to verify that the Ordering instances and the Order instance all
    * come from the same canonical source.
    */
  private def consistentOrdering(
    a1: Ordering[A],
    a2: Ordering[A]
  ): Boolean =
    (a1, a2, Order[A]) match {
      case (a1: OrderingFromOrder[A], a2: OrderingFromOrder[A], order) =>
        a1.value == order && a1.value == order
      case (a1, a2, order: OrderFromOrdering[A]) =>
        a1 == order.value && a2 == order.value
      case _ =>
        false
    }

  def compare(a1: SortedSet[A], a2: SortedSet[A]): Int =
    cats.kernel.instances.int.catsKernelStdOrderForInt.compare(a1.size, a2.size) match {
      case 0 =>
        // In the event that the ordering instances are not _exactly_ the
        // same, we have to sort/rebuild the sets. If we don't we risk
        // violating the Monoid identity law.
        if (consistentOrdering(a1.ordering, a2.ordering)) {
          // Hopefully this is the branch we hit the vast majority of the
          // time.
          StaticMethods.iteratorCompare(a1.iterator, a2.iterator)
        } else {
          val ordering: Ordering[A] = Order[A].toOrdering
          StaticMethods.iteratorCompare(a1.toSeq.sorted(ordering).iterator, a2.toSeq.sorted(ordering).iterator)
        }
      case x => x
    }

  // Could be removed, but MiMa complains
  override def eqv(a1: SortedSet[A], a2: SortedSet[A]): Boolean =
    super.eqv(a1, a2)
}

// FIXME use context bound in 3.x
class SortedSetHash[A](implicit hashA: Hash[A]) extends Hash[SortedSet[A]] {
  import scala.util.hashing.MurmurHash3._

  @deprecated("Use the constructor _without_ Order instead, since Order is not required", "2.1.0")
  private[instances] def this(o: Order[A], h: Hash[A]) = this()(h)

  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  def hash(xs: SortedSet[A]): Int = {
    var a, b, n = 0
    var c = 1
    xs.foreach { x =>
      val h = Hash[A].hash(x)
      a += h
      b ^= h
      c = cats.kernel.instances.StaticMethods.updateUnorderedHashC(c, h)
      n += 1
    }
    var h = setSeed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }
  override def eqv(s1: SortedSet[A], s2: SortedSet[A]): Boolean =
    StaticMethods.iteratorEq(s1.iterator, s2.iterator)(Eq[A])
}

class SortedSetSemilattice[A: Order] extends BoundedSemilattice[SortedSet[A]] {
  def empty: SortedSet[A] = SortedSet.empty(implicitly[Order[A]].toOrdering)
  def combine(x: SortedSet[A], y: SortedSet[A]): SortedSet[A] = x | y
}

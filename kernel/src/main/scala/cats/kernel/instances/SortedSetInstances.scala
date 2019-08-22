package cats.kernel
package instances

import cats.kernel.{BoundedSemilattice, Hash, Order}
import scala.collection.immutable.SortedSet

trait SortedSetInstances extends SortedSetInstances1 {
  implicit def catsKernelStdHashForSortedSet[A: Order: Hash]: Hash[SortedSet[A]] =
    new SortedSetHash[A]
}

trait SortedSetInstances1 {
  implicit def catsKernelStdOrderForSortedSet[A: Order]: Order[SortedSet[A]] =
    new SortedSetOrder[A]

  implicit def catsKernelStdBoundedSemilatticeForSortedSet[A: Order]: BoundedSemilattice[SortedSet[A]] =
    new SortedSetSemilattice[A]
}

class SortedSetOrder[A: Order] extends Order[SortedSet[A]] {
  def compare(a1: SortedSet[A], a2: SortedSet[A]): Int =
    cats.kernel.instances.int.catsKernelStdOrderForInt.compare(a1.size, a2.size) match {
      case 0 => StaticMethods.iteratorCompare(a1.iterator, a2.iterator)
      case x => x
    }

  override def eqv(s1: SortedSet[A], s2: SortedSet[A]): Boolean =
    StaticMethods.iteratorEq(s1.iterator, s2.iterator)
}

class SortedSetHash[A: Order: Hash] extends Hash[SortedSet[A]] {
  import scala.util.hashing.MurmurHash3._

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
    StaticMethods.iteratorEq(s1.iterator, s2.iterator)(Order[A])
}

class SortedSetSemilattice[A: Order] extends BoundedSemilattice[SortedSet[A]] {
  def empty: SortedSet[A] = SortedSet.empty(implicitly[Order[A]].toOrdering)
  def combine(x: SortedSet[A], y: SortedSet[A]): SortedSet[A] = x | y
}

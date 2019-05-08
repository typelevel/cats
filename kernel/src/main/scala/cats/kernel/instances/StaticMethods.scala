package cats
package kernel
package instances

import scala.collection.mutable

object StaticMethods extends cats.kernel.compat.HashCompat {

  def wrapMutableMap[K, V](m: mutable.Map[K, V]): Map[K, V] =
    new WrappedMutableMap(m)

  private[kernel] class WrappedMutableMap[K, V](m: mutable.Map[K, V])
      extends kernel.compat.WrappedMutableMapBase[K, V](m) {
    override def size: Int = m.size
    def get(k: K): Option[V] = m.get(k)
    def iterator: Iterator[(K, V)] = m.iterator
  }

  // scalastyle:off return
  def iteratorCompare[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Order[A]): Int = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          val x = xs.next
          val y = ys.next
          val cmp = ev.compare(x, y)
          if (cmp != 0) return cmp
        } else {
          return 1
        }
      } else {
        return if (ys.hasNext) -1 else 0
      }
    }
    0
  }

  def iteratorPartialCompare[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: PartialOrder[A]): Double = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          val x = xs.next
          val y = ys.next
          val cmp = ev.partialCompare(x, y)
          if (cmp != 0.0) return cmp
        } else {
          return 1.0
        }
      } else {
        return if (ys.hasNext) -1.0 else 0.0
      }
    }
    0.0
  }

  def iteratorEq[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Eq[A]): Boolean = {
    while (true) {
      if (xs.hasNext) {
        if (ys.hasNext) {
          if (ev.neqv(xs.next, ys.next)) return false
        } else {
          return false
        }
      } else {
        return !ys.hasNext
      }
    }
    true
  }
  // scalastyle:on return

  def combineNIterable[A, R](b: mutable.Builder[A, R], x: Iterable[A], n: Int): R = {
    var i = n
    while (i > 0) { b ++= x; i -= 1 }
    b.result
  }

  def combineAllIterable[A, R](b: mutable.Builder[A, R], xs: TraversableOnce[Iterable[A]]): R = {
    xs.foreach(b ++= _)
    b.result
  }

  // Adapted from scala.util.hashing.MurmurHash#productHash.
  def product1Hash(_1Hash: Int): Int = {
    import scala.util.hashing.MurmurHash3._
    var h = productSeed
    h = mix(h, _1Hash)
    finalizeHash(h, 1)
  }

  // Adapted from scala.util.hashing.MurmurHash#productHash.
  def product2Hash(_1Hash: Int, _2Hash: Int): Int = {
    import scala.util.hashing.MurmurHash3._
    var h = productSeed
    h = mix(h, _1Hash)
    h = mix(h, _2Hash)
    finalizeHash(h, 2)
  }
}

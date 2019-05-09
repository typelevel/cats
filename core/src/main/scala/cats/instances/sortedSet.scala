package cats
package instances

import cats.kernel.{BoundedSemilattice, Hash, Order}
import scala.collection.immutable.SortedSet
import scala.annotation.tailrec
import cats.implicits._

trait SortedSetInstances extends SortedSetInstances1 {

  implicit val catsStdInstancesForSortedSet: Foldable[SortedSet] with SemigroupK[SortedSet] =
    new Foldable[SortedSet] with SemigroupK[SortedSet] {

      def combineK[A](x: SortedSet[A], y: SortedSet[A]): SortedSet[A] = x | y

      def foldLeft[A, B](fa: SortedSet[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: SortedSet[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def foldMap[A, B](fa: SortedSet[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      override def get[A](fa: SortedSet[A])(idx: Long): Option[A] = {
        @tailrec
        def go(idx: Int, it: Iterator[A]): Option[A] =
          if (it.hasNext) {
            if (idx == 0) Some(it.next)
            else {
              it.next
              go(idx - 1, it)
            }
          } else None
        if (idx < Int.MaxValue && idx >= 0L) go(idx.toInt, fa.toIterator) else None
      }

      override def size[A](fa: SortedSet[A]): Long = fa.size.toLong

      override def exists[A](fa: SortedSet[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: SortedSet[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: SortedSet[A]): Boolean = fa.isEmpty

      override def fold[A](fa: SortedSet[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: SortedSet[A]): List[A] = fa.toList

      override def reduceLeftOption[A](fa: SortedSet[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: SortedSet[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def collectFirst[A, B](fa: SortedSet[A])(pf: PartialFunction[A, B]): Option[B] =
        fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: SortedSet[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))
    }

  implicit def catsStdShowForSortedSet[A: Show]: Show[SortedSet[A]] = new Show[SortedSet[A]] {
    def show(fa: SortedSet[A]): String =
      fa.toIterator.map(_.show).mkString("SortedSet(", ", ", ")")
  }

  implicit def catsKernelStdOrderForSortedSet[A: Order]: Order[SortedSet[A]] =
    new SortedSetOrder[A]
}

trait SortedSetInstances1 {
  implicit def catsKernelStdHashForSortedSet[A: Order: Hash]: Hash[SortedSet[A]] =
    new SortedSetHash[A]

  implicit def catsKernelStdSemilatticeForSortedSet[A: Order]: BoundedSemilattice[SortedSet[A]] =
    new SortedSetSemilattice[A]
}

class SortedSetOrder[A: Order] extends Order[SortedSet[A]] {
  def compare(a1: SortedSet[A], a2: SortedSet[A]): Int =
    Order[Int].compare(a1.size, a2.size) match {
      case 0 => Order.compare(a1.toStream, a2.toStream)
      case x => x
    }

  override def eqv(s1: SortedSet[A], s2: SortedSet[A]): Boolean = {
    implicit val x = Order[A].toOrdering
    s1.toStream.corresponds(s2.toStream)(Order[A].eqv)
  }
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
  override def eqv(s1: SortedSet[A], s2: SortedSet[A]): Boolean = {
    implicit val x = Order[A].toOrdering
    s1.toStream.corresponds(s2.toStream)(Order[A].eqv)
  }
}

class SortedSetSemilattice[A: Order] extends BoundedSemilattice[SortedSet[A]] {
  def empty: SortedSet[A] = SortedSet.empty(implicitly[Order[A]].toOrdering)
  def combine(x: SortedSet[A], y: SortedSet[A]): SortedSet[A] = x | y
}

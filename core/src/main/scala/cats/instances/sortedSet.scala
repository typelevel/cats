package cats
package instances

import cats.kernel.{BoundedSemilattice, Hash, PartialOrder}
import scala.collection.immutable.SortedSet
import scala.annotation.tailrec
import cats.syntax.show._

trait SortedSetInstances extends SortedSetInstances1 {

  implicit val catsStdInstancesForSortedSet: Foldable[SortedSet] with SemigroupK[SortedSet] =
    new Foldable[SortedSet] with SemigroupK[SortedSet] {

      def combineK[A](x: SortedSet[A], y: SortedSet[A]): SortedSet[A] = x | y

      def foldLeft[A, B](fa: SortedSet[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: SortedSet[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.iterator, lb)(f)

      override def get[A](fa: SortedSet[A])(idx: Long): Option[A] = {
        @tailrec
        def go(idx: Int, it: Iterator[A]): Option[A] = {
          if (it.hasNext) {
            if (idx == 0) Some(it.next) else {
              it.next
              go(idx - 1, it)
            }
          } else None
        }
        if (idx < Int.MaxValue && idx >= 0L)  go(idx.toInt, fa.toIterator) else None
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
    }

  implicit def catsStdShowForSortedSet[A:Show]: Show[SortedSet[A]] = new Show[SortedSet[A]] {
    def show(fa: SortedSet[A]): String =
      fa.toIterator.map(_.show).mkString("SortedSet(", ", ", ")")
  }

  implicit def catsKernelStdHashForSortedSet[A]: Hash[SortedSet[A]] =
    new SortedSetHash[A]
}

trait SortedSetInstances1 {
  implicit def catsKernelStdPartialOrderForSortedSet[A]: PartialOrder[SortedSet[A]] =
    new SortedSetPartialOrder[A]

  implicit def catsKernelStdSemilatticeForSortedSet[A: Order]: BoundedSemilattice[SortedSet[A]] =
    new SortedSetSemilattice[A]
}

class SortedSetPartialOrder[A] extends PartialOrder[SortedSet[A]] {
  def partialCompare(x: SortedSet[A], y: SortedSet[A]): Double =
    if (x eq y) 0.0
    else if (x.size < y.size) if (x.subsetOf(y)) -1.0 else Double.NaN
    else if (y.size < x.size) if (y.subsetOf(x)) 1.0 else Double.NaN
    else if (x == y) 0.0
    else Double.NaN

  // Does not require an Eq on elements: Scala sets must use the universal `equals`.
  override def eqv(x: SortedSet[A], y: SortedSet[A]): Boolean = x == y
}

class SortedSetHash[A] extends Hash[SortedSet[A]] {
  // Does not require a Hash on elements: Scala sets must use the universal `hashCode`.
  def hash(x: SortedSet[A]): Int = x.hashCode()

  // Does not require an Eq on elements: Scala sets must use the universal `equals`.
  def eqv(x: SortedSet[A], y: SortedSet[A]): Boolean = x == y
}

class SortedSetSemilattice[A: Order] extends BoundedSemilattice[SortedSet[A]] {
  def empty: SortedSet[A] = SortedSet.empty(implicitly[Order[A]].toOrdering)
  def combine(x: SortedSet[A], y: SortedSet[A]): SortedSet[A] = x | y
}


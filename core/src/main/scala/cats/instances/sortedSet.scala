package cats
package instances

import cats.kernel.{BoundedSemilattice, Hash, Order}
import scala.collection.immutable.SortedSet
import scala.annotation.tailrec

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
        if (idx < Int.MaxValue && idx >= 0L) go(idx.toInt, fa.iterator) else None
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
      fa.iterator.map(Show[A].show).mkString("SortedSet(", ", ", ")")
  }

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.sortedSet.catsKernelStdOrderForSortedSet")
  private[instances] def catsKernelStdOrderForSortedSet[A: Order]: Order[SortedSet[A]] =
    cats.kernel.instances.sortedSet.catsKernelStdOrderForSortedSet[A]
}

private[instances] trait SortedSetInstances1 {
  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.sortedSet.catsKernelStdHashForSortedSet")
  private[instances] def catsKernelStdHashForSortedSet[A: Order: Hash]: Hash[SortedSet[A]] =
    cats.kernel.instances.sortedSet.catsKernelStdHashForSortedSet1[A]

  @deprecated("2.0.0-RC2", "Use cats.kernel.instances.sortedSet.catsKernelStdSemilatticeForSortedSet")
  def catsKernelStdSemilatticeForSortedSet[A: Order]: BoundedSemilattice[SortedSet[A]] =
    cats.kernel.instances.sortedSet.catsKernelStdBoundedSemilatticeForSortedSet[A]
}

private[instances] trait SortedSetInstancesBinCompat0 {
  implicit val catsStdSemigroupalForSortedSet: Semigroupal[SortedSet] = new Semigroupal[SortedSet] {
    override def product[A, B](fa: SortedSet[A], fb: SortedSet[B]): SortedSet[(A, B)] = {
      implicit val orderingA = fa.ordering
      implicit val orderingB = fb.ordering

      fa.flatMap(a => fb.map(b => a -> b))
    }
  }
}

private[instances] trait SortedSetInstancesBinCompat1 extends LowPrioritySortedSetInstancesBinCompat1 {
  // TODO: Remove when this is no longer necessary for binary compatibility.
  implicit override def catsKernelStdHashForSortedSet[A: Order: Hash]: Hash[SortedSet[A]] =
    cats.kernel.instances.sortedSet.catsKernelStdHashForSortedSet1[A]
}

private[instances] trait LowPrioritySortedSetInstancesBinCompat1
    extends cats.kernel.instances.SortedSetInstances
    with SortedSetInstances {
  implicit override def catsKernelStdOrderForSortedSet[A: Order]: Order[SortedSet[A]] =
    cats.kernel.instances.sortedSet.catsKernelStdOrderForSortedSet[A]

  override def catsKernelStdHashForSortedSet[A: Order: Hash]: Hash[SortedSet[A]] =
    cats.kernel.instances.sortedSet.catsKernelStdHashForSortedSet1[A]
}

@deprecated("2.0.0-RC2", "Use cats.kernel.instances.SortedSetHash")
class SortedSetHash[A: Order: Hash] extends cats.kernel.instances.SortedSetHash[A]

@deprecated("2.0.0-RC2", "Use cats.kernel.instances.SortedSetOrder")
class SortedSetOrder[A: Order] extends cats.kernel.instances.SortedSetOrder[A]

@deprecated("2.0.0-RC2", "Use cats.kernel.instances.SortedSetSemilattice")
class SortedSetSemilattice[A: Order] extends cats.kernel.instances.SortedSetSemilattice[A]

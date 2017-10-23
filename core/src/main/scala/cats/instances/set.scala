package cats
package instances

import scala.annotation.tailrec

import cats.syntax.show._

trait SetInstances extends cats.kernel.instances.SetInstances {

  implicit val catsStdInstancesForSet: Foldable[Set] with MonoidK[Set] =
    new Foldable[Set] with MonoidK[Set] {

      def empty[A]: Set[A] = Set.empty[A]

      def combineK[A](x: Set[A], y: Set[A]): Set[A] = x | y

      def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Set[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def get[A](fa: Set[A])(idx: Long): Option[A] = {
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

      override def size[A](fa: Set[A]): Long = fa.size.toLong

      override def exists[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Set[A]): Boolean = fa.isEmpty

      override def fold[A](fa: Set[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: Set[A]): List[A] = fa.toList

      override def reduceLeftOption[A](fa: Set[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: Set[A])(f: A => Boolean): Option[A] = fa.find(f)
    }

  implicit def catsStdShowForSet[A:Show]: Show[Set[A]] = new Show[Set[A]] {
    def show(fa: Set[A]): String =
      fa.toIterator.map(_.show).mkString("Set(", ", ", ")")
  }
}

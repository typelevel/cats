package cats
package instances

import cats.syntax.show._
import scala.annotation.tailrec

trait SetInstances extends cats.kernel.instances.SetInstances {

  implicit val catsStdInstancesForSet: Foldable[Set] with Unfoldable[Set] with MonoidK[Set] =
    new Foldable[Set] with Unfoldable[Set] with MonoidK[Set] {

      def empty[A]: Set[A] = Set.empty[A]

      def combineK[A](x: Set[A], y: Set[A]): Set[A] = x | y

      def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Set[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.iterator, lb)(f)

      override def size[A](fa: Set[A]): Long = fa.size.toLong

      override def exists[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Set[A]): Boolean = fa.isEmpty

      override def foldM[G[_], A, B](fa: Set[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] =
        Foldable.iteratorFoldM(fa.toIterator, z)(f)

      def unfoldLeft[A, B](seed: B)(f: B => Option[(B, A)]): Set[A] = {
        @tailrec def loop(seed: B)(xs: Set[A]): Set[A] = f(seed) match {
          case None         => xs
          case Some((b, a)) => loop(b)(xs + a)
        }

        loop(seed)(Set.empty)
      }

      override def none[A]: Set[A] = Set.empty
      override def singleton[A](value: A): Set[A] = Set(value)
      override def replicate[A](n: Int)(value: A): Set[A] = if (n > 0) Set(value) else Set.empty
      override def build[A](as: A*): Set[A] = as.toSet
    }

  implicit def catsStdShowForSet[A:Show]: Show[Set[A]] = new Show[Set[A]] {
    def show(fa: Set[A]): String =
      fa.toIterator.map(_.show).mkString("Set(", ", ", ")")
  }
}

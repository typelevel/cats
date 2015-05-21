package cats
package std

import scala.annotation.tailrec

trait SetInstances extends algebra.std.SetInstances {
  implicit val setInstance: Foldable[Set] with MonoidK[Set] =
    new Foldable[Set] with MonoidK[Set] {

      def empty[A]: Set[A] = Set.empty[A]

      def combine[A](x: Set[A], y: Set[A]): Set[A] = x | y

      def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def partialFold[A, B](fa: Set[A])(f: A => Fold[B]): Fold[B] =
        Fold.partialIterate(fa)(f)
    }
}

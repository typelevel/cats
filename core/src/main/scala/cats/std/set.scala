package cats
package std

import scala.annotation.tailrec

trait SetInstances {
  implicit val setInstance =
    new Foldable[Set] with arrow.MonoidK[Set] {

      def empty[A]: Set[A] = Set.empty[A]

      def combine[A](x: Set[A], y: Set[A]): Set[A] = x | y

      def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)
      
      def foldRight[A, B](fa: Set[A], b: B)(f: (A, B) => B): B =
        fa.foldRight(b)(f)
    }
}

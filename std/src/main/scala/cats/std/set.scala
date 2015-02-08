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

      def foldRight[A, B](fa: Set[A], b: B)(f: (A, B) => B): B =
        fa.foldRight(b)(f)

      def foldRight[A, B](fa: Set[A], b: Lazy[B])(f: (A, Lazy[B]) => B): Lazy[B] = {
        val it = fa.iterator
        def loop(b: Lazy[B]): Lazy[B] =
          if (it.hasNext) Lazy.byName(f(it.next, b)) else b
        Lazy(loop(b).force)
      }
    }
}

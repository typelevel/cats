package cats
package instances

import cats.kernel.CommutativeMonoid
import cats.syntax.show._

trait SetInstances extends cats.kernel.instances.SetInstances {

  implicit val catsStdInstancesForSet: MonoidK[Set] with UnorderedFoldable[Set] =
    new MonoidK[Set] with UnorderedFoldable[Set] {

      def empty[A]: Set[A] = Set.empty[A]

      def combineK[A](x: Set[A], y: Set[A]): Set[A] = x | y

      def foldMapUnordered[A, B](fa: Set[A])(f: A => B)(implicit B: CommutativeMonoid[B]): B =
        fa.foldLeft(B.empty)((b, a) => B.combine(b, f(a)))
    }

  implicit def catsStdShowForSet[A:Show]: Show[Set[A]] = new Show[Set[A]] {
    def show(fa: Set[A]): String =
      fa.toIterator.map(_.show).mkString("Set(", ", ", ")")
  }
}

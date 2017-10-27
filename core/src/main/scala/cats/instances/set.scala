package cats
package instances

import cats.syntax.show._

trait SetInstances extends cats.kernel.instances.SetInstances {

  implicit val catsStdInstancesForSet: MonoidK[Set] =
    new MonoidK[Set] {

      def empty[A]: Set[A] = Set.empty[A]

      def combineK[A](x: Set[A], y: Set[A]): Set[A] = x | y

    }

  implicit def catsStdShowForSet[A:Show]: Show[Set[A]] = new Show[Set[A]] {
    def show(fa: Set[A]): String =
      fa.toIterator.map(_.show).mkString("Set(", ", ", ")")
  }
}

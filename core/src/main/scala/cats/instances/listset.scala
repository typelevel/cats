package cats
package instances

import scala.collection.immutable.ListSet

trait ListSetInstances {

  implicit def catsStdInstancesForListSet: MonoidK[ListSet] =
    new MonoidK[ListSet] {
      def empty[A]: ListSet[A] = ListSet.empty[A]
      def combineK[A](x: ListSet[A], y: ListSet[A]): ListSet[A] =
        x ++ y
    }
}

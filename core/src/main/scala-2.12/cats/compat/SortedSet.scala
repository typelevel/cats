package cats
package compat

import scala.collection.immutable

private[cats] object SortedSet {
  def zipWithIndex[A](s: immutable.SortedSet[A])(implicit A: Ordering[A]): immutable.SortedSet[(A, Int)] =
    s.zipWithIndex

}

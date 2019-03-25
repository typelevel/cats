package cats.kernel
package instances

import scala.annotation.tailrec
import scala.collection.immutable.ListSet

object listset extends ListSetInstances

class ListSetEq[A](implicit ev: Eq[A]) extends Eq[ListSet[A]] {
  def eqv(xs: ListSet[A], ys: ListSet[A]): Boolean = {
    @tailrec def loop(xs: ListSet[A], ys: ListSet[A]): Boolean =
      xs match {
        case _ if xs.isEmpty =>
          ys.isEmpty
        case _ =>
          ys match {
            case _ if ys.isEmpty =>
              false
            case _ =>
              if (ev.eqv(xs.last, ys.last)) loop(xs.init, ys.init) else false
          }
      }
    (xs eq ys) || loop(xs, ys)
  }
}


trait ListSetInstances {
  implicit def catsKernelStdMonoidForListSet[A]: Monoid[ListSet[A]] = new ListSetMonoid[A]

  implicit def catsKernelStdEqForListSet[A: Eq]: Eq[ListSet[A]] = new ListSetEq[A]

  class ListSetMonoid[A] extends Monoid[ListSet[A]] {
    def empty: ListSet[A] = ListSet.empty[A]

    def combine(x: ListSet[A], y: ListSet[A]): ListSet[A] = x ++ y

    override def combineN(x: ListSet[A], n: Int): ListSet[A] =
      StaticMethods.combineNIterable(ListSet.newBuilder[A], x, n)

    override def combineAll(xs: TraversableOnce[ListSet[A]]): ListSet[A] =
      StaticMethods.combineAllIterable(ListSet.newBuilder[A], xs)
  }
}

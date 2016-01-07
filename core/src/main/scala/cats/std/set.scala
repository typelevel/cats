package cats
package std

import cats.syntax.show._

trait SetInstances {
  implicit val setInstance: Foldable[Set] with MonoidK[Set] =
    new Foldable[Set] with MonoidK[Set] {

      def empty[A]: Set[A] = Set.empty[A]

      def combine[A](x: Set[A], y: Set[A]): Set[A] = x | y

      def foldLeft[A, B](fa: Set[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Set[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.iterator, lb)(f)

      override def exists[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Set[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Set[A]): Boolean = fa.isEmpty
    }

    implicit def setMonoid[A]: Monoid[Set[A]] = MonoidK[Set].algebra[A]

  implicit def setPartialOrder[A]: PartialOrder[Set[A]] = new SetPartialOrder[A]

  implicit def setShow[A:Show]: Show[Set[A]] = new Show[Set[A]] {
    def show(fa: Set[A]): String =
      fa.toIterator.map(_.show).mkString("Set(", ", ", ")")
  }
}

class SetPartialOrder[A] extends PartialOrder[Set[A]] {
  def partialCompare(x: Set[A], y: Set[A]): Double =
    if (x.size < y.size) if (x.subsetOf(y)) -1.0 else Double.NaN
    else if (y.size < x.size) -partialCompare(y, x)
    else if (x == y) 0.0
    else Double.NaN

  override def eqv(x: Set[A], y: Set[A]): Boolean = x == y
}

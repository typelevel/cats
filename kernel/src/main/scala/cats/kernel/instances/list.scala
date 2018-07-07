package cats.kernel
package instances

import scala.annotation.tailrec

package object list extends ListInstances

trait ListInstances extends ListInstances1 {
  implicit def catsKernelStdOrderForList[A: Order]: Order[List[A]] =
    new ListOrder[A]
  implicit def catsKernelStdMonoidForList[A]: Monoid[List[A]] =
    new ListMonoid[A]
}

trait ListInstances1 extends ListInstances2 {
  implicit def catsKernelStdPartialOrderForList[A: PartialOrder]: PartialOrder[List[A]] =
    new ListPartialOrder[A]

  implicit def catsKernelStdHashForList[A: Hash]: Hash[List[A]] =
    new ListHash[A]
}

trait ListInstances2 {
  implicit def catsKernelStdEqForList: Delay[Eq, List] =
    new Delay[Eq, List] {
      override def apply[A](fa: Eq[A]): Eq[List[A]] = new ListEq[A]()(fa)
    }
}

class ListOrder[A](implicit ev: Order[A]) extends Order[List[A]] {
  def compare(xs: List[A], ys: List[A]): Int = {
    @tailrec def loop(xs: List[A], ys: List[A]): Int =
      xs match {
        case Nil =>
          if (ys.isEmpty) 0 else -1
        case x :: xs =>
          ys match {
            case Nil => 1
            case y :: ys =>
              val n = ev.compare(x, y)
              if (n != 0) n else loop(xs, ys)
          }
      }
    if (xs eq ys) 0 else loop(xs, ys)
  }
}

class ListPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[List[A]] {
  def partialCompare(xs: List[A], ys: List[A]): Double = {
    @tailrec def loop(xs: List[A], ys: List[A]): Double =
      xs match {
        case Nil =>
          if (ys.isEmpty) 0.0 else -1.0
        case x :: xs =>
          ys match {
            case Nil => 1.0
            case y :: ys =>
              val n = ev.partialCompare(x, y)
              if (n != 0.0) n else loop(xs, ys)
          }
      }
    if (xs eq ys) 0.0 else loop(xs, ys)
  }
}

class ListHash[A](implicit ev: Hash[A]) extends ListEq[A]()(ev) with Hash[List[A]] {
  def hash(x: List[A]): Int = StaticMethods.listHash(x)(ev)
}

class ListEq[A](implicit ev: Eq[A]) extends Eq[List[A]] {
  def eqv(xs: List[A], ys: List[A]): Boolean = {
    def loop(xs: List[A], ys: List[A]): Boolean =
      xs match {
        case Nil =>
          ys.isEmpty
        case x :: xs =>
          ys match {
            case y :: ys =>
              if (ev.eqv(x, y)) loop(xs, ys) else false
            case Nil =>
              false
          }
      }
    (xs eq ys) || loop(xs, ys)
  }
}

class ListMonoid[A] extends Monoid[List[A]] {
  def empty: List[A] = Nil
  def combine(x: List[A], y: List[A]): List[A] = x ::: y

  override def combineN(x: List[A], n: Int): List[A] =
    StaticMethods.combineNIterable(List.newBuilder[A], x, n)

  override def combineAll(xs: TraversableOnce[List[A]]): List[A] =
    StaticMethods.combineAllIterable(List.newBuilder[A], xs)
}

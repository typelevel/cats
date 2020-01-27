package cats.kernel
package instances

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import compat.scalaVersionSpecific._

@suppressUnusedImportWarningForScalaVersionSpecific
trait ArraySeqInstances extends ArraySeqInstances1 {
  implicit def catsKernelStdOrderForArraySeq[A: Order]: Order[ArraySeq[A]] =
    new ArraySeqOrder[A]

  implicit def catsKernelStdMonoidForArraySeq[A: ClassTag]: Monoid[ArraySeq[A]] =
    new ArraySeqMonoid[A]
}

private[cats] trait ArraySeqInstances1 extends ArraySeqInstances2 {
  implicit def catsKernelStdPartialOrderForArraySeq[A: PartialOrder]: PartialOrder[ArraySeq[A]] =
    new ArraySeqPartialOrder[A]

  implicit def catsKernelStdHashForArraySeq[A: Hash]: Hash[ArraySeq[A]] =
    new ArraySeqHash[A]
}

private[cats] trait ArraySeqInstances2 {
  implicit def catsKernelStdEqForArraySeq[A: Eq]: Eq[ArraySeq[A]] =
    new ArraySeqEq[A]
}

final private[cats] class ArraySeqOrder[A](implicit ev: Order[A]) extends Order[ArraySeq[A]] {
  def compare(xs: ArraySeq[A], ys: ArraySeq[A]): Int = {
    @tailrec def loop(i: Int): Int =
      (i < xs.length, i < ys.length) match {
        case (true, true) =>
          val n = ev.compare(xs(i), ys(i))
          if (n != 0) n else loop(i + 1)
        case (true, false)  => 1
        case (false, true)  => -1
        case (false, false) => 0
      }

    if (xs eq ys) 0 else loop(i = 0)
  }
}

private[cats] class ArraySeqPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[ArraySeq[A]] {
  def partialCompare(xs: ArraySeq[A], ys: ArraySeq[A]): Double = {
    @tailrec def loop(i: Int): Double =
      (i < xs.length, i < ys.length) match {
        case (true, true) =>
          val n = ev.partialCompare(xs(i), ys(i))
          if (n != 0) n else loop(i + 1)
        case (true, false)  => 1
        case (false, true)  => -1
        case (false, false) => 0
      }

    if (xs eq ys) 0.0 else loop(i = 0)
  }
}

private[cats] class ArraySeqHash[A](implicit ev: Hash[A]) extends ArraySeqEq[A]()(ev) with Hash[ArraySeq[A]] {
  def hash(xs: ArraySeq[A]): Int = StaticMethods.orderedHash(xs)
}

private[cats] class ArraySeqEq[A](implicit ev: Eq[A]) extends Eq[ArraySeq[A]] {
  def eqv(xs: ArraySeq[A], ys: ArraySeq[A]): Boolean = {
    @tailrec def loop(i: Int): Boolean =
      (i < xs.length, i < ys.length) match {
        case (true, true)   => if (ev.eqv(xs(i), ys(i))) loop(i + 1) else false
        case (true, false)  => false
        case (false, true)  => false
        case (false, false) => true
      }

    (xs eq ys) || loop(i = 0)
  }
}

final private[cats] class ArraySeqMonoid[A] extends Monoid[ArraySeq[A]] {
  def empty: ArraySeq[A] =
    ArraySeq.untagged.empty

  def combine(xs: ArraySeq[A], ys: ArraySeq[A]): ArraySeq[A] =
    xs.concat(ys)

  override def combineN(x: ArraySeq[A], n: Int): ArraySeq[A] =
    StaticMethods.combineNIterable(ArraySeq.untagged.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[ArraySeq[A]]): ArraySeq[A] =
    StaticMethods.combineAllIterable(ArraySeq.untagged.newBuilder[A], xs)
}

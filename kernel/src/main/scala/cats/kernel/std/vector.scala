package cats.kernel
package std

import cats.kernel.std.util.StaticMethods

package object vector extends VectorInstances

trait VectorInstances extends VectorInstances1 {
  implicit def catsKernelStdOrderForVector[A: Order]: Order[Vector[A]] =
    new VectorOrder[A]
  implicit def catsKernelStdMonoidForVector[A]: Monoid[Vector[A]] =
    new VectorMonoid[A]
}

trait VectorInstances1 extends VectorInstances2 {
  implicit def catsKernelStdPartialOrderForVector[A: PartialOrder]: PartialOrder[Vector[A]] =
    new VectorPartialOrder[A]
}

trait VectorInstances2 {
  implicit def catsKernelStdEqForVector[A: Eq]: Eq[Vector[A]] =
    new VectorEq[A]
}

class VectorOrder[A](implicit ev: Order[A]) extends Order[Vector[A]] {
  def compare(xs: Vector[A], ys: Vector[A]): Int =
    if (xs eq ys) 0
    else StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

class VectorPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[Vector[A]] {
  def partialCompare(xs: Vector[A], ys: Vector[A]): Double =
    if (xs eq ys) 0.0
    else StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

class VectorEq[A](implicit ev: Eq[A]) extends Eq[Vector[A]] {
  def eqv(xs: Vector[A], ys: Vector[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

class VectorMonoid[A] extends Monoid[Vector[A]] {
  def empty: Vector[A] = Vector.empty
  def combine(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

  override def combineN(x: Vector[A], n: Int): Vector[A] = {
    val buf = Vector.newBuilder[A]
    var i = n
    while (i > 0) {
      buf ++= x
      i -= 1
    }
    buf.result
  }

  override def combineAll(xs: TraversableOnce[Vector[A]]): Vector[A] = {
    val buf = Vector.newBuilder[A]
    xs.foreach(buf ++= _)
    buf.result
  }
}

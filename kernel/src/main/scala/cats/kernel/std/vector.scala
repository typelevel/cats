package cats.kernel
package std

import cats.kernel.std.util.StaticMethods

package object vector extends VectorInstances

trait VectorInstances extends VectorInstances1 {
  implicit def vectorOrder[A: Order] = new VectorOrder[A]
  implicit def vectorMonoid[A] = new VectorMonoid[A]
}

trait VectorInstances1 extends VectorInstances2 {
  implicit def vectorPartialOrder[A: PartialOrder] = new VectorPartialOrder[A]
}

trait VectorInstances2 {
  implicit def vectorEq[A: Eq] = new VectorEq[A]
}

class VectorOrder[A](implicit ev: Order[A]) extends Order[Vector[A]] {
  def compare(xs: Vector[A], ys: Vector[A]): Int =
    StaticMethods.iteratorCompare(xs.iterator, ys.iterator)
}

class VectorPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[Vector[A]] {
  def partialCompare(xs: Vector[A], ys: Vector[A]): Double =
    StaticMethods.iteratorPartialCompare(xs.iterator, ys.iterator)
}

class VectorEq[A](implicit ev: Eq[A]) extends Eq[Vector[A]] {
  def eqv(xs: Vector[A], ys: Vector[A]): Boolean =
    StaticMethods.iteratorEq(xs.iterator, ys.iterator)
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

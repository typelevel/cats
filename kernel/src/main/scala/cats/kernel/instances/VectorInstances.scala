package cats.kernel
package instances
import compat.scalaVersionSpecific._

@suppressUnusedImportWarningForScalaVersionSpecific
trait VectorInstances extends VectorInstances1 {
  implicit def catsKernelStdOrderForVector[A: Order]: Order[Vector[A]] =
    new VectorOrder[A]

  implicit def catsKernelStdMonoidForVector[A]: Monoid[Vector[A]] =
    new VectorMonoid[A]
}

private[instances] trait VectorInstances1 extends VectorInstances2 {
  implicit def catsKernelStdHashForVector[A: Hash]: Hash[Vector[A]] =
    new VectorHash[A]
}

private[instances] trait VectorInstances2 extends VectorInstances3 {
  implicit def catsKernelStdPartialOrderForVector[A: PartialOrder]: PartialOrder[Vector[A]] =
    new VectorPartialOrder[A]
}

private[instances] trait VectorInstances3 {
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

class VectorHash[A](implicit ev: Hash[A]) extends VectorEq[A] with Hash[Vector[A]] {
  def hash(xs: Vector[A]): Int = StaticMethods.orderedHash(xs)
}

class VectorEq[A](implicit ev: Eq[A]) extends Eq[Vector[A]] {
  def eqv(xs: Vector[A], ys: Vector[A]): Boolean =
    if (xs eq ys) true
    else StaticMethods.iteratorEq(xs.iterator, ys.iterator)
}

class VectorMonoid[A] extends Monoid[Vector[A]] {
  def empty: Vector[A] = Vector.empty
  def combine(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

  override def combineN(x: Vector[A], n: Int): Vector[A] =
    StaticMethods.combineNIterable(Vector.newBuilder[A], x, n)

  override def combineAll(xs: IterableOnce[Vector[A]]): Vector[A] =
    StaticMethods.combineAllIterable(Vector.newBuilder[A], xs)
}

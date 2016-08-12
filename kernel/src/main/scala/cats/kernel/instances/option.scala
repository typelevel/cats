package cats.kernel
package instances

package object option extends OptionInstances

trait OptionInstances extends OptionInstances1 {
  implicit def catsKernelStdOrderForOption[A: Order]: Order[Option[A]] =
    new OptionOrder[A]
  implicit def catsKernelStdMonoidForOption[A: Semigroup]: Monoid[Option[A]] =
    new OptionMonoid[A]
}

trait OptionInstances1 extends OptionInstances0 {
  implicit def catsKernelStdPartialOrderForOption[A: PartialOrder]: PartialOrder[Option[A]] =
    new OptionPartialOrder[A]
}

trait OptionInstances0 {
  implicit def catsKernelStdEqForOption[A: Eq]: Eq[Option[A]] =
    new OptionEq[A]
}

class OptionOrder[A](implicit A: Order[A]) extends Order[Option[A]] {
  def compare(x: Option[A], y: Option[A]): Int =
    x match {
      case None =>
        if (y.isEmpty) 0 else -1
      case Some(a) =>
        y match {
          case None => 1
          case Some(b) => A.compare(a, b)
        }
    }
}

class OptionPartialOrder[A](implicit A: PartialOrder[A]) extends PartialOrder[Option[A]] {
  def partialCompare(x: Option[A], y: Option[A]): Double =
    x match {
      case None =>
        if (y.isEmpty) 0.0 else -1.0
      case Some(a) =>
        y match {
          case None => 1.0
          case Some(b) => A.partialCompare(a, b)
        }
    }
}

class OptionEq[A](implicit A: Eq[A]) extends Eq[Option[A]] {
  def eqv(x: Option[A], y: Option[A]): Boolean =
    x match {
      case None => y.isEmpty
      case Some(a) =>
        y match {
          case None => false
          case Some(b) => A.eqv(a, b)
        }
    }
}

class OptionMonoid[A](implicit A: Semigroup[A]) extends Monoid[Option[A]] {
  def empty: Option[A] = None
  def combine(x: Option[A], y: Option[A]): Option[A] =
    x match {
      case None => y
      case Some(a) =>
        y match {
          case None => x
          case Some(b) => Some(A.combine(a, b))
        }
    }
}

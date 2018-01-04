package cats

package object laws {

  type IsEq[A] = cats.kernel.laws.IsEq[A]
  val IsEq = cats.kernel.laws.IsEq

  implicit final class IsEqArrow[A](val lhs: A) extends AnyVal {
    def <->(rhs: A): IsEq[A] = IsEq(lhs, rhs)
  }
}

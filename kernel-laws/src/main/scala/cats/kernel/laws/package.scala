package cats.kernel

package object laws {

  implicit final class IsEqArrow[A](val lhs: A) extends AnyVal {
    def <->(rhs: A): IsEq[A] = IsEq(lhs, rhs)
  }
}

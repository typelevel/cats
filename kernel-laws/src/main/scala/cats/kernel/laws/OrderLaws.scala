package cats.kernel
package laws

import cats.kernel.Order

trait OrderLaws[A] extends PartialOrderLaws[A] {

  override implicit def E: Order[A]

  def totality(x: A, y: A): IsEq[Boolean] =
    (E.lteqv(x, y) || E.lteqv(y, x)) <-> true

  def compare(x: A, y: A): IsEq[Boolean] = {
    val c = E.compare(x, y)
    (((c < 0) == E.lt(x, y)) && ((c == 0) == E.eqv(x, y)) && ((c > 0) == E.gt(x, y))) <-> true
  }

  def min(x: A, y: A): IsEq[A] = {
    val c = E.compare(x, y)
    val m = E.min(x, y)
    if (c < 0) m <-> x
    else if (c == 0) x <-> y
    else m <-> y
  }

  def max(x: A, y: A): IsEq[A] = {
    val c = E.compare(x, y)
    val m = E.max(x, y)
    if (c < 0) m <-> y
    else if (c == 0) x <-> y
    else m <-> x
  }

}

object OrderLaws {
  def apply[A](implicit ev: Order[A]): OrderLaws[A] =
    new OrderLaws[A] { def E: Order[A] = ev }
}


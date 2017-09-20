package cats.kernel.laws

import cats.kernel.PartialOrder

trait PartialOrderLaws[A] extends EqLaws[A] {
  override implicit def E: PartialOrder[A]

  def reflexitivity(x: A): IsEq[Boolean] =
    E.gteqv(x, x) <-> true

  def antisymmetry(x: A, y: A): IsEq[Boolean] =
    (!(E.lteqv(x, y) && E.lteqv(y, x)) || E.eqv(x, y)) <-> true

  def transitivity(x: A, y: A, z: A): IsEq[Boolean] =
    (!(E.lteqv(x, y) && E.lteqv(y, z)) || E.lteqv(x, z)) <-> true

  def gteqv(x: A, y: A): IsEq[Boolean] =
    E.lteqv(x, y) <-> E.gteqv(y, x)

  def lt(x: A, y: A): IsEq[Boolean] =
    E.lt(x, y) <-> (E.lteqv(x, y) && E.neqv(x, y))

  def gt(x: A, y: A): IsEq[Boolean] =
    E.lt(x, y) <-> E.gt(y, x)

  def partialCompare(x: A, y: A): IsEq[Boolean] = {
    val c = E.partialCompare(x, y)
    (((c < 0) == E.lt(x, y)) && ((c == 0) == E.eqv(x, y)) && ((c > 0) == E.gt(x, y))) <-> true
  }

  def pmin(x: A, y: A): IsEq[Option[A]] = {
    val c = E.partialCompare(x, y)
    val m = E.pmin(x, y)
    if (c < 0) m <-> Option(x)
    else if (c == 0) Option(x) <-> Option(y)
    else if (c > 0) m <-> Option(y)
    else m <-> None
  }

  def pmax(x: A, y: A): IsEq[Option[A]] = {
    val c = E.partialCompare(x, y)
    val m = E.pmax(x, y)
    if (c < 0) m <-> Option(y)
    else if (c == 0) Option(x) <-> Option(y)
    else if (c > 0) m <-> Option(x)
    else m <-> None
  }


}

object PartialOrderLaws {
  def apply[A](implicit ev: PartialOrder[A]): PartialOrderLaws[A] =
    new PartialOrderLaws[A] { def E: PartialOrder[A] = ev }
}

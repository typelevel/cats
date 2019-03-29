package cats.kernel.laws

import cats.kernel.Eq

trait EqLaws[A] {

  implicit def E: Eq[A]

  def reflexivityEq(x: A): IsEq[A] =
    x <-> x

  def symmetryEq(x: A, y: A): IsEq[Boolean] =
    E.eqv(x, y) <-> E.eqv(y, x)

  def antiSymmetryEq(x: A, y: A, f: A => A): IsEq[Boolean] =
    (!E.eqv(x, y) || E.eqv(f(x), f(y))) <-> true

  def transitivityEq(x: A, y: A, z: A): IsEq[Boolean] =
    (!(E.eqv(x, y) && E.eqv(y, z)) || E.eqv(x, z)) <-> true

}

object EqLaws {
  def apply[A](implicit ev: Eq[A]): EqLaws[A] =
    new EqLaws[A] { def E: Eq[A] = ev }
}

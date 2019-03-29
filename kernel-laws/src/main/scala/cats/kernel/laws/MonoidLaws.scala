package cats.kernel.laws

import cats.kernel.{Eq, Monoid}

trait MonoidLaws[A] extends SemigroupLaws[A] {
  implicit override def S: Monoid[A]

  def leftIdentity(x: A): IsEq[A] =
    S.combine(S.empty, x) <-> x

  def rightIdentity(x: A): IsEq[A] =
    S.combine(x, S.empty) <-> x

  def repeat0(x: A): IsEq[A] =
    S.combineN(x, 0) <-> S.empty

  def collect0(x: A): IsEq[A] =
    S.combineAll(Nil) <-> S.empty

  def combineAll(xs: Vector[A]): IsEq[A] =
    S.combineAll(xs) <-> (S.empty +: xs).reduce(S.combine)

  def isId(x: A, eqv: Eq[A]): IsEq[Boolean] =
    eqv.eqv(x, S.empty) <-> S.isEmpty(x)(eqv)

}

object MonoidLaws {
  def apply[A](implicit ev: Monoid[A]): MonoidLaws[A] =
    new MonoidLaws[A] { def S: Monoid[A] = ev }
}

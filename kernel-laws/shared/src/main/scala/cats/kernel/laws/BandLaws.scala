package cats.kernel.laws

import cats.kernel.Band

trait BandLaws[A] extends SemigroupLaws[A] {
  implicit override def S: Band[A]

  def idempotence(x: A): IsEq[A] =
    S.combine(x, x) <-> x

}

object BandLaws {
  def apply[A](implicit ev: Band[A]): BandLaws[A] =
    new BandLaws[A] { def S: Band[A] = ev }
}

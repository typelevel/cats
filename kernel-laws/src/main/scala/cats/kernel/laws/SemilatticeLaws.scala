package cats.kernel.laws

import cats.kernel.Semilattice

trait SemilatticeLaws[A] extends CommutativeSemigroupLaws[A] with BandLaws[A] {
  implicit def S: Semilattice[A]
}

object SemilatticeLaws {
  def apply[A](implicit ev: Semilattice[A]): SemilatticeLaws[A] =
    new SemilatticeLaws[A] { def S: Semilattice[A] = ev }
}

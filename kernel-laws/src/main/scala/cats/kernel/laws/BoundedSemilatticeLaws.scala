package cats.kernel.laws

import cats.kernel.BoundedSemilattice

trait BoundedSemilatticeLaws[A] extends CommutativeMonoidLaws[A] with SemilatticeLaws[A] {
  override implicit def S: BoundedSemilattice[A]

}

object BoundedSemilatticeLaws {
  def apply[A](implicit ev: BoundedSemilattice[A]): BoundedSemilatticeLaws[A] =
    new BoundedSemilatticeLaws[A] { def S: BoundedSemilattice[A] = ev }
}

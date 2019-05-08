package cats.kernel.laws

import cats.kernel.CommutativeMonoid

trait CommutativeMonoidLaws[A] extends MonoidLaws[A] with CommutativeSemigroupLaws[A] {
  implicit override def S: CommutativeMonoid[A]

}

object CommutativeMonoidLaws {
  def apply[A](implicit ev: CommutativeMonoid[A]): CommutativeMonoidLaws[A] =
    new CommutativeMonoidLaws[A] { def S: CommutativeMonoid[A] = ev }
}

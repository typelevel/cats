package cats
package kernel
package laws

trait CommutativeGroupLaws[A] extends GroupLaws[A] with CommutativeMonoidLaws[A] {
  implicit override def S: CommutativeGroup[A]
}

object CommutativeGroupLaws {
  def apply[A](implicit ev: CommutativeGroup[A]): CommutativeGroupLaws[A] =
    new CommutativeGroupLaws[A] { def S: CommutativeGroup[A] = ev }
}

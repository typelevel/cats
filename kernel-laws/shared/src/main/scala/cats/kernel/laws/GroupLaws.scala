package cats.kernel
package laws

trait GroupLaws[A] extends MonoidLaws[A] {
  implicit override def S: Group[A]

  def leftInverse(x: A): IsEq[A] =
    S.empty <-> S.combine(S.inverse(x), x)

  def rightInverse(x: A): IsEq[A] =
    S.empty <-> S.combine(x, S.inverse(x))

  def consistentInverse(x: A, y: A): IsEq[A] =
    S.remove(x, y) <-> S.combine(x, S.inverse(y))
}

object GroupLaws {
  def apply[A](implicit ev: Group[A]): GroupLaws[A] =
    new GroupLaws[A] { def S: Group[A] = ev }
}

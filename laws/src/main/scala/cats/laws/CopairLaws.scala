package cats.laws

import cats.Copair

trait CopairLaws[F[_,_]] extends BitraverseLaws[F] with BifoldableLaws[F] with BifunctorLaws[F] {
  implicit override def F: Copair[F]

  def copairFoldIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab <-> F.fold[A,B,F[A,B]](fab)(F.left, F.right)

  def copairSwapIdentity[A,B](fab: F[A,B]): IsEq[F[A,B]] =
    fab <-> F.swap(F.swap(fab))

  def copairLeftAssociativeIdentity[A, B, C](a: A, fa: A => C, fb: B => C): IsEq[C] =
    F.fold(F.left(a))(fa, fb) <-> fa(a)

  def copairRightAssociativeIdentity[A, B, C](b: B, fa: A => C, fb: B => C): IsEq[C] =
    F.fold(F.right(b))(fa, fb) <-> fb(b)


}

object CopairLaws {
  def apply[F[_, _]](implicit ev: Copair[F]): CopairLaws[F] =
    new CopairLaws[F] { def F: Copair[F] = ev }
}

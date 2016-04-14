package cats.laws

import cats.Copair
import cats.syntax.copair._

trait CopairLaws[F[_,_]] extends BitraverseLaws[F] with BifoldableLaws[F] with BifunctorLaws[F] {
  implicit override def F: Copair[F]

  def copairFoldIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab <-> fab.fold(_.leftC[F, B], _.rightC[F, A])

  def copairSwapIdentity[A,B](fab: F[A,B]): IsEq[F[A,B]] =
    fab <-> fab.swap.swap

  def copairLeftAssociativeIdentity[A, B, C](a: A, fa: A => C, fb: B => C): IsEq[C] =
    a.leftC[F, B].fold(fa, fb) <-> fa(a)

  def copairRightAssociativeIdentity[A, B, C](b: B, fa: A => C, fb: B => C): IsEq[C] =
    b.rightC[F, A].fold(fa, fb) <-> fb(b)
}

object CopairLaws {
  def apply[F[_, _]](implicit ev: Copair[F]): CopairLaws[F] =
    new CopairLaws[F] { def F: Copair[F] = ev }
}

package cats.laws

import cats.Copair
import cats.syntax.copair._

trait CopairLaws[F[_,_]] extends BitraverseLaws[F] with BifoldableLaws[F] with BifunctorLaws[F] {
  implicit override def F: Copair[F]

  def copairFoldIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab <-> fab.fold(_.leftC[F, B], _.rightC[F, A])

  def copairDoubleSwapIdentity[A,B](fab: F[A,B]): IsEq[F[A,B]] =
    fab <-> fab.swap.swap

  def copairLeftIdentity[A, B, C](a: A, fa: A => C, fb: B => C): IsEq[C] =
    a.leftC[F, B].fold(fa, fb) <-> fa(a)

  def copairRightIdentity[A, B, C](b: B, fa: A => C, fb: B => C): IsEq[C] =
    b.rightC[F, A].fold(fa, fb) <-> fb(b)

  def copairToIdentity[A, B](fab: F[A,B]): IsEq[F[A,B]] =
    fab.to[F] <-> fab

  def copairLeftSwapIdentity[A, B](b: B): IsEq[F[A, B]] =
    b.leftC[F, A].swap <-> b.rightC[F, A]

  def copairRightSwapIdentity[A, B](a: A): IsEq[F[A, B]] =
    a.rightC[F, B].swap <-> a.leftC[F, B]

}

object CopairLaws {
  def apply[F[_, _]](implicit ev: Copair[F]): CopairLaws[F] =
    new CopairLaws[F] { def F: Copair[F] = ev }
}

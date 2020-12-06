package cats
package syntax

trait ApplicativeSyntax {
  implicit final def catsSyntaxApplicativeId[A](a: A): ApplicativeIdOps[A] =
    new ApplicativeIdOps[A](a)
  implicit final def catsSyntaxApplicative[F[_], A](fa: F[A]): ApplicativeOps[F, A] =
    new ApplicativeOps[F, A](fa)
}

final class ApplicativeIdOps[A](private val a: A) extends AnyVal {
  def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
}

final class ApplicativeOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def replicateA(n: Int)(implicit F: Applicative[F]): F[List[A]] = F.replicateA(n, fa)
  def replicateA_(n: Int)(implicit F: Applicative[F]): F[Unit] = F.replicateA_(n, fa)
  def unlessA(cond: Boolean)(implicit F: Applicative[F]): F[Unit] = F.unlessA(cond)(fa)
  def whenA(cond: Boolean)(implicit F: Applicative[F]): F[Unit] = F.whenA(cond)(fa)
}

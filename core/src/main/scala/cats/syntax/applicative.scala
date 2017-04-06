package cats
package syntax

trait ApplicativeSyntax {
  implicit def catsSyntaxApplicativeId[A](a: A): ApplicativeIdOps[A] = new ApplicativeIdOps[A](a)
  implicit def catsSyntaxApplicative[F[_], A](fa: F[A])(implicit F: Applicative[F]): ApplicativeOps[F, A] = new ApplicativeOps[F, A](fa)
}

final class ApplicativeIdOps[A](val a: A) extends AnyVal {
  def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
}

final class ApplicativeOps[F[_], A](fa: F[A])(implicit F: Applicative[F]) {
  def replicateA(n: Int): F[List[A]] = F.replicateA(n, fa)
  def unlessA(cond: Boolean): F[Unit] = F.unlessA(cond)(fa)
  def whenA(cond: Boolean): F[Unit] = F.whenA(cond)(fa)
}

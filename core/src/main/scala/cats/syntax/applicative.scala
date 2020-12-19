package cats
package syntax

trait ApplicativeSyntax {
  implicit final def catsSyntaxApplicativeId[A](a: A): ApplicativeIdOps[A] =
    new ApplicativeIdOps[A](a)
  implicit final def catsSyntaxApplicative[F[_], A](fa: F[A]): ApplicativeOps[F, A] =
    new ApplicativeOps[F, A](fa)
  implicit final def catsSyntaxApplicativeBoolean[F[_]](fBool: F[Boolean]): ApplicativeBooleanOps[F] =
    new ApplicativeBooleanOps(fBool)
}

final class ApplicativeIdOps[A](private val a: A) extends AnyVal {
  def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
}

final class ApplicativeOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def replicateA(n: Int)(implicit F: Applicative[F]): F[List[A]] = F.replicateA(n, fa)
  def unlessA(cond: Boolean)(implicit F: Applicative[F]): F[Unit] = F.unlessA(cond)(fa)
  def whenA(cond: Boolean)(implicit F: Applicative[F]): F[Unit] = F.whenA(cond)(fa)
}

final class ApplicativeBooleanOps[F[_]](private val fCond: F[Boolean]) extends AnyVal {
  def whenS[A](fTrue: => F[Unit])(implicit F: Applicative[F]): F[Unit] = F.whenS(fCond)(fTrue)
}

package cats
package syntax

trait ApplicativeSyntax {
  implicit def catsSyntaxApplicativeId[A](a: A): ApplicativeIdOps[A] = new ApplicativeIdOps[A](a)
  implicit def catsSyntaxApplicativeEval[A](a: Eval[A]): ApplicativeEvalOps[A] = new ApplicativeEvalOps[A](a)
  implicit def catsSyntaxApplicativeReplicateA[F[_]: Applicative, A](fa: F[A]): ReplicateAOps[F, A] = new ReplicateAOps(fa)
  implicit def catsSyntaxApplicativeUnlessA[F[_]: Applicative, A](fa: F[A]): UnlessAOps[F, A] = new UnlessAOps(fa)
  implicit def catsSyntaxApplicativeWhenA[F[_]: Applicative, A](fa: F[A]): WhenAOps[F, A] = new WhenAOps(fa)
}

final class ApplicativeIdOps[A](val a: A) extends AnyVal {
  def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
}

final class ApplicativeEvalOps[A](val a: Eval[A]) extends AnyVal {
  def pureEval[F[_]](implicit F: Applicative[F]): F[A] = F.pureEval(a)
}

final class ReplicateAOps[F[_], A](fa: F[A])(implicit F: Applicative[F]) {
  def replicateA(n: Int): F[List[A]] = F.replicateA(n, fa)
}

final class UnlessAOps[F[_], A](fa: => F[A])(implicit F: Applicative[F]) {
  def unlessA(cond: Boolean): F[Unit] = F.unlessA(cond)(fa)
}

final class WhenAOps[F[_], A](fa: => F[A])(implicit F: Applicative[F]) {
  def whenA(cond: Boolean): F[Unit] = F.whenA(cond)(fa)
}

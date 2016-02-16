package cats
package syntax

trait ApplicativeSyntax {
  implicit def applicativeIdSyntax[A](a: A): ApplicativeIdOps[A] = new ApplicativeIdOps[A](a)
  implicit def applicativeEvalSyntax[A](a: Eval[A]): ApplicativeEvalOps[A] = new ApplicativeEvalOps[A](a)
}

final class ApplicativeIdOps[A](val a: A) extends AnyVal {
  def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
}

final class ApplicativeEvalOps[A](val a: Eval[A]) extends AnyVal {
  def pureEval[F[_]](implicit F: Applicative[F]): F[A] = F.pureEval(a)
}

package cats
package syntax

trait ApplicativeSyntax {
  implicit def catsSyntaxApplicativeId[A](a: A): ApplicativeIdOps[A] = new ApplicativeIdOps[A](a)
}

final class ApplicativeIdOps[A](val a: A) extends AnyVal {
  def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
}

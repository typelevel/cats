package cats
package syntax

trait SelectiveSyntax extends Selective.ToSelectiveOps {
  implicit final def catsSyntaxSelectiveBooleanOps[F[_]](fBool: F[Boolean]): SelectiveBooleanOps[F] =
    new SelectiveBooleanOps(fBool)
}

final class SelectiveBooleanOps[F[_]](private val fBool: F[Boolean]) extends AnyVal {
  def ifS[A](ifTrue: => F[A])(ifFalse: => F[A])(implicit F: Selective[F]): F[A] = F.ifS(fBool)(ifTrue)(ifFalse)
  def whenS[A](fa: F[Unit])(implicit F: Selective[F]): F[Unit] = F.whenS(fBool)(fa)
}

package cats
package syntax

trait SelectiveSyntax extends Selective.ToSelectiveOps {
  implicit final def catsSyntaxSelectiveBooleanOps[F[_]](fBool: F[Boolean]): SelectiveBooleanOps[F] =
    new SelectiveBooleanOps(fBool)
}

final class SelectiveBooleanOps[F[_]](private val fCond: F[Boolean]) extends AnyVal {
  def whenS[A](fTrue: => F[Unit])(implicit F: Selective[F]): F[Unit] = F.whenS(fCond)(fTrue)
}
